{-# LANGUAGE StrictData #-}

module OptionsConverter (
    HeaderFilters,
    Filter (..),
    HeaderType (..),
    HeaderFilter (..),
    headerFilters
) where
import InputParser
import Control.Monad
import Data.Containers.ListUtils
import Data.List
import Data.Maybe
import Data.String.Utils (replace)
import {-# SOURCE #-}  UrlBlocker

type FilterFabrique = Policy -> RequestOptions -> HeaderPolicy
data HeaderType = HeaderType {_name :: String, _taggerType :: TaggerType, _level :: Int,
                              _typeCode :: Char, _fabrique :: FilterFabrique}
data Filter = Filter { _code :: String, _regex :: String, _orEmpty :: Bool } deriving Eq
data HeaderPolicy = Specific Filter | Any | None deriving Eq
data HeaderFilter = HeaderFilter HeaderType Filter
type HeaderFilters = [[HeaderFilter]]

allTypes :: [HeaderType]
allTypes = [accept, contentType, requestedWith, referer]

accept, contentType, requestedWith, referer :: HeaderType
accept = HeaderType "accept" Client 1 'A' acceptFilter
contentType = HeaderType "content-type" Server 1 'C' contentTypeFilter
requestedWith = HeaderType "x-requested-with" Client 1 'X' requestedWithFilter
referer = HeaderType "referer" Client 2 'R' refererFilter


headerFilters :: Policy -> Int -> RequestOptions -> Maybe HeaderFilters
headerFilters _ 0 _ = Just []
headerFilters policy level requestOptions@RequestOptions{_requestType = requestType}
    = let requestOptions' = requestOptions{_requestType = convertPopup $ convertOther requestType}
      in do
         nextLevel <- headerFilters policy (level - 1) requestOptions'
         let
            passthrough = checkPassthrough requestOptions'
            filters = do
                       headerType <- allTypes
                       guard (_level headerType == level)
                       case _fabrique headerType policy requestOptions' of
                          Specific filter' -> return $ Just $ HeaderFilter headerType filter'
                          None -> return Nothing
                          Any -> mzero
         when (not passthrough && all isNothing filters && not (null filters)) $ fail "filters blocked"
         return $ case catMaybes filters of
                    []       -> nextLevel
                    filters' -> filters' : nextLevel

convertPopup :: Restrictions RequestType -> Restrictions RequestType
convertPopup (Restrictions positive negative)= Restrictions positive' negative
    where
    positiveContentTypes = fromMaybe [] positive >>= contentTypes True
    positive' | Popup `elem` negative && null positiveContentTypes = Nothing
              | otherwise                                          = positive

convertOther :: Restrictions RequestType -> Restrictions RequestType
convertOther (Restrictions positive negative)= Restrictions positive' negative'
    where
    allContentOptions = [Script, Image, Stylesheet, Object, ObjectSubrequest, Document]
    positiveList = fromMaybe [] positive
    negative' | Other `elem` positiveList = allContentOptions \\ positiveList
              | otherwise                 = negative
    positive' | Other `elem` negative     = Just $ allContentOptions \\ negative'
              | positive == Just [Other]  = Nothing
              | otherwise                 = positive

checkPassthrough :: RequestOptions -> Bool
checkPassthrough RequestOptions {_requestType = (Restrictions positive _) }
    = maybe False (not . null . intersect [Subdocument, Popup]) positive

acceptFilter, contentTypeFilter, requestedWithFilter, refererFilter :: FilterFabrique

contentTypeFilter  policy (RequestOptions (Restrictions positive negative) thirdParty _ _ _ _ _ _)
    | fromMaybe True emptyPositive && isJust positive = None
    | result == mempty = Any
    | otherwise = Specific $ Filter code regex orEmpty
    where
    negative' | isNothing positive && fromMaybe False thirdParty = Document : negative
              | otherwise                  = negative
    negativePart = mappend ("n", "") <$> convert False negative'
    positivePart = positive >>= convert True
    result@(code, regex) = mconcat $ catMaybes [positivePart, negativePart]
    orEmpty = (policy == Unblock) && isNothing positive
    emptyPositive = not . any (`notElem` maybe "" fst negativePart) . fst <$> positivePart

    convert  _      []                        = Nothing
    convert include requestTypes | null code' = Nothing
                                 | otherwise  = Just (code', regex')
        where   contentTypes' = nubOrd $ requestTypes >>= contentTypes include
                code' = sort $ head . dropWhile (`elem` "/(?:x-)") <$> contentTypes'
                regex' = lookahead contentTypes' "[\\s\\w]*" include

acceptFilter excludePattern options = case contentTypeFilter excludePattern options of
                                            Specific res -> Specific res {_orEmpty = False}
                                            other      -> other


requestedWithFilter _ RequestOptions{ _requestType = Restrictions positive negative } =
        case result of
            Nothing       -> Any
            Just result'  -> Specific $ Filter (code result') (lookahead ["xmlhttprequest"] "\\s*" result')  (not result')
    where
    code True = "x"
    code False = "nx"
    result | Xmlhttprequest `elem` negative                                  = Just False
           | Xmlhttprequest `elem` fromMaybe [] positive                     = Just True
           | hasContentTypes False negative
             && maybe True (not . hasContentTypes True) positive = Just True
           | otherwise                                                       = Nothing
    hasContentTypes include = not . all (null . contentTypes include)


refererFilter policy RequestOptions{ _thirdParty = thirdParty, _domain = Restrictions positive negative }
    | fromMaybe False emptyPositive  = None
    | result == mempty = Any
    | otherwise = Specific $ Filter code regex orEmpty
    where
    negativePart = mappend ("n", "") <$> convert False negative
    positivePart = positive >>= convert True
    thirdPartyPart tp = (if tp then "t" else "nt",
                         concat ["(?", lookAheadPolicy $ not tp,
                                 ":\\s*(?:https?:\\/\\/)?(?:[\\w.-]*\\.)?([\\w-]+\\.[\\w-]+)[^\\w.-].*\\1$)",
                                 "\ns@^referer:.*@$&\\t$host@Di"])
    result@(code, regex) = mconcat $ catMaybes [positivePart, negativePart, thirdPartyPart <$> thirdParty]
    emptyPositive = not . any (`notElem` negative) <$> positive
    orEmpty =  (policy == Unblock) && (isNothing positive || not (fromMaybe True thirdParty))
    convert _ [] = Nothing
    convert include domains = let
        code' = intercalate "][" $ sort domains
        regex' = lookahead domains "[^\\n]*[./]" include
        in Just ("[" ++ code' ++ "]", regex')

lookAheadPolicy :: Bool -> String
lookAheadPolicy True = "="
lookAheadPolicy False = "!"

lookahead :: [String] -> String -> Bool -> String
lookahead list prefix include = join ["(?", lookAheadPolicy include,
                  ":", prefix ,"(?:", intercalate "|" $ excapeRx <$> list, "))"]
                  where
                  excapeRx = replace "/" "\\/" . replace "." "\\."

contentTypes :: Bool -> RequestType -> [String]
contentTypes _ Script = ["/(?:x-)?javascript"]
contentTypes _ Image = ["image/"]
contentTypes _ Stylesheet = ["/css"]
contentTypes _ Object = ["video/","audio/","/(?:x-)?shockwave-flash"]
contentTypes _ ObjectSubrequest = ["video/","audio/","/octet-stream"]
contentTypes _ Document = ["/html", "/xml"]
contentTypes False Subdocument = ["/html", "/xml"]
contentTypes _ _ = []
