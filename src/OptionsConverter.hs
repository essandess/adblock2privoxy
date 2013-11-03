module OptionsConverter (
    HeaderFilters,
    Filter (..),
    HeaderType (..),
    HeaderFilter (..),
    headerFilters
) where
import InputParser
import Control.Applicative
import Control.Monad
import Data.List
import Data.Monoid hiding (Any)
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
allTypes = [accept, contentType, requestedWith, referrer]
 
accept, contentType, requestedWith, referrer :: HeaderType
accept = HeaderType "accept" Client 1 'A' acceptFilter
contentType = HeaderType "content-type" Server 1 'C' contentTypeFilter
requestedWith = HeaderType "x-requested-with" Client 1 'X' requestedWithFilter
referrer = HeaderType "referrer" Client 2 'R' referrerFilter


headerFilters :: Policy -> Int -> RequestOptions -> Maybe HeaderFilters
headerFilters _ 0 _ = Just []
headerFilters policy level requestOptions
    = do 
         nextLevel <- headerFilters policy (level - 1) requestOptions
         let filters = do
                       headerType <- allTypes
                       guard (_level headerType == level)
                       case (_fabrique headerType) policy requestOptions of
                          Specific filter' -> return $ Just $ HeaderFilter headerType filter'
                          None -> return Nothing
                          Any -> mzero
         when (all isNothing filters && not (null filters)) $ fail "filters blocked"
         return $ case catMaybes filters of
                    []       -> nextLevel
                    filters' -> filters' : nextLevel
 
acceptFilter, contentTypeFilter, requestedWithFilter, referrerFilter :: FilterFabrique

contentTypeFilter  policy (RequestOptions (Restrictions positive negative) thirdParty _ _ _ _ _)
    | fromMaybe False emptyPositive   = None
    | result == mempty = Any 
    | otherwise = Specific $ Filter code regex orEmpty
    where 
    negative' 
        | fromMaybe False thirdParty = Document : negative
        | otherwise                  = negative   
    negativePart = mappend ("n", "") <$> convert False negative'
    positivePart = positive >>= convert True
    result@(code, regex) = mconcat $ catMaybes [positivePart, negativePart]
    orEmpty = (policy == Unblock) && isNothing positive
    emptyPositive = null . filter (`notElem` (fromMaybe "" $ fst <$> negativePart)) . fst <$> positivePart
   
    convert _ [] = Nothing
    convert include requestTypes = let
        contentTypes' = nub $ requestTypes >>= contentTypes include
        code' = sort $ (head . dropWhile (`elem` "/(?:x-)")) <$> contentTypes'
        regex' = lookahead contentTypes' ".*" include
        in Just (code', regex')
    
acceptFilter excludePattern options = case contentTypeFilter excludePattern options of
                                            Specific res -> Specific res {_orEmpty = False}
                                            other      -> other
                             
                                        
requestedWithFilter _ (RequestOptions (Restrictions positive negative) _ _ _ _ _ _) =
        case result of
            Nothing       -> Any
            Just result'  -> Specific $ Filter "" (lookahead ["XMLHttpRequest"] "\\s*" result')  (not result')
    where
    result
        | Xmlhttprequest `elem` negative                             = Just False
        | Xmlhttprequest `elem` fromMaybe [] positive                = Just True
        |                           (hasContentTypes False    negative) 
          && (fromMaybe True $ not . hasContentTypes True <$> positive)   = Just True
        | otherwise                                                  = Nothing
    
    
    hasContentTypes include = not . all null . fmap (contentTypes include)


referrerFilter policy (RequestOptions _ thirdParty (Restrictions positive negative) _ _ _ _) 
    | fromMaybe False emptyPositive  = None
    | result == mempty = Any 
    | otherwise = Specific $ Filter code regex orEmpty
    where
    negativePart = mappend ("n", "") <$> convert False negative
    positivePart = positive >>= convert True
    thirdPartyPart tp = (if tp then "t" else "nt", lookahead ["$host"] ".*\\/" (not tp))
    result@(code, regex) = mconcat $ catMaybes [positivePart, negativePart, thirdPartyPart <$> thirdParty]
    
    emptyPositive = null . filter (`notElem` negative) <$> positive
    orEmpty =  (policy == Unblock) && (isNothing positive || (not $ fromMaybe True thirdParty))
     
    convert _ [] = Nothing
    convert include domains = let
        code' = intercalate "][" $ sort domains
        regex' = lookahead domains ".*[./]" include
        in Just ("[" ++ code' ++ "]", regex')
           
    
lookahead :: [String] -> String -> Bool -> String    
lookahead list prefix include = join ["(?", (if include then "=" else "!"), 
                  ":", prefix ,"(?:", intercalate "|" $ excapeRx <$> list, "))"]
                  where
                  excapeRx = replace "/" "\\/" . replace "." "\\."                          
                                        
contentTypes :: Bool -> RequestType -> [String]
contentTypes _ Script = ["/(?:x-)?javascript"]
contentTypes _ Image = ["image/"]
contentTypes _ Stilesheet = ["/css"]
contentTypes _ Object = ["video/","audio/","/(?:x-)?shockwave-flash"]
contentTypes _ ObjectSubrequest = ["video/","audio/","/octet-stream"]
contentTypes _ Document = ["/html", "/xml"]
contentTypes False Subdocument = ["/html", "/xml"]
contentTypes _ _ = []  
                  