module OptionsConverter (
    HeaderFilters,
    Filter (..),
    HeaderType (..),
    HeaderFilter (..),
    FilterNode (..),
    filterNodes
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
data FilterNode = Node { _pattern :: [Pattern], _filters :: HeaderFilters, _isNested :: Bool, _policy :: Policy}

--TODO: SPECIAL CASE 1 & 2

allTypes :: [HeaderType]
allTypes = [accept, contentType, requestedWith, referrer]
 
accept, contentType, requestedWith, referrer :: HeaderType
accept = HeaderType "accept" Client 1 'A' acceptFilter
contentType = HeaderType "content-type" Server 1 'C' contentTypeFilter
requestedWith = HeaderType "x-requested-with" Client 1 'X' requestedWithFilter
referrer = HeaderType "referrer" Client 2 'R' referrerFilter


filterNodes :: Policy -> [Pattern] -> RequestOptions -> [FilterNode]
filterNodes policy patterns requestOptions 
    = collectNodes patterns $ headerFilters policy requestOptions 2
    where collectNodes _ Nothing = [] 
          collectNodes patterns' (Just filters@(_: next)) 
            = Node patterns' filters (null patterns') policy : collectNodes [] (Just next)
          collectNodes patterns' (Just []) = [Node patterns' [] (null patterns') policy]


headerFilters :: Policy -> RequestOptions -> Int -> Maybe HeaderFilters
headerFilters _ _ 1 = Just []
headerFilters policy requestOptions level
    = do 
         nextLevel <- headerFilters policy requestOptions (level - 1)
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

contentTypeFilter  policy (RequestOptions (Restrictions positive negative) _ _ _ _ _ _)
    | fromMaybe False emptyPositive   = None
    | result == mempty = Any 
    | otherwise = Specific $ Filter code regex orEmpty
    where    
    negativePart = mappend ("n", "") <$> convert False negative
    positivePart = positive >>= convert True
    result@(code, regex) = mconcat $ catMaybes [positivePart, negativePart]
    
    emptyPositive = null . filter (`notElem` negative) <$> positive
    orEmpty = (policy == Unblock) && isNothing positive
   
    convert _ [] = Nothing
    convert include requestTypes = let
        contentTypes' = nub $ requestTypes >>= contentTypes
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
        |                           (hasContentTypes     negative) 
          && (fromMaybe True $ not . hasContentTypes <$> positive)   = Just True
        | otherwise                                                  = Nothing
    
    hasContentTypes  = not . all null . fmap contentTypes


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
    orEmpty = (policy == Unblock) && (isNothing positive || (not $ fromMaybe True thirdParty))
     
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
                                        
contentTypes :: RequestType -> [String]
contentTypes Script = ["/(?:x-)?javascript"]
contentTypes Image = ["image/"]
contentTypes Stilesheet = ["/css"]
contentTypes Object = ["video/","audio/","/(?:x-)?shockwave-flash"]
contentTypes ObjectSubrequest = ["video/","audio/","/octet-stream"]
contentTypes Subdocument = ["/html"]
contentTypes _ = []                   