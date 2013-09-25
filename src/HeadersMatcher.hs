module HeadersMatcher where
import InputParser
import Control.Applicative
import Control.Monad
import Data.List
import Data.Maybe


data HeaderType = HeaderType {_name :: String, onResponse :: Bool, level :: Int}
data Header = Header {_type::HeaderType,  _code :: String, _regex :: String, _orEmpty :: Bool }

accept, contentType, requestedWith, referrer :: HeaderType
accept = HeaderType "accept" False 1
contentType = HeaderType "content-type" True 1
requestedWith = HeaderType "x-requested-with" False 1
referrer = HeaderType "referrer" False 2


 
makeAccept, makeContentType, makeRequestedWith, makeReferrer :: Bool -> RequestOptions -> Maybe Header

makeContentType  excludePattern (RequestOptions (Restrictions positive negative) _ _ _ _ _ _)
    | specified = Just $ Header contentType code regex orEmpty
    | otherwise = Nothing
    where
    negativeContentTypes = nub.join $ fromRequestType <$> negative
    negativeCode = 'n' : typeCodes negativeContentTypes
    negativeRegex = ""
    typeCodes types = sort $ head <$> types
    
    
    
    
    
    
    
    positiveContentTypes = case positive of
                              Nothing -> []
                              Just positive' -> nub.join $ fromRequestType <$> positive'
    
                              
    contentTypes = case positive of
                        Nothing -> nub.join $ fromRequestType <$> negative
                        Just positive' -> let positiveTypes = nub.join $ fromRequestType <$> positive'
                                              negativeTypes = nub.join $ fromRequestType <$> negative
                                          in [t | t <- positiveTypes,t `notElem` negativeTypes]
    include = isJust positive
    regex = ""
    code = ""
    specified = include || (not.null $ negative)
    orEmpty = excludePattern && not include
    fromRequestType :: RequestType -> [String]
    fromRequestType Script = ["javascript"]
    fromRequestType Image = ["image\\/"]
    fromRequestType Stilesheet = ["\\/css"]
    fromRequestType Object = ["video\\/","audio\\/","\\/shockwave-flash"]
    fromRequestType ObjectSubrequest = ["video\\/","audio\\/","\\/octet-stream"]
    fromRequestType Subdocument = ["\\/html"]
    fromRequestType _ = []
    
 
makeAccept excludePattern options = case makeContentType excludePattern options of
                                        Nothing -> Nothing
                                        Just res -> Just res {_type = accept, _orEmpty = False}
                             
                                        
makeRequestedWith excludePattern options@(RequestOptions (Restrictions positive negative) _ _ _ _ _ _) =
        case res2 of
            Nothing    -> Nothing
           -- Just True  -> Just $ Header requestedWith [] True  False
           -- Just False -> Just $ Header requestedWith [] False True
    where
    res1 :: Maybe Bool 
    res1 = Just False --when (Xmlhttprequest `elem` negative) $ False -- then Just False else Nothing

    res2 = if isJust res1 then res1 else 
                case positive of
                Nothing -> Nothing
                Just positive' ->  if Xmlhttprequest `elem` positive' then Just True else Nothing
    
    --res3 = if isJust res2 then res2 else
    --            case makeContentType excludePattern options of
    --            Nothing -> Nothing
    --            Just res -> if not._include $ res then Just True else Nothing


makeReferrer excludePattern (RequestOptions requestTypes thirdParty (Restrictions positive negative) _ _ _ _) = undefined
    where
        --нельзя представить значения в виде одного списка. Видимо, надо в виде строки все делать... строка = лукахеды все                                   
                                        
                                        
                                        
                                        
                                        
                                        
                                        
                                        
                                        
                                        
                                        
                                        
                                        
                                        
                                        
    
    