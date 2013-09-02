module Normalizer where
import InputParser
import Control.Applicative hiding (many)
import Text.ParserCombinators.Parsec hiding (Line, (<|>))
import Control.Monad.State
import Data.List
import Data.String.Utils (replace)
import Data.Maybe
import Data.List.Utils (split)
import ParsecExt
import Utils

type Path' = (
                String,  -- proto
                String,  -- host
                String  -- query   
              )


url :: String -> Either ParseError [Path]
url = parse (makePaths <$> bindStart <*> cases urlParts <*> bindEnd) "url"
    where
        bindStart = try (string "||") <|> try (string "|") <|> return "" <?> "query start"
        bindEnd = (char '|' <* eof) <|> ('\0' <$ eof) <?> "query end"
        makePath start (proto, host, query) end = Path (length start) proto host query end 
        makePaths start mid end = makePath start <$> mid <*> (pure (end == '|')) 

urlParts :: [StringStateParser Path']
urlParts = square3 proto (manyCases host) (oneCase query)
        where          
            append xs x = xs ++ [x]
            proto :: StringStateParser String
            proto = do
                    masksString <- get
                    case masksString of
                        Nothing -> 
                            do
                            put $ Just $ intercalate protocolsSeparator protocols
                            return ""
                        Just masksString' -> 
                            do
                            let masks = split protocolsSeparator masksString'
                            if null masks 
                                then lift pzero
                                else 
                                    do
                                    name <- lift $ many $ protocolChar
                                    sep <- lift $ many $ oneOf $ hostSeparators
                                    let chars = name ++ (replace "^" "//" sep)
                                    nextChar <- lift $ lookAhead anyChar
                                    let masks' = filterProtoMasks masks chars nextChar
                                    if null masks' || null chars
                                        then lift pzero
                                        else
                                            do
                                            if isJust $ find null masks' 
                                                then put $ Just $ ""
                                                else put $ Just $ intercalate protocolsSeparator masks'  
                                            if nextChar == '*' 
                                                then return $ chars ++ ['*']
                                                else return chars
            host = try (append <$> many hostChar <*> char '*') <|>
                   try (append <$> many1 hostChar <*> lookAhead (separator)) <?> "host"
            separator = (oneOf hostSeparators <|> queryEnd) <?> "separator"
            query = notFollowedBy (try $ string "//") *> manyTill anyChar (lookAhead (try queryEnd)) <?> "query"
            queryEnd = (char '|' <* eof) <|> ('\0' <$ eof) <?> "query end"
            
filterProtoMasks :: [String] -> String -> Char -> [String]
filterProtoMasks masks chars nextChar = catMaybes $ map filterProtoMask masks
    where filterProtoMask mask = if nextChar /= '*' 
                                    then if isSuffixOf chars mask
                                         then Just ""
                                         else Nothing 
                                    else let tailFound = find (chars `isPrefixOf`) (tails mask)
                                         in drop (length chars) <$> tailFound 

                   
data Path = Path { _bindStart :: Int,
                   _proto :: String,
                   _hosts :: String,
                   _query :: String,
                   _bindEnd :: Bool
                   }
              deriving (Show)

hostChar :: Parser Char
hostChar = alphaNum <|> oneOf ".-:"

protocols :: [String]
protocols = ["https://", "http://"]

protocolsSeparator :: String
protocolsSeparator = ";"

protocolChar :: Parser Char
protocolChar = oneOf (delete '/' $ nub $ join $ protocols)

hostSeparators :: String
hostSeparators = "^/"

normalizeLines :: [Line] -> [Line]
normalizeLines = join.fmap normalizeLine

normalizeLine :: Line -> [Line]
normalizeLine (Line text (ElementHide restr excl pattern)) = 
        [(Line text (ElementHide (normalizeRestrictions restr) excl pattern))]
normalizeLine (Line text (RequestBlock excl pattern 
       (RequestOptions restrRt tp restrDom mc coll dnt u))) = newLine <$> (normalizeBlockPattern pattern)
        where newLine pattern' =
                (Line text (RequestBlock excl pattern' 
                 (RequestOptions (normalizeRestrictions restrRt) tp (normalizeRestrictions restrDom) mc coll dnt u))) 
normalizeLine a = [a]

normalizeBlockPattern :: Pattern -> [Pattern]
normalizeBlockPattern p = undefined

normalizeRestrictions :: (Eq a) => Restrictions a -> Restrictions a
normalizeRestrictions = annigilate.allowAll.deduplicate
    where 
        deduplicate (Restrictions (Just p) n) = Restrictions (Just $ nub p) (nub n)
        deduplicate a = a
        allowAll (Restrictions (Just []) n@(_:_)) = Restrictions Nothing n
        allowAll a = a
        annigilate (Restrictions (Just p) n) = 
                            let notN x = not (x `elem` n)
                            in Restrictions (Just $ filter notN p) n
        annigilate a = a

            


