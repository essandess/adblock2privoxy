module Normalizer where
import InputParser
import Control.Applicative hiding (many)
import Text.ParserCombinators.Parsec hiding (Line, (<|>))
import Control.Monad
import Data.List
import ParsecExt
import Data.Monoid

type Path' = (
                Sum Int,  -- bind start
                String,  -- proto
                String,  -- host
                String,  -- query
                Any   -- bind end    
              )


--cc :: String -> Either ParseError [Path']
--cc = parse (cases urlParts) "url"

urlParts :: [Parser Path']
urlParts = [bindStart', proto', host', query', bindEnd']
        where
            bindStart' = (\x -> (x, z, z, z, z)) <$> bindStart
            proto' =     (\x -> (z, x, z, z, z)) <$> proto
            host' =      (\x -> (z, z, x, z, z)) <$> host
            query' =     (\x -> (z, z, z, x, z)) <$> query
            bindEnd' =   (\x -> (z, z, z, z, x)) <$> bindEnd
            
            z :: Monoid a => a
            z = mempty 
            
            bindStart = Sum <$> option 0 (1 <$ char '|') <?> "left border"
            proto = option "" (try (many letter <* string "://")) <?> "proto"
            host = ((:) <$> (char '*' <|> hostChar) <*> many1 hostChar) <|> lookAhead separator <?> "host"
            separator = pure <$> (oneOf hostSeparators <|> queryEnd) <?> "separator"
            query = manyTill anyChar (lookAhead (try queryEnd)) <?> "query"
            queryEnd = (char '|' <* eof) <|> ('\0' <$ eof) <?> "query end"
            bindEnd = Any <$> (False <$ eof <|> True <$ char '|' <* eof) <?> "right border"

                   
data Path = Path { _bindStart :: Sum Int,
                   _proto :: String,
                   _hosts :: String,
                   _query :: String,
                   _bindEnd :: Any
                   }
              deriving (Show)

hostChar :: Parser Char
hostChar = alphaNum <|> oneOf ".-:"

hostSeparators :: String
hostSeparators = "^/*"


path :: Parser Path
path = Path <$> bindStart <*> proto <*> host <*> query <*> bindEnd
    where 
        bindStart = Sum <$> option 0 (1 <$ char '|') <?> "left border"
        
        proto = option "" (try (many letter <* string "://")) <?> "proto"
        
        --hosts = try ((:) <$> host <*> hosts) <|> lookAhead separator <?> "hosts"
        host = ((:) <$> (char '*' <|> hostChar) <*> many1 hostChar) <|> lookAhead separator <?> "host"
        separator = pure <$> (oneOf hostSeparators <|> queryEnd) <?> "separator"
        query = manyTill anyChar (lookAhead (try queryEnd)) <?> "query"
        queryEnd = ('\0' <$ char '|') <|> ('\0' <$ eof) <?> "query end"
        bindEnd = Any <$> option False (True <$ char '|') <?> "right border"
        
--path :: MyParser Path
--path = Path <$> bindStart <*> proto <*> optionMaybe hosts <*> query <*> bindEnd
--    where 
--        bindStart = option BindStartNone (char '|' *> (option BindStartStrict (BindStartSoft <$ char '|'))) <?> "left border"
--        proto = option "" (try (many letter <* string "://")) <?> "proto"
--        hosts = try ((:) <$> host <*> hosts) <|> lookAhead separator <?> "hosts"
--        host = (:) <$> (char '*' <|> hostChar) <*> many1 hostChar  <?> "host"
--        separator = pure.pure <$> (oneOf hostSeparators <|> queryEnd) <?> "separator"
--        query = manyTill anyChar (lookAhead (try queryEnd)) <?> "query"
--        queryEnd = ('\0' <$ char '|') <|> ('\0' <$ eof) <?> "query end"
--        bindEnd = option False (True <$ char '|') <?> "right border"



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

            


