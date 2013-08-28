module InputParser where
import Control.Applicative hiding ((<|>))
import Text.ParserCombinators.Parsec hiding (Line, many, optional)
import Data.List.Utils (split)
import Data.List
import Data.Char
import Data.Monoid
import Control.Monad
import Text.Parsec.Permutation
import ParsecExt

--------------------------------------------------------------------------
---------------------------- data model  ------------------------------------
--------------------------------------------------------------------------

-- composite
data Line = Line String Record
        deriving (Read,Show,Eq)
        
data Record =   Unknown |
                Comment | 
                ElementHide (Restrictions Domain) Exclude Pattern | 
                RequestBlock Exclude Pattern RequestOptions
        deriving (Read,Show,Eq)
                   
data RequestType =  Script | Image | Stilesheet | Object | Xmlhttprequest | Popup |
                    ObjectSubrequest | Subdocument | Document | Elemhide | Other
                    deriving (Read, Show,Eq)

data RequestOptions = RequestOptions 
                            (Restrictions RequestType) 
                            (Maybe ThirdParty) 
                            (Restrictions Domain) 
                            MatchCase
                            (Maybe Collapse)
                            DoNotTrack
                            [String]
        deriving (Read,Show,Eq)

-- primitive
type ThirdParty = Bool
type Exclude = Bool
type Collapse = Bool
type MatchCase = Bool
type DoNotTrack = Bool
type Pattern = String
type Domain = String

-- helpers
data Restrictions a = Restrictions {
                          positive :: Maybe [a],
                          negative :: [a]}
        deriving (Read,Show,Eq)

--------------------------------------------------------------------------
---------------------------- parsers  ------------------------------------
--------------------------------------------------------------------------

adblockFile :: MyParser [Line]        
adblockFile = header *> sepEndBy line (oneOf eol)
    where 
        header = string "[Adblock Plus " <* version <* string "]"  <* lineEnd
        version = join <$> sepBy (many1 digit) (char '.')


line :: MyParser Line 
line = Line <$> text <*> choice (try <$> [comment, elementHide, match, unknown]) <?> "filtering rule"  
    where
        text = lookAhead (manyTill anyChar lineEnd)

elementHide :: MyParser Record
elementHide = ElementHide <$> domains ',' <*> excludeMatch <*> pattern
    where
        excludeMatch = char '#' *> ((False <$ string "#") <|> (True <$ string "@#"))
        pattern = manyTill anyChar (lookAhead lineEnd)

match :: MyParser Record
match = RequestBlock <$> excludeMatch <*> pattern <*> options
    where
        excludeMatch = option False $ True <$ count 2 (char '@')
        patternEnd = try (return () <* char '$' <* requestOptions <* lineEnd) <|> try (return () <* lineEnd)
        pattern = manyTill anyChar (lookAhead patternEnd)
        options = option '$' (char '$') *> requestOptions

comment :: MyParser Record
comment = Comment <$ (separatorLine <|> commentText)
            where commentText = char '!' <* skipMany notLineEnd
                  separatorLine = lookAhead lineEnd

unknown :: MyParser Record
unknown = Unknown <$ skipMany notLineEnd

requestOptions :: MyParser RequestOptions
requestOptions = runPermParser $ RequestOptions 
                                    <$> requestTypes 
                                    <*> (getMaybeAll <$> requestOptionNorm "ThirdParty") 
                                    <*> optionalDomain
                                    <*> (getAllOrFalse <$> requestOptionNorm  "MatchCase")
                                    <*> (getMaybeAll <$> requestOptionNorm "Collapse")
                                    <*> (getAllOrFalse <$> requestOptionNorm "Donottrack")
                                    <* manyPerm separator 
                                    <*> unknownOption
    where 
        optionalDomain = optionPerm noRestrictions $ try domainOption
        requestTypes = Restrictions <$> (Just <$> manyPerm  (try requestTypeOption)) <*> manyPerm (try notRequestTypeOption)
        notRequestTypeOption = char '~' *> requestTypeOption
        requestOptionNorm = manyPerm.try.requestOption
        separator = try (lineSpaces *> char ',' <* lineSpaces)
        unknownOption = manyPerm $ try optionName
        
requestOption :: String -> MyParser All
requestOption name = All <$> option True (char '~' *> return False) <* checkOptionName name
                             


requestTypeOption :: MyParser RequestType
requestTypeOption =  do  t <- optionName 
                         case reads t of
                            [(result, "")] -> return result
                            _ -> pzero <?> "request type"    

      
                    
domainOption :: MyParser (Restrictions Domain)
domainOption =  checkOptionName "Domain" *> lineSpaces *> char '=' *> lineSpaces *> domains '|'

optionName :: MyParser String
optionName = asOptionName <$> ((:) <$> letter <*> many (alphaNum <|> char '-'))
                where
                     capitalize [] = ""
                     capitalize (x:xs) = toUpper x:(toLower<$>xs)
                     ws = split "-"
                     asOptionName = join.liftA capitalize.ws

checkOptionName :: String -> MyParser ()
checkOptionName name =  do t <- optionName
                           when (name /= t) (pzero <?> "option type")
                    
domain :: MyParser Domain
domain = join <$> intersperse "." <$> parts
            where 
            parts = sepBy1 domainPart (char '.') 
            domainPart = many1 (alphaNum <|> char '-')

domains :: Char -> MyParser (Restrictions Domain)
domains sep = runPermParser restrictions
    where 
        restrictions = Restrictions <$> (Just <$> manyPerm  (try domain)) <*> manyPerm  (try notDomain) <* manyPerm (try separator)
        separator = lineSpaces *> char sep <* lineSpaces
        notDomain = char '~' *> domain
                                        
--helpers
eol :: String
eol = "\r\n"

lineSpaces :: MyParser ()
--lineSpaces = spaces
lineSpaces = skipMany (satisfy isLineSpace) <?> "white space"
    where isLineSpace c = c == ' ' || c == '\t'

lineEnd :: MyParser Char
lineEnd = oneOf eol <|> ('\0' <$ eof)

notLineEnd :: MyParser Char
notLineEnd = noneOf eol


getMaybeAll :: [All] -> Maybe Bool
getMaybeAll [] = Nothing
getMaybeAll list = Just$getAll$mconcat list

getAllOrFalse :: [All] -> Bool
getAllOrFalse [] = False
getAllOrFalse list = getAll$mconcat list

noRestrictions :: Restrictions a
noRestrictions = Restrictions Nothing []
