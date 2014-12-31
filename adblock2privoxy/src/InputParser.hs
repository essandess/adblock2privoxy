module InputParser ( 
Line (..),
Restrictions (..),
RequestOptions (..),
Record (..),
RequestType (..),
Pattern,
Domain,
Policy (..),
RecordSource (..),
adblockFile,
recordSourceText
)
where
import Control.Applicative hiding ((<|>))
import Text.ParserCombinators.Parsec hiding (Line, many, optional)
import Data.List.Utils (split)
import Data.List
import Data.Char
import Data.Monoid
import Control.Monad
import Text.Parsec.Permutation
import System.FilePath
 
--------------------------------------------------------------------------
---------------------------- data model  ---------------------------------
--------------------------------------------------------------------------

-- composite
data Line = Line RecordSource Record
        deriving (Show,Eq)

data RecordSource = RecordSource { _position :: SourcePos, _rawRecord :: String } deriving (Show,Eq)  
data Policy = Block | Unblock deriving (Show, Eq, Read, Ord)        
data Record =   Error String |
                Comment String | 
                ElementHide (Restrictions Domain) Policy Pattern | 
                RequestBlock Policy Pattern RequestOptions
        deriving (Read,Show,Eq)
                   
data RequestType =  Script | Image | Stylesheet | Object | Xmlhttprequest | Popup |
                    ObjectSubrequest | Subdocument | Document | Other
                    deriving (Read, Show,Eq)

data RequestOptions = RequestOptions {
                            _requestType :: Restrictions RequestType, 
                            _thirdParty  :: Maybe Bool, 
                            _domain      :: Restrictions Domain, 
                            _matchCase   :: Bool,
                            _collapse    :: Maybe Bool,
                            _doNotTrack  :: Bool,
                            _elemHide    :: Bool,
                            _unknown     :: [String]
                      }
        deriving (Read,Show,Eq)

-- primitive
type Pattern = String
type Domain = String

-- helpers
data Restrictions a = Restrictions {
                          _positive :: Maybe [a],
                          _negative :: [a]}
        deriving (Read,Show,Eq)

recordSourceText :: RecordSource -> String
recordSourceText (RecordSource position rawRecord)
   = concat [rawRecord, " (", takeFileName $ sourceName position, ": ", show $ sourceLine position, ")"]
   
--------------------------------------------------------------------------
---------------------------- parsers  ------------------------------------
--------------------------------------------------------------------------

adblockFile :: Parser [Line]        
adblockFile = header *> sepEndBy line (oneOf eol)
    where 
        header = string "[Adblock Plus " <* version <* string "]"  <* lineEnd
        version = join <$> sepBy (many1 digit) (char '.')


line :: Parser Line 
line = do
    position <- getPosition 
    let text = lookAhead (manyTill anyChar lineEnd)
        sourcePosition = RecordSource position <$> text
    Line <$> sourcePosition <*> choice (try <$> [comment, elementHide, match, unknown]) <?> "filtering rule"  
    
        

elementHide :: Parser Record
elementHide = ElementHide <$> domains ',' <*> excludeMatch <*> pattern
    where
        excludeMatch = char '#' *> ((Block <$ string "#") <|> (Unblock <$ string "@#"))
        pattern = manyTill anyChar (lookAhead lineEnd)

match :: Parser Record
match = RequestBlock <$> excludeMatch <*> pattern <*> options
    where
        excludeMatch = option Block $ Unblock <$ count 2 (char '@')
        patternEnd = try (return () <* char '$' <* requestOptions <* lineEnd) <|> try (return () <* lineEnd)
        pattern = manyTill (noneOf "#") (lookAhead patternEnd)
        options = option '$' (char '$') *> requestOptions

comment :: Parser Record
comment = Comment <$> (separatorLine <|> commentText)
            where commentText = char '!' *> many notLineEnd
                  separatorLine = lookAhead lineEnd *> return ""

unknown :: Parser Record
unknown = Error "Record type detection failed" <$ skipMany notLineEnd

requestOptions :: Parser RequestOptions
requestOptions = runPermParser $ RequestOptions 
                                    <$> (fixRestrictions <$> requestTypes) 
                                    <*> (getMaybeAll <$> requestOptionNorm "ThirdParty") 
                                    <*> (fixRestrictions <$> optionalDomain)
                                    <*> (getAllOrFalse <$> requestOptionNorm  "MatchCase")
                                    <*> (getMaybeAll <$> requestOptionNorm "Collapse")
                                    <*> (getAllOrFalse <$> requestOptionNorm "Donottrack")
                                    <*> (getAllOrFalse <$> requestOptionNorm "Elemhide")
                                    <* manyPerm separator 
                                    <*> unknownOption
    where 
        optionalDomain = optionPerm noRestrictions $ try domainOption
        requestTypes = Restrictions <$> (Just <$> manyPerm  (try requestTypeOption)) <*> manyPerm (try notRequestTypeOption)
        notRequestTypeOption = char '~' *> requestTypeOption
        requestOptionNorm = manyPerm.try.requestOption
        separator = try (lineSpaces *> char ',' <* lineSpaces)
        unknownOption = manyPerm $ try optionName
        
requestOption :: String -> Parser All
requestOption name = All <$> option True (char '~' *> return False) <* checkOptionName name
                             


requestTypeOption :: Parser RequestType
requestTypeOption =  do  t <- optionName 
                         case reads t of
                            [(result, "")] -> return result
                            _ -> pzero <?> "request type"    

      
                    
domainOption :: Parser (Restrictions Domain)
domainOption =  checkOptionName "Domain" *> lineSpaces *> char '=' *> lineSpaces *> domains '|'

optionName :: Parser String
optionName = asOptionName <$> ((:) <$> letter <*> many (alphaNum <|> char '-'))
                where
                     capitalize [] = ""
                     capitalize (x:xs) = toUpper x:(toLower<$>xs)
                     ws = split "-"
                     asOptionName = join.liftA capitalize.ws

checkOptionName :: String -> Parser ()
checkOptionName name =  do t <- optionName
                           when (name /= t) (pzero <?> "option type")
                    
domain :: Parser Domain
domain = join <$> intersperse "." <$> parts
            where 
            parts = sepBy1 domainPart (char '.') 
            domainPart = many1 (alphaNum <|> char '-')

domains :: Char -> Parser (Restrictions Domain)
domains sep = fixRestrictions <$> runPermParser restrictions
    where 
        restrictions = Restrictions <$> (Just <$> manyPerm  (try domain)) <*> manyPerm  (try notDomain) <* manyPerm (try separator)
        separator = lineSpaces *> char sep <* lineSpaces
        notDomain = char '~' *> domain
                                        
--helpers
eol :: String
eol = "\r\n"

lineSpaces :: Parser ()
lineSpaces = skipMany (satisfy isLineSpace) <?> "white space"
    where isLineSpace c = c == ' ' || c == '\t'

lineEnd :: Parser Char
lineEnd = oneOf eol <|> ('\0' <$ eof)

notLineEnd :: Parser Char
notLineEnd = noneOf eol


getMaybeAll :: [All] -> Maybe Bool
getMaybeAll [] = Nothing
getMaybeAll list = Just $ getAll $ mconcat list

getAllOrFalse :: [All] -> Bool
getAllOrFalse [] = False
getAllOrFalse list = getAll $ mconcat list

noRestrictions :: Restrictions a
noRestrictions = Restrictions Nothing []

fixRestrictions :: (Eq a) => Restrictions a -> Restrictions a
fixRestrictions = deduplicate.allowAll
        where 
        allowAll (Restrictions (Just []) n) = Restrictions Nothing n
        allowAll a = a
        deduplicate (Restrictions (Just p) n) = Restrictions (Just $ nub p) (nub n)
        deduplicate a = a

        
        
        
        