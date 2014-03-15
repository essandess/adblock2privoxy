module SourceInfo
(
SourceInfo(..),
extractInfo,
showInfo
) where
import InputParser
import Control.Monad.State
import Control.Applicative hiding (many)
import Text.ParserCombinators.Parsec hiding ((<|>),State,Line)
import Data.Time.Clock
import Data.Time.Calendar 
import System.Locale
import Data.Time.Format


data SourceInfo = SourceInfo { _title, _filename, _license, _homepage :: String, 
                               _lastUpdated :: UTCTime, _expires, _version :: Integer } | NoInfo

emptySourceInfo :: SourceInfo
emptySourceInfo = SourceInfo "" "" "" "" (UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0) ) 72 0

showInfo :: UTCTime -> SourceInfo -> [String] 
showInfo _ NoInfo = ["----- a source skipped -----"]
showInfo now sourceInfo@(SourceInfo _ filename _ _ lastUpdated expires _) = 
                    [concat ["----- source -----"]]
                    ++ optionalLine "Title: " _title
                    ++ [concat ["Filename: ", filename],
                    concat ["Last modified: ", formatTime defaultTimeLocale "%d %b %Y %H:%M %Z" lastUpdated],
                    concat ["Expires: ", show expires, " hours", expired]] 
                    ++ optionalLine "Version: " _version
                    ++ optionalLine "License: " _license
                    ++ optionalLine "Homepage: " _homepage
    where 
    expired | (diffUTCTime now lastUpdated) > (fromInteger $ expires * 60 * 60) = " (expired)"
                                | otherwise = []
    optionalLine caption getter | getter sourceInfo == getter emptySourceInfo = []
                                | otherwise = [concat [caption, show $ getter sourceInfo]] 

extractInfo :: [Line] -> SourceInfo
extractInfo lns@(Line RecordSource{_position = pos} _:_) 
    = execState (sequence $ lineInfo <$> take 50 lns) initial
    where initial =emptySourceInfo { _filename = sourceName pos} 
extractInfo _ = NoInfo

lineInfo :: Line -> State SourceInfo ()
lineInfo (Line _ (Comment text)) = do
    info <- get
    let titleParser = (\x -> info{_title = x}) <$> (string "Title: " *> many1 anyChar)
        homepageParser = (\x -> info{_homepage = x}) <$> (string "Homepage: " *> many1 anyChar)
        lastUpdatedParser = (\x -> case x of 
                                        Just time -> info{_lastUpdated = time}
                                        Nothing   -> info) 
            <$> parseTime defaultTimeLocale "%d %b %Y %H:%M %Z" 
            <$> (string "Last modified: " *> many1 anyChar)
        licenseParser = (\x -> info{_license = x}) 
            <$> ((string "Licen" <|> string "Лицензия") *> (manyTill anyChar $ char ':') 
                *> skipMany (char ' ') *> many1 anyChar)
        expiresParser = (\n unit -> info{_expires = unit * read n}) 
            <$> (string "Expires: " *> many1 digit) <*> (24 <$ string " days" <|> 1 <$ string " hours")
        versionParser = (\x -> info{_version = read x}) <$> (string "Version: " *> many1 digit)
        commentParser = skipMany (char ' ') *> 
            (try titleParser <|> try expiresParser <|> try versionParser 
              <|> try licenseParser <|> try homepageParser <|> try lastUpdatedParser)
    case parse commentParser "" text of
        Left _ -> return ()
        Right info' -> put info' 
lineInfo _ = return ()
