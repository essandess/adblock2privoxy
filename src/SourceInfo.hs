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

data SourceInfo = SourceInfo { _title, _filename :: String, _expires, _version :: Int } | NoInfo

showInfo :: SourceInfo -> [String] 
showInfo NoInfo = ["----- a source skipped -----"]
showInfo (SourceInfo title filename expires version) = [
                    concat ["----- source -----"],
                    concat ["Title: ", title],
                    concat ["Filename: ", filename],
                    concat ["Version: ", show version],
                    concat ["Expires: ", show expires, " hours"]]

extractInfo :: [Line] -> SourceInfo
extractInfo lns@(Line RecordSource{_position = pos} _:_) 
    = execState (sequence $ lineInfo <$> take 50 lns) initial
    where initial = SourceInfo "" (sourceName pos) 0 0
extractInfo _ = NoInfo

lineInfo :: Line -> State SourceInfo ()
lineInfo (Line _ (Comment text)) = do
    info <- get
    let titleParser = (\x -> info{_title = x}) <$> (string "Title: " *> many1 anyChar)
        expiresParser = (\n unit -> info{_expires = unit * read n}) 
            <$> (string "Expires: " *> many1 digit) <*> (24 <$ string " days" <|> 1 <$ string " hours")
        versionParser = (\x -> info{_version = read x}) <$> (string "Version: " *> many1 digit)
        commentParser = skipMany (char ' ') *> try titleParser <|> try expiresParser <|> versionParser
    case parse commentParser "" text of
        Left _ -> return ()
        Right info' -> put info' 
lineInfo _ = return ()
