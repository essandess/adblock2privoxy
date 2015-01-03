module SourceInfo
(
SourceInfo(_url),
showInfo,
updateInfo,
makeInfo,
readLogInfos,
infoExpired
) where
import InputParser
import Control.Monad.State
import Control.Applicative hiding (many)
import Text.ParserCombinators.Parsec hiding ((<|>),State,Line)
import Data.Time.Clock
import Data.Time.Calendar 
import System.Locale
import Data.Time.Format
import Data.Maybe (catMaybes)
import Data.String.Utils (split)


data SourceInfo = SourceInfo { _title, _url, _license, _homepage :: String, 
                               _lastUpdated :: UTCTime, _expires, _version :: Integer, _expired :: Bool }

emptySourceInfo :: SourceInfo
emptySourceInfo = SourceInfo "" "" "" "" (UTCTime (ModifiedJulianDay 0) (secondsToDiffTime 0) ) 72 0 True

separator :: String
separator = "----- source -----"

endMark :: String
endMark = "------- end ------"

showInfo :: [SourceInfo] -> [String] 
showInfo sourceInfos = (sourceInfos >>= showInfoItem) ++ [endMark ++ "\n"]

showInfoItem :: SourceInfo -> [String] 
showInfoItem sourceInfo@(SourceInfo _ url _ _ lastUpdated expires _ expired) = 
        catMaybes [ Just separator,
                    optionalLine "Title: " _title,
                    Just $ "Url: " ++ url,
                    Just $ "Last modified: " ++ formatTime defaultTimeLocale "%d %b %Y %H:%M %Z" lastUpdated,
                    Just $ concat ["Expires: ", show expires, " hours", expiredMark], 
                    optionalLine "Version: " $ show . _version,
                    optionalLine "License: " _license,
                    optionalLine "Homepage: " _homepage ]
    where 
    expiredMark | expired = " (expired)"
                | otherwise = ""
    optionalLine caption getter | getter sourceInfo == getter emptySourceInfo = Nothing
                                | otherwise = Just $ caption ++ getter sourceInfo 

updateInfo :: UTCTime -> [Line] -> SourceInfo -> SourceInfo
updateInfo now lns old
    = updated { _expired = infoExpired now updated } 
    where 
    initial = old { _lastUpdated = now } 
    updated = execState (sequence $ parseInfo . lineComment <$> take 50 lns) initial
    
makeInfo :: String -> SourceInfo
makeInfo url = emptySourceInfo { _url = url }

readLogInfos :: [String] -> [SourceInfo]
readLogInfos lns = chunkInfo <$> chunks
   where 
   chunks = filter (not.null) . split [separator] . takeWhile (/= endMark) $ lns
   chunkInfo chunk = execState (sequence $ parseInfo <$> chunk) emptySourceInfo

infoExpired :: UTCTime -> SourceInfo -> Bool
infoExpired now (SourceInfo _ _ _ _ lastUpdated expires _ _ ) = 
        diffUTCTime now lastUpdated > fromInteger (expires * 60 * 60)

lineComment :: Line -> String
lineComment (Line _ (Comment text)) = text
lineComment _ = "" 

parseInfo :: String -> State SourceInfo ()
parseInfo text = do
    info <- get
    let urlParser = (\x -> info{_url = x}) <$> ((string "Url: " <|> string "Redirect: ") *> many1 anyChar)
        titleParser = (\x -> info{_title = x}) <$> (string "Title: " *> many1 anyChar)
        homepageParser = (\x -> info{_homepage = x}) <$> (string "Homepage: " *> many1 anyChar)
        lastUpdatedParser = (\x -> case x of 
                                        Just time -> info{_lastUpdated = time}
                                        Nothing   -> info) 
            <$> parseTime defaultTimeLocale "%d %b %Y %H:%M %Z" 
            <$> (string "Last modified: " *> many1 anyChar)
        licenseParser = (\x -> info{_license = x}) 
            <$> ((string "Licen" <|> string "Лицензия") *> manyTill anyChar (char ':') 
                *> skipMany (char ' ') *> many1 anyChar)
        expiresParser = (\n unit -> info{_expires = unit * read n}) 
            <$> (string "Expires: " *> many1 digit) <*> (24 <$ string " days" <|> 1 <$ string " hours") 
        versionParser = (\x -> info{_version = read x}) <$> (string "Version: " *> many1 digit)
        stringParser = skipMany (char ' ') *> 
            (try urlParser <|> try titleParser <|> try expiresParser <|> try versionParser 
              <|> try licenseParser <|> try homepageParser <|> try lastUpdatedParser)
    case parse stringParser "" text of
        Left _ -> return ()
        Right info' -> put info' 
