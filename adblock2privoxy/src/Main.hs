module Main where
import InputParser
import ElementBlocker
import UrlBlocker
import Text.ParserCombinators.Parsec hiding (Line, many, optional)
import Task
import Control.Applicative hiding (many)
import SourceInfo as Source
import ProgramOptions as Options
import System.Environment
import Templates
import Data.Time.Clock 
import Network.HTTP.Conduit
import Network.URI
import Data.Text.Lazy.Encoding
import Data.Text.Lazy (unpack)
import Network.Socket
import System.Directory
import System.IO


  
getResponse :: String -> IO String
getResponse url = do
        putStrLn $ "load " ++ url ++ "..."
        withSocketsDo $ unpack . decodeUtf8 <$> simpleHttp url
        
getFileContent :: String -> IO String
getFileContent url = do
    handle <- openFile url ReadMode 
    hSetEncoding handle utf8
    hGetContents handle

processSources :: Options -> String -> [SourceInfo]-> IO ()
processSources options taskFile sources = do 
        (parsed, sourceInfo) <- unzip <$> mapM parseSource sources   
        let parsed' = concat parsed 
            sourceInfoText = showInfo sourceInfo
            optionsText = logOptions options               
        writeTask taskFile (sourceInfoText ++ optionsText) parsed'
        if null._cssDomain $ options
                then putStrLn "WARNING: CSS generation is not run because webserver domain is not specified"
                else elemBlock (_webDir options) sourceInfoText parsed'
        urlBlock (_privoxyDir options) sourceInfoText parsed'
        writeTemplateFiles (_privoxyDir options) (_cssDomain options)
        putStrLn $ "Run 'adblock2privoxy -t " ++ taskFile ++ "' every 1-2 days to process data updates."
        where 
        parseSource sourceInfo = do
            let 
                url = _url sourceInfo
                loader = if isURI url then getResponse else getFileContent
            putStrLn $ "process " ++ url
            text <- loader url
            now <- getCurrentTime
            case parse adblockFile url text of
                Right parsed -> 
                        let sourceInfo' = updateInfo now parsed sourceInfo 
                            url' = _url sourceInfo'
                        in if url == url'     
                           then return (parsed, sourceInfo')
                           else parseSource sourceInfo'
                Left msg -> return ([], sourceInfo) <$ putStrLn $ show msg
        
main::IO()
main =  do 
        now <- getCurrentTime
        args <- getArgs
        (options@(Options printVersion _ _ taskFile _ forced), urls) <- parseOptions args
        (options', task) <- do
                fileExists <- doesFileExist taskFile
                if fileExists
                        then do task <- readTask taskFile 
                                return (fillFromLog options task, Just task)
                        else return (options, Nothing)                        
        let 
            action
                | printVersion = putStrLn versionText
                | not . null $ urls 
                   = processSources options' taskFile (makeInfo <$> urls)
                | otherwise = case task of
                        Nothing -> writeError "no input specified"  
                        (Just task') -> do
                                let sources = Source.readLogInfos task'
                                if forced || or (infoExpired now <$> sources)                                
                                        then processSources options' taskFile sources
                                        else putStrLn "all sources are up to date"
                            
        action
        now' <- getCurrentTime
        putStrLn $ concat ["Execution done in ", show $ diffUTCTime now' now, " seconds."]

