module Main where
import InputParser
import ElementBlocker
import UrlBlocker
import Text.ParserCombinators.Parsec hiding (Line, many, optional)
import Task
import Control.Applicative hiding (many)
import SourceInfo
import System.Console.GetOpt
import System.Environment
import Templates (writeTemplateFiles)
import Data.Time.Clock (getCurrentTime)
import Network.HTTP
import Network.URI
import Utils

data Options = Options
     { _showVersion :: Bool
     , _privoxyDir  :: FilePath
     , _webDir      :: FilePath
     , _taskFile    :: FilePath
     } deriving Show

options :: [OptDescr (Options -> Options)]
options =
     [ Option "V" ["version"]
         (NoArg (\ opts -> opts { _showVersion = True }))
         "show version number"
     , Option "p"   ["privoxyDir"]
         (ReqArg (\ f opts -> opts { _privoxyDir = f })
                 "PATH")
         "privoxy config output path (required)"
     , Option "w"   ["webDir"]
         (ReqArg (\ f opts -> opts { _webDir = f })
                 "PATH")
         "css files output path (optional, privoxyDir is used by default)"
     , Option "t"   ["taskFile"]
         (ReqArg (\ f opts -> opts { _taskFile = f })
                 "PATH")
         "path to task file containing urls to process"
     ]

parseOptions :: [String] -> IO (Options, [String])
parseOptions argv =
   case getOpt Permute options argv of
      (opts,nonOpts,[]  ) -> 
                case foldl (flip id) (Options False "" "" "") opts of
                        Options False "" _ _ -> writeError "Privoxy dir is not specified.\n"
                        opts'@(Options _ privoxyDir "" _) -> return (opts'{_webDir = privoxyDir}, nonOpts)
                        opts' -> return (opts', nonOpts)
      (_,_,errs) -> writeError $ concat errs
  
writeError :: String -> IO a
writeError msg = ioError $ userError $ msg ++ usageInfo header options
        where         
        header = "Usage: adblock2privoxy [OPTION...] [URL...]"

getResponse :: String -> IO String
getResponse url = do
        response <- simpleHTTP (getRequest url)
        getResponseBody response

processSources :: String -> String -> [SourceInfo]-> IO ()
processSources privoxyDir webDir sources = do 
        (parsed, sourceInfo) <- unzip <$> mapM parseSource sources   
        let parsed' = concat parsed 
        infoText <- showInfos <$> getCurrentTime $> sourceInfo               
        writeTask privoxyDir infoText parsed'
        elemBlock webDir infoText parsed'
        urlBlock privoxyDir infoText parsed'
        writeTemplateFiles privoxyDir
        where 
        parseSource sourceInfo = do
                            let 
                                url = _url sourceInfo
                                loader = if isURI url then getResponse else readFile
                            putStrLn $ "parse " ++ url
                            text <- loader url
                            now <- getCurrentTime
                            case parse adblockFile url text of
                                Right parsed -> return (parsed, updateInfo now parsed sourceInfo)
                                Left msg -> return ([], sourceInfo) <$ putStrLn $ show msg
        
main::IO()
main = do 
        args <- getArgs
        (opts, urls) <- parseOptions args
        let acton
                | _showVersion opts = putStrLn "adblock2privoxy version 1.0"
                | not . null $ urls 
                   =    processSources (_privoxyDir opts) (_webDir opts) (makeInfo <$> urls)
                | not . null $ _taskFile opts 
                   = do task <- readTask . _taskFile $ opts
                        processSources (_privoxyDir opts) (_webDir opts) (logInfo task)
                | otherwise = writeError "no input specified"
        acton
        putStrLn "done"

