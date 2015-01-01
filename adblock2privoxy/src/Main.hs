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
import Templates
import Data.Time.Clock 
import Network.HTTP.Conduit
import Network.URI
import Data.Text.Lazy.Encoding
import Data.Text.Lazy (unpack)
import Network.Socket
import System.FilePath
import Paths_adblock2privoxy (version)
import Data.Version (showVersion)

data Options = Options
     { _showVersion :: Bool
     , _privoxyDir  :: FilePath
     , _webDir      :: FilePath
     , _taskFile    :: FilePath
     , _forced    :: Bool
     } deriving Show

options :: [OptDescr (Options -> Options)]
options =
     [ Option "v" ["version"]
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
     , Option "f" ["forced"]
         (NoArg (\ opts -> opts { _forced = True }))
         "run even if no sources are expired"
     ]

parseOptions :: [String] -> IO (Options, [String])
parseOptions argv =
   case getOpt Permute options argv of
      (opts,nonOpts,[]  ) -> 
                case foldl (flip id) (Options False "" "" "" False) opts of
                        Options False "" _ _ _ -> writeError "Privoxy dir is not specified.\n"
                        opts'@Options{_showVersion = True} -> return (opts', nonOpts)
                        opts' -> return (setDefaults opts', nonOpts)
      (_,_,errs) -> writeError $ concat errs
   where
        setDefaults opts@(Options _ privoxyDir "" _ _) = setDefaults opts{ _webDir = privoxyDir }
        setDefaults opts@(Options _ privoxyDir _ "" _) = setDefaults opts{ _taskFile = privoxyDir </> "ab2p.task" }
        setDefaults opts = opts  
  
writeError :: String -> IO a
writeError msg = ioError $ userError $ msg ++ usageInfo header options
        where         
        header = "Usage: adblock2privoxy [OPTION...] [URL...]"

getResponse :: String -> IO String
getResponse url = do
        putStrLn $ "load " ++ url ++ "..."
        withSocketsDo $ unpack . decodeUtf8 <$> simpleHttp url

processSources :: String -> String -> String -> [SourceInfo]-> IO ()
processSources privoxyDir webDir taskFile sources = do 
        (parsed, sourceInfo) <- unzip <$> mapM parseSource sources   
        let parsed' = concat parsed 
            infoText = showInfos  sourceInfo               
        writeTask taskFile infoText parsed'
        elemBlock webDir infoText parsed'
        urlBlock privoxyDir infoText parsed'
        writeTemplateFiles privoxyDir
        putStrLn $ "Run 'adblock2privoxy " ++ taskFile ++ "' every 1-2 days to process data updates"
        where 
        parseSource sourceInfo = do
            let 
                url = _url sourceInfo
                loader = if isURI url then getResponse else readFile
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
        (Options printVersion privoxyDir webDir taskFile forced, urls) <- parseOptions args
        let acton
                | printVersion = putStrLn $ "adblock2privoxy version " ++ showVersion version
                | not . null $ urls 
                   =    processSources privoxyDir webDir taskFile (makeInfo <$> urls)
                | not . null $ taskFile 
                   = do task <- readTask taskFile
                        let sources = logInfo task                        
                        if forced || or (infoExpired now <$> sources)                                
                                then processSources privoxyDir webDir taskFile sources
                                else putStrLn "all sources are up to date"
                | otherwise = writeError "no input specified"
        acton
        now' <- getCurrentTime
        putStrLn $ concat ["done in ", show $ diffUTCTime now' now, " seconds"]

