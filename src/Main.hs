module Main where
import InputParser
import System.IO
import ElementBlocker
import UrlBlocker
import Text.ParserCombinators.Parsec hiding (Line, many, optional)
import Statistics
import Control.Applicative hiding (many)
import SourceInfo
import System.Console.GetOpt
import System.Environment
import Control.Monad
import Templates (writeTemplateFiles)
import Data.Time.Clock (getCurrentTime)

data Options = Options
     { _showVersion :: Bool
     , _privoxyDir  :: FilePath
     , _webDir      :: FilePath
     } deriving Show

options :: [OptDescr (Options -> Options)]
options =
     [ Option ['V'] ["version"]
         (NoArg (\ opts -> opts { _showVersion = True }))
         "show version number"
     , Option ['p']     ["privoxyDir"]
         (ReqArg (\ f opts -> opts { _privoxyDir = f })
                 "PATH")
         "privoxy config output path"
     , Option ['w']     ["webDir"]
         (ReqArg (\ f opts -> opts { _webDir = f })
                 "PATH")
         "css files output path"
     ]

parseOptions :: [String] -> IO (Options, [String])
parseOptions argv =
   case getOpt Permute options argv of
      (opts,nonOpts,[]  ) -> 
                case foldl (flip id) (Options False [] []) opts of
                        Options False [] _ -> writeError "Privoxy dir is not specified.\n"
                        opts'@(Options _ privoxyDir []) -> return (opts'{_webDir = privoxyDir}, nonOpts)
                        opts' -> return (opts', nonOpts)
      (_,_,errs) -> writeError $ concat errs
  where 
        writeError msg = ioError $ userError $ msg ++ usageInfo header options
        header = "Usage: adblock2privoxy [OPTION...] adblockFiles..."


processFiles :: String -> String -> [String] -> IO [()]
processFiles privoxyDir webDir filenames = do 
        let parseFile filename = do
            putStrLn $ "parse " ++ filename
            inputFile <- openFile filename ReadMode
            text <- hGetContents inputFile
            case parse adblockFile filename text of
                Right parsed -> return (parsed, extractInfo parsed, inputFile)
                Left msg -> return ([], NoInfo, inputFile) <$ putStrLn $ show msg
                    
        (parsed, sourceInfo, handlers) <- unzip3 <$> mapM parseFile filenames
        showInfo' <- showInfo <$> getCurrentTime    
        let parsed' = concat parsed 
            info    = (sourceInfo >>= showInfo') ++ ["------- end ------\n"]               
        stat privoxyDir info parsed'
        elemBlock webDir info parsed'
        urlBlock privoxyDir info parsed'
        writeTemplateFiles privoxyDir
        sequence $ hClose <$> handlers
        
main::IO()
main = do 
        args <- getArgs
        (opts, filenames) <- parseOptions args
        when (_showVersion opts) $ putStrLn "adblock2privoxy version 1.0"
        when (not . null $_privoxyDir opts) $
                do _ <- processFiles (_privoxyDir opts) (_webDir opts) filenames
                   putStrLn "done"

