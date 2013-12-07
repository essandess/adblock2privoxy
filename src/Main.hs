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
import Data.Maybe 
import System.Environment
import Control.Monad

f1, f2, f3, f4, f5, outDir, baseDir :: String
baseDir = "/home/alexey/Projects/AdBlock2Privoxy/test-data/"
f1 = baseDir ++ "easylist.txt"
f2 = baseDir ++ "advblock.txt" -- Ru Ad 
f3 = baseDir ++ "bitblock.txt" -- Ru trash blocks includes fanboy-annoyance
f4 = baseDir ++ "easyprivacy.txt"
f5 = baseDir ++ "my.txt"
outDir = "/home/alexey/test/ab"
--filenames :: [String]
--filenames = [f1, f2, f3, f4, f5]
--filenames = ["/home/alexey/Projects/adblock2privoxy/test-data/testData"]


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
                 "FILE")
         "output FILE"
     , Option ['w']     ["webDir"]
         (ReqArg (\ f opts -> opts { _webDir = f })
                 "FILE")
         "input FILE"
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
        header = "Usage: adblock2privoxy [OPTION...] files..."


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
        let parsed' = concat parsed 
            info    = (sourceInfo >>= showInfo) ++ ["------- end ------\n"]               
        stat privoxyDir info parsed'
        elemBlock webDir info parsed'
        urlBlock privoxyDir info parsed'
        sequence $ hClose <$> handlers
        
main::IO()
main = do 
        args <- getArgs
        (opts, filenames) <- parseOptions args
        when (_showVersion opts) $ putStrLn "adblock2privoxy version 1.0"
        when (not . null $_privoxyDir opts) $
                do _ <- processFiles (_privoxyDir opts) (_webDir opts) filenames
                   putStrLn "done"

