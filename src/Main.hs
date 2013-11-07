module Main where
import InputParser
import System.IO
import ElementBlocker
import UrlBlocker
import Text.ParserCombinators.Parsec hiding (Line, many, optional)
import Statistics
import Control.Applicative hiding (many)
import SourceInfo

f1, outDir :: String
f1 = "/home/alexey/Projects/AdBlock2Privoxy/test-data/testData"
--f1 = "/home/alexey/Projects/AdBlock2Privoxy/test-data/easylist.txt"
--filename = "/home/alexey/Downloads/advblock.txt"
outDir = "/home/alexey/test/ab"
filenames :: [String]
filenames = [f1]

main::IO()
main = do 
        let parseFile filename = do
            putStrLn $ filename ++ ": parsing started"
            inputFile <- openFile filename ReadMode
            text <- hGetContents inputFile
            case parse adblockFile filename text of
                Right parsed -> return (parsed, extractInfo parsed, inputFile)
                Left msg -> return ([], NoInfo, inputFile) <$ putStrLn $ show msg
                    
        (parsed, sourceInfo, handlers) <- unzip3 <$> mapM parseFile filenames   
        let parsed' = concat parsed 
            info    = (sourceInfo >>= showInfo) ++ ["------- end ------\n"]               
        stat outDir info parsed'
        elemBlock (outDir ++ "/site") info parsed'
        urlBlock outDir info parsed'
        _ <- sequence $ hClose <$> handlers
        putStrLn "done"

