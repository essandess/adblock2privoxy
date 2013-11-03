module Main where
import InputParser
import System.IO
import ElementBlocker
import UrlBlocker
import Text.ParserCombinators.Parsec hiding (Line, many, optional)
import Statistics

filename, outDir :: String
filename = "/home/alexey/Projects/AdBlock2Privoxy/test-data/testData"
--filename = "/home/alexey/Downloads/easylist.txt"
--filename = "/home/alexey/Downloads/advblock.txt"
outDir = "/home/alexey/test/ab"


main::IO()
main = do
        putStrLn $ filename ++ ": parsing started"
        inputFile <- openFile filename ReadMode
        text <- hGetContents inputFile
        let parsed = parse adblockFile filename text
        putStrLn $ show parsed
        case parsed of 
            Right parsed' -> 
                do 
                   stat outDir parsed'
                   elemBlock (outDir ++ "/site") parsed'
                   urlBlock outDir parsed'
            Left msg -> putStrLn $ show msg
        hClose inputFile
        putStrLn "done"

