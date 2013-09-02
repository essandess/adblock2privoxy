module Main where
import InputParser
import Control.Applicative hiding ((<|>))
import Text.ParserCombinators.Parsec hiding (Line, many, optional)
import Control.Monad
import Data.List
import System.IO
import ParsecExt
import Normalizer
import Utils


filename :: String
--filename = "/home/alexey/Downloads/advblock.txt"
filename = "/home/alexey/Downloads/easylist.txt"
filename1 = "/home/alexey/Downloads/easylist1.txt"

data Stat = Stat {total,comm,block,el::Int} deriving (Show)

getStat :: Line -> Stat-> Stat
getStat  (Line _ Comment) (Stat t c b e) = Stat (t + 1) (c + 1) b e
getStat  (Line _ (RequestBlock _ _ _)) (Stat t c b e) = Stat (t + 1) c (b + 1) e
getStat  (Line _ (ElementHide _ _ _)) (Stat t c b e) = Stat (t + 1) c b (e + 1) 
getStat  _ (Stat t c b e) = Stat (t + 1) c b e

main::IO()
main = do
        inputFile <- openFile filename ReadMode
        outFile <- openFile filename1 WriteMode
        text <- hGetContents inputFile
        --(Right res) <- return $ (concatText <$> filter problems <$> parse adblockFile filename text)
        parsed <- return $ (collectStat <$> parse adblockFile filename text)
        putStr.show$parsed
        hClose inputFile
        hClose outFile
        putStrLn "done"
    where
        lineText (Line t _) = t
        concatText = join . intersperse ('\n':[]) . map lineText
        collectStat = foldr getStat (Stat 0 0 0 0)
        problems (Line _ Unknown) = True
        problems _= False