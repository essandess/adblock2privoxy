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
import PolicyTree
import ElementBlocker


filename :: String
filename = "/home/alexey/Projects/AdBlock2Privoxy/testData"

data Stat = Stat {total,comm,block,el::Int} deriving (Show)

getStat :: Line -> Stat-> Stat
getStat  (Line _ Comment) (Stat t c b e) = Stat (t + 1) (c + 1) b e
getStat  (Line _ RequestBlock {}) (Stat t c b e) = Stat (t + 1) c (b + 1) e
getStat  (Line _ ElementHide {}) (Stat t c b e) = Stat (t + 1) c b (e + 1) 
getStat  _ (Stat t c b e) = Stat (t + 1) c b e

main::IO()
main = do
        inputFile <- openFile filename ReadMode
        text <- hGetContents inputFile
        parsed <- return $ parse adblockFile filename text
        let res = case parsed of
                        Right parsed' -> show $ elemBlockTree $ fixLines parsed'
                        Left msg -> show msg
        ---putStrLn $ show $ fixLines <$> parsed
        putStrLn res
        hClose inputFile
        putStrLn "done"
    where
        lineText (Line t _) = t
        concatText = join . intersperse ('\n':[]) . map lineText
        collectStat = foldr getStat (Stat 0 0 0 0)
        problems (Line _ Error {}) = True
        problems _= False