module Main where
import InputParser
import Control.Applicative 
import Text.ParserCombinators.Parsec hiding (Line, many, optional, (<|>))
import Control.Monad
import Data.List
import System.IO
import ParsecExt
import Normalizer
import Utils
import PolicyTree
import ElementBlocker
import qualified Data.Map as Map
import Data.Maybe 


filename :: String
--filename = "/home/alexey/Projects/AdBlock2Privoxy/testData"c
filename = "/home/alexey/Downloads/easylist.txt"
--filename = "/home/alexey/Downloads/advblock.txt"
outDir = "/home/alexey/test/ab"
type Stat = Map.Map String Int 


increment :: String -> Stat-> Stat
increment key map = Map.insertWith (+) key 1 map

isJustFilled Nothing = False
isJustFilled (Just list) = not.null $ list

getStat :: Line -> Stat-> Stat
getStat  (Line _ Comment) = increment "comments"
getStat  (Line _ Error {}) = increment "errors"
getStat  (Line _ ElementHide {}) = increment "elemHide"
getStat  (Line _ (RequestBlock exclude _ (RequestOptions requestType thirdParty domains _ _ _ _))) = r
    where 

    r s = r8 s
    r1 = increment "RBlock"
    r2 | exclude = r1 . increment "excludeRBlock"
       | otherwise = r1
    r3 | isJust thirdParty = r2 . increment "thidrPartyRBlock"
       | otherwise = r2
    r4 | (not.null._negative $ domains) || ((isJustFilled . _positive) $ domains) = r3 . increment "domainsRBlock"
       | otherwise = r3
    r5 | isJust thirdParty && ((not.null._negative $ domains) || ((isJustFilled . _positive) $ domains)) = r4 . increment "domains&thirdPartyRBlock"
       | otherwise = r4
    r6 | ((not.null._negative $ requestType) && ((isJustFilled . _positive) $ requestType)) = r5 . increment "mixedRequestTypeRBlock"
       | otherwise = r5
    r7 | ((not.null._negative $ requestType) || ((isJustFilled . _positive) $ requestType)) = r6 . increment "requestTypeRBlock"
       | otherwise = r6
    r8 | isJust thirdParty && ((not.null._negative $ requestType) || ((isJustFilled . _positive) $ requestType)) = r7 . increment "requestType&thirdPartyRBlock"
       | otherwise = r7
    

getWrong  (Line text (RequestBlock exclude _ (RequestOptions requestType thirdParty domains _ _ _ _)))
    | ((not.null._negative $ requestType) && ((isJustFilled . _positive) $ requestType)) = Just text
getWrong _ = Nothing

main::IO()
main = do
        inputFile <- openFile filename ReadMode
        text <- hGetContents inputFile
        parsed <- return $ parse adblockFile filename text
        
        case parsed of
            --Right parsed' -> writeElemBlock outDir $ (elemBlockData $ (fixLines $ parsed'))
            Right parsed' -> putStrLn $ show $ collectStat parsed'
            Left msg -> putStrLn $ show msg
        ---putStrLn $ show $ fixLines <$> parsed
        --putStrLn res
        hClose inputFile
        putStrLn "done"
    where
        collectStat = foldr getStat (Map.empty)
