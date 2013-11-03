module Statistics where
import qualified Data.Map as Map
import InputParser
import Data.Maybe 
import System.IO
import System.FilePath.Posix

type Stat = Map.Map String Int 

stat :: String -> [Line] -> IO ()
stat path lns = 
    let result = collectStat lns 
        filename = path </> "stat.txt"
    in do  
        outFile <- openFile filename WriteMode
        hPutStrLn outFile $ show result
        hClose outFile

collectStat :: [Line] -> Stat
collectStat = foldr getStat (Map.empty)

increment :: String -> Stat-> Stat
increment key statData = Map.insertWith (+) key 1 statData

isJustFilled :: Maybe [a] -> Bool
isJustFilled Nothing = False
isJustFilled (Just list) = not.null $ list

getStat :: Line -> Stat-> Stat
getStat  (Line _ Comment) = increment "comments"
getStat  (Line _ Error {}) = increment "errors"
getStat  (Line _ ElementHide {}) = increment "elemHide"
getStat  (Line _ (RequestBlock policy _ (RequestOptions requestType thirdParty domains _ _ _ _))) = r
    where 
    r s = r8 s
    r1 = increment "RBlock"
    r2 | (policy == InputParser.Unblock) = r1 . increment "excludeRBlock"
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