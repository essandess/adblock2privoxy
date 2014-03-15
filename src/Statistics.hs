module Statistics where
import qualified Data.Map as Map
import InputParser
import Data.Maybe 
import System.IO
import System.FilePath
import Control.Applicative ((<$>))
import Control.Monad.State

type Stat = Map.Map String Int 

stat :: String -> [String] -> [Line] -> IO ()
stat path info lns = 
    let result = collectStat lns 
        filename = path </> "ab2p.stat"
        resultLine (name, value) = concat [name, ": ", show value] 
        errorLine (Line position (Error text)) 
            = [concat ["ERROR: ", recordSourceText position, " - ", text]]
        errorLine _ = []
    in do  
        outFile <- openFile filename WriteMode
        _ <- mapM (hPutStrLn outFile) info
        _ <- sequence $ hPutStrLn outFile . resultLine <$> Map.toAscList result
        _ <- sequence $ hPutStrLn outFile <$> (lns >>= errorLine)
        hClose outFile

collectStat :: [Line] -> Stat
collectStat = foldr getStat Map.empty

increment :: String -> Stat-> Stat
increment key = Map.insertWith (+) key 1

isJustFilled :: Maybe [a] -> Bool
isJustFilled Nothing = False
isJustFilled (Just list) = not.null $ list


getStat :: Line -> Stat-> Stat
getStat  (Line _ Comment {} ) = increment "Comments"
getStat  (Line _ Error {}) = increment "Errors"
getStat  (Line _ ElementHide {}) = increment "Elements hiding rules"
getStat  (Line _ (RequestBlock policy _ (RequestOptions _ thirdParty domains _ _ _ _ _))) = execState stateState
    where 
    incrementState = modify . increment
    stateState = do
        incrementState "Request block rules total"
        when (policy == InputParser.Unblock) $ incrementState "Request block rules for exception"
        when (isJust thirdParty) $ incrementState "Rules with third party option"
        when ((not.null._negative $ domains) || (isJustFilled . _positive $ domains)) $ incrementState "Request block rules with domain option"
        when ((not.null._negative $ domains) || (isJustFilled . _positive $ domains)) $ incrementState "Request block rules with request type options"
        

       
       
       