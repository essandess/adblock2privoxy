module Task (
writeTask,
readTask
) where
import System.IO
import System.FilePath
import InputParser
import Statistics
import Control.Applicative ((<$>))

writeTask :: String -> [String] -> [Line] -> IO ()
writeTask path info lns = 
    let 
        statistics = collectStat lns
        filename = path </> "ab2p.task"
        errorLine (Line position (Error text)) 
            = [concat ["ERROR: ", recordSourceText position, " - ", text]]
        errorLine _ = []
    in do  
        outFile <- openFile filename WriteMode
        _ <- mapM (hPutStrLn outFile) info
        _ <- sequence $ hPutStrLn outFile <$> statistics 
        _ <- sequence $ hPutStrLn outFile <$> (lns >>= errorLine)
        hClose outFile

readTask :: String -> IO [String]       
readTask path = lines <$> readFile path
