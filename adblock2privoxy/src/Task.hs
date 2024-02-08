module Task (
writeTask,
readTask
) where
import System.IO.Strict as Strict
import System.IO
import InputParser
import Statistics

writeTask :: String -> [String] -> [Line] -> IO ()
writeTask filename info lns =
    let
        statistics = collectStat lns
        errorLine (Line position (Error text))
            = [concat ["ERROR: ", recordSourceText position, " - ", text]]
        errorLine _ = []
    in do
        outFile <- openFile filename WriteMode
        mapM_ (hPutStrLn outFile) info
        mapM_ (hPutStrLn outFile) statistics
        mapM_ (hPutStrLn outFile) (lns >>= errorLine)
        hClose outFile

readTask :: String -> IO [String]
readTask path = do
        result <- lines <$> Strict.readFile path
        return $ length result `seq` result --read whole file to allow its overwriting
