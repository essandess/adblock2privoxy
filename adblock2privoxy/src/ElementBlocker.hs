{-# LANGUAGE StrictData #-}

module ElementBlocker (
elemBlock
) where
import InputParser hiding (Policy(..))
import qualified InputParser
import PolicyTree
import ProgramOptions (DebugLevel(DebugLevel))
import qualified Data.Map as Map
import Data.Maybe
import Utils
import System.IO
import System.FilePath
import Data.List
import System.Directory
import qualified Templates
import Control.Monad
import Data.String.Utils (startswith)


type BlockedRulesTree = DomainTree [Pattern]
data ElemBlockData = ElemBlockData [Pattern] BlockedRulesTree deriving Show

elemBlock :: String -> [String] -> DebugLevel -> [Line] -> IO ()
elemBlock path info debug = writeElemBlock . elemBlockData
    where
    writeElemBlock :: ElemBlockData -> IO ()
    writeElemBlock (ElemBlockData flatPatterns rulesTree) =
        do
           let debugPath = path </> "debug"
               filteredInfo = filter ((||) <$> not . startswith "Url:" <*> startswith "Url: http") info
           createDirectoryIfMissing True path
           cont <- getDirectoryContents path
           mapM_ removeOld cont
           when (debug > DebugLevel 0) $ createDirectoryIfMissing True debugPath
           writeBlockTree path debugPath rulesTree
           writePatterns filteredInfo (path </> "ab2p.common.css") (if debug > DebugLevel 0 then debugPath </> "ab2p.common.css" else "") flatPatterns
    removeOld entry' =
        let entry = path </> entry'
        in do
           isDir <- doesDirectoryExist entry
           if isDir then when (head entry' /= '.') $ removeDirectoryRecursive entry
                    else when (takeExtension entry == ".css") $ removeFile entry
    writeBlockTree :: String -> String -> BlockedRulesTree -> IO ()
    writeBlockTree normalNodePath debugNodePath (Node name patterns children) =
        do
            createDirectoryIfMissing True normalPath
            when (debug > DebugLevel 1) $ createDirectoryIfMissing True debugPath
            mapM_ (writeBlockTree normalPath debugPath) children
            writePatterns ["See ab2p.common.css for sources info"] normalFilename (if debug > DebugLevel 1 then debugFilename else "") patterns
        where
            normalPath
                | null name = normalNodePath
                | otherwise = normalNodePath </> name
            debugPath
                | null name = debugNodePath
                | otherwise = debugNodePath </> name
            normalFilename = normalPath </> "ab2p.css"
            debugFilename = debugPath </> "ab2p.css"
    writePatterns :: [String] -> String -> String -> [Pattern] -> IO ()
    writePatterns _ _ _ [] = return ()
    writePatterns info' normalFilename debugFilename patterns =
         do
            writeCssFile normalFilename $ intercalate "\n" ((++ Templates.blockCss) . intercalate "," <$>
                    splitEvery 4000 patterns)
            when (debugFilename /= "") $
                writeCssFile debugFilename $ intercalate "\n" $ (++ Templates.blockCss) <$> patterns
         where
         splitEvery n = takeWhile (not . null) . unfoldr (Just . splitAt n)
         writeCssFile filename content =
                do outFile <- openFile filename WriteMode
                   hSetEncoding outFile utf8
                   hPutStrLn outFile "/*"
                   mapM_ (hPutStrLn outFile) info'
                   hPutStrLn outFile "*/"
                   hPutStrLn outFile content
                   hClose outFile

elemBlockData :: [Line] -> ElemBlockData
elemBlockData input = ElemBlockData
                        (Map.foldrWithKey appendFlatPattern []              policyTreeMap)
                        (Map.foldrWithKey appendTreePattern (Node "" [] []) policyTreeMap)
    where
    policyTreeMap :: Map.Map String PolicyTree
    policyTreeMap =  Map.unionWith (trimTree Block .*. mergePolicyTrees Unblock)
                            blockLinesMap
                            (erasePolicy Block <$> unblockLinesMap)
        where
        blockLinesMap = Map.fromListWith (mergeAndTrim Block) (mapMaybe blockLine input)
        unblockLinesMap = Map.fromListWith (mergeAndTrim Unblock) (mapMaybe unblockLine input)
        unblockLine (Line _ (ElementHide domains InputParser.Unblock pattern)) = (,) pattern <$> restrictionsTree Unblock domains
        unblockLine _ = Nothing
        blockLine (Line _ (ElementHide domains InputParser.Block pattern)) = (,) pattern <$> restrictionsTree Block domains
        blockLine _ = Nothing

    appendTreePattern ::  Pattern -> PolicyTree -> BlockedRulesTree -> BlockedRulesTree
    appendTreePattern pattern policyTree
          | null $ _children policyTree     = id
          | otherwise                       = mergeTrees appendPattern policyTree
       where appendPattern policy patterns = case policy of
                                                Block -> pattern:patterns
                                                _     -> patterns

    appendFlatPattern ::  Pattern -> PolicyTree -> [Pattern] -> [Pattern]
    appendFlatPattern pattern policyTree patterns
          | null (_children policyTree) && _value policyTree == Block  = pattern:patterns
          | otherwise                                                  = patterns
