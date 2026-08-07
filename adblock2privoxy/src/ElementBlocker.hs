{-# LANGUAGE StrictData #-}

module ElementBlocker
  ( elemBlock,
  )
where

import Control.Monad
import qualified Data.ByteString.Builder as BSB
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe
import InputParser hiding (Policy (..))
import qualified InputParser
import PolicyTree
import ProgramOptions (DebugLevel (DebugLevel))
import System.Directory
import System.FilePath
import System.IO
import qualified Templates
import Utils

type BlockedRulesTree = DomainTree [Pattern]

data ElemBlockData = ElemBlockData [Pattern] BlockedRulesTree deriving (Show)

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
            if isDir
              then when (not (startswith "." entry')) $ removeDirectoryRecursive entry
              else when (takeExtension entry == ".css") $ removeFile entry
    writeBlockTree :: String -> String -> BlockedRulesTree -> IO ()
    writeBlockTree normalNodePath debugNodePath (Node name patterns children) =
      do
        -- avoid traversing already-created ancestor chain
        createDirectoryIfMissing False normalPath
        when (debug > DebugLevel 1) $ createDirectoryIfMissing False debugPath
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
        writeCssFile normalFilename $
          intercalate
            "\n"
            ( (++ Templates.blockCss) . intercalate ","
                <$> splitEvery 4000 patterns
            )
        when (debugFilename /= "")
          $ writeCssFile debugFilename
          $ intercalate "\n"
          $ (++ Templates.blockCss) <$> patterns
      where
        splitEvery n = takeWhile (not . null) . unfoldr (Just . splitAt n)
        writeCssFile :: String -> String -> IO ()
        writeCssFile filename content =
          do
            outFile <- openBinaryFile filename WriteMode
            -- Build the entire file as one Builder and issue a
            -- single hPutBuilder, instead of opening in text mode
            -- (forcing every hPutStrLn through the Handle's UTF-8
            -- transcoding layer) with 3+ separate small writes
            BSB.hPutBuilder outFile $
              mconcat
                [ BSB.stringUtf8 "/*\n",
                  foldMap (\l -> BSB.stringUtf8 l <> BSB.char7 '\n') info',
                  BSB.stringUtf8 "*/\n",
                  BSB.stringUtf8 content,
                  BSB.char7 '\n'
                ]
            hClose outFile

elemBlockData :: [Line] -> ElemBlockData
elemBlockData input =
  ElemBlockData
    (Map.foldrWithKey appendFlatPattern [] policyTreeMap)
    blockedRulesTree
  where
    -- Per selector/pattern, group all the per-rule domain trees that share it,
    -- then combine each group with ONE balanced merge (mergeBalanced) followed
    -- by one trim. For a selector shared by k rules original was O(k^2) node
    -- operations; this is O(k log k)
    policyTreeMap :: Map.Map String PolicyTree
    policyTreeMap =
      Map.unionWith
        (trimTree Block .*. mergePolicyTrees Unblock)
        blockLinesMap
        (erasePolicy Block <$> unblockLinesMap)
      where
        blockLinesMap = trimTree Block . mergeBalanced (mergePolicyTrees Block) <$> blockGroups
        unblockLinesMap = trimTree Unblock . mergeBalanced (mergePolicyTrees Unblock) <$> unblockGroups

        blockGroups = Map.fromListWith (++) (mapMaybe blockLine input)
        unblockGroups = Map.fromListWith (++) (mapMaybe unblockLine input)

        unblockLine (Line _ (ElementHide domains InputParser.Unblock pattern)) =
          (\t -> (pattern, [t])) <$> restrictionsTree Unblock domains
        unblockLine _ = Nothing
        blockLine (Line _ (ElementHide domains InputParser.Block pattern)) =
          (\t -> (pattern, [t])) <$> restrictionsTree Block domains
        blockLine _ = Nothing

    -- Same approach for the final tree
    blockedRulesTree :: BlockedRulesTree
    blockedRulesTree = case patternTrees of
      [] -> Node "" [] []
      ts -> mergeBalanced (mergeTrees (++)) ts
      where
        patternTrees =
          [ singleTree pattern policyTree
          | (pattern, policyTree) <- Map.toList policyTreeMap
          ]

        singleTree pattern policyTree
          | null (_children policyTree) = Node "" [] []
          | otherwise = mergeTrees appendPattern policyTree (Node "" [] [])
          where
            appendPattern policy patterns = case policy of
              Block -> pattern : patterns
              _ -> patterns

    appendFlatPattern :: Pattern -> PolicyTree -> [Pattern] -> [Pattern]
    appendFlatPattern pattern policyTree patterns
      | null (_children policyTree) && _value policyTree == Block = pattern : patterns
      | otherwise = patterns
