module ElementBlocker (
elemBlock
) where
import InputParser hiding (Policy(..))
import qualified InputParser 
import PolicyTree
import Control.Applicative
import qualified Data.Map as Map
import Data.Maybe
import Utils
import System.IO
import System.FilePath.Posix
import Data.List 
import System.Directory (createDirectoryIfMissing)
import Control.Monad (unless)
import qualified Templates 
  

type BlockedRulesTree = DomainTree [Pattern] 
data ElemBlockData = ElemBlockData [Pattern] BlockedRulesTree deriving Show

elemBlock :: String -> [String] -> [Line] -> IO ()
elemBlock path info = writeElemBlock . elemBlockData
    where
    writeElemBlock :: ElemBlockData -> IO ()
    writeElemBlock (ElemBlockData flatPatterns rulesTree) = 
        do
           writeBlockTree path rulesTree 
           writePatterns (path </> "ab2p.common.css") flatPatterns           
    writeBlockTree :: String -> BlockedRulesTree -> IO ()
    writeBlockTree nodePath (Node name patterns children) =
        do
            createDirectoryIfMissing True path'
            _ <- sequence (writeBlockTree path' <$> children)
            writePatterns filename patterns        
        where
            path' 
                | null name = nodePath
                | otherwise = nodePath </> name
            filename = path' </> "ab2p.css"      
    writePatterns :: String -> [Pattern] -> IO ()
    writePatterns filename patterns = 
         do outFile <- openFile filename WriteMode
            hPutStrLn outFile "/*"
            _ <- mapM (hPutStrLn outFile) $ info
            hPutStrLn outFile "*/"
            hPutStrLn outFile $ intercalate "," patterns
            unless (null patterns) $ hPutStrLn outFile $ Templates.blockCss
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

