module ElementBlocker (
elemBlockData,
BlockedRulesTree,
ElemBlockData (..),
writeElemBlock
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

writeElemBlock :: String -> ElemBlockData -> IO ()
writeElemBlock path (ElemBlockData flatPatterns rulesTree) = 
    do
       writeBlockTree path rulesTree 
       writePatterns (path </> "adblock.common.css") flatPatterns           

writeBlockTree :: String -> BlockedRulesTree -> IO ()
writeBlockTree path (Node name patterns children) =
    do
        createDirectoryIfMissing True path'
        _ <- sequence (writeBlockTree path' <$> children)
        writePatterns filename patterns        
    where
        path' 
            | null name = path
            | otherwise = path </> name
        filename = path' </> "adblock.css"
        
writePatterns :: String -> [Pattern] -> IO ()
writePatterns filename patterns = 
     do outFile <- openFile filename WriteMode
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

