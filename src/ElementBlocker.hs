module ElementBlocker (
elemBlockTree,
BlockedRulesTree,
ElemBlockData (..)
) where
import InputParser
import PolicyTree
import Control.Applicative
import qualified Data.Map as Map
import Data.Maybe
import Utils
    

type BlockedRulesTree = DomainTree [Pattern] 
data ElemBlockData = ElemBlockData [Pattern] BlockedRulesTree deriving Show

elemBlockTree :: [Line] -> ElemBlockData 
elemBlockTree input = ElemBlockData 
                        (Map.foldrWithKey appendFlatPattern []              policyTreeMap)
                        (Map.foldrWithKey appendTreePattern (Node "" [] []) policyTreeMap) 
    where 
    policyTreeMap = policyTrees input
    
    policyTrees :: [Line] -> Map.Map String PolicyTree
    policyTrees lns =  Map.unionWith (trimTree Block .*. mergePolicyTrees Unblock) 
                            blockLinesMap 
                            (erasePolicy Block <$> unblockLinesMap)
        where 
        blockLinesMap = Map.fromListWith (mergeAndTrim Block) (mapMaybe blockLine lns)
        unblockLinesMap = Map.fromListWith (mergeAndTrim Unblock) (mapMaybe unblockLine lns)
        unblockLine (Line _ (ElementHide domains True pattern)) = (,) pattern <$> restrictionsTree Unblock domains
        unblockLine _ = Nothing  
        blockLine (Line _ (ElementHide domains False pattern)) = (,) pattern <$> restrictionsTree Block domains
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

