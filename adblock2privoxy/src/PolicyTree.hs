module PolicyTree (
NodePolicy (..),
DomainTree (..),
PolicyTree,
restrictionsTree,
mergeTrees,
mergePolicyTrees,
trimTree,
mergeAndTrim,
erasePolicy

,domainTree
) where
import Control.Applicative
import InputParser hiding (Policy(..))
import Data.String.Utils (split)
import Utils

data NodePolicy = None | Block | Unblock deriving (Eq, Show)
data DomainTree a = Node { _name :: String, _value :: a, _children :: [DomainTree a] }
type PolicyTree = DomainTree NodePolicy

showTree :: Show a => Int -> DomainTree a -> String
showTree lvl (Node name value children) 
    = concat $  
        [replicate (lvl * 2) ' ', "\"", name, "\" - ", (show value)]
        ++ (('\n':) <$> showTree (lvl + 1) <$> children)

instance Show a => Show (DomainTree a) where
    show = showTree 0

restrictionsTree :: NodePolicy -> Restrictions Domain -> Maybe PolicyTree
restrictionsTree positivePolicy (Restrictions p n) = trimTree positivePolicy <$> mergedTree
    where
    negativePolicy = case positivePolicy of
                        Block -> Unblock
                        _     -> Block  
    positiveTree = case p of
                        Nothing -> Just $ Node "" positivePolicy []
                        Just p' -> concatTrees positivePolicy $ domainTree positivePolicy <$> p'
    negativeTree = concatTrees negativePolicy $ domainTree negativePolicy <$> n
    mergedTree = case negativeTree of
                    Nothing -> positiveTree
                    Just negativeTree' -> mergePolicyTrees negativePolicy negativeTree' <$> positiveTree

erasePolicy :: NodePolicy -> PolicyTree -> PolicyTree
erasePolicy policy (Node n p c) = Node n policy' (erasePolicy policy <$> c)
    where policy'
            | p == policy   = None
            | otherwise     = p

domainTree :: NodePolicy -> Domain -> PolicyTree
domainTree policy domain = makeTree policy $ ("":) $ reverse $ split "." domain

makeTree :: NodePolicy -> [String] -> PolicyTree
makeTree _ [] = error "No nodes proviced"
makeTree policy [node] = Node node policy []
makeTree policy (node:nodes) = Node node None [makeTree policy nodes]

mergeAndTrim :: NodePolicy -> PolicyTree -> PolicyTree -> PolicyTree
mergeAndTrim trump = trimTree trump .*. mergePolicyTrees trump
 
concatTrees :: NodePolicy -> [PolicyTree] -> Maybe PolicyTree
concatTrees _ [] = Nothing
concatTrees _ [tree] = Just tree
concatTrees trump (tree:trees) = mergePolicyTrees trump tree <$> concatTrees trump trees

mergePolicyTrees :: NodePolicy -> PolicyTree -> PolicyTree -> PolicyTree
mergePolicyTrees trump = mergeTrees mergePolicy 
    where
    mergePolicy policy1 policy2
            | policy1 == None      = policy2
            | policy2 == None      = policy1
            | policy1 == trump     = policy1
            | otherwise            = policy2

mergeTrees :: (a -> b -> b) -> DomainTree a -> DomainTree b -> DomainTree b
mergeTrees mergeValue t1@(Node name1 value1 children1) t2@(Node name2 value2 children2)
        = Node mergeName (mergeValue value1 value2) (mergeChildren children1 children2)
        where
        -- names expected to be equal and/or empty
        mergeName 
            | name1 == ""     = name2
            | otherwise       = name1
        
        t1Default = t1{_name = "", _children = []}    
        t2Default = t2{_name = "", _children = []}    
        
        mergeChildren [] [] = []
        mergeChildren (t1Child:t1Children') [] = mergeTrees mergeValue t1Child   t2Default : mergeChildren t1Children' []
        mergeChildren [] (t2Child:t2Children') = mergeTrees mergeValue t1Default t2Child   : mergeChildren []          t2Children'
        mergeChildren t1Children@(t1Child:t1Children') t2Children@(t2Child:t2Children') 
            | _name t1Child == _name t2Child   = mergeTrees mergeValue t1Child   t2Child   : mergeChildren t1Children' t2Children'
            | _name t1Child >  _name t2Child   = mergeTrees mergeValue t1Child   t2Default : mergeChildren t1Children' t2Children
            | otherwise                        = mergeTrees mergeValue t1Default t2Child   : mergeChildren t1Children  t2Children'
    

trimTree :: NodePolicy -> PolicyTree -> PolicyTree
trimTree trump (Node name policy children) = Node name policy childrenFiltered
    where 
    childrenFiltered = filter (not.redundantChild) childrenTrimmed
    childrenTrimmed = trimTree trump <$> children
    redundantChild (Node _ childPolicy childChildren) = samePolicy childPolicy && null childChildren
    samePolicy childPolicy = childPolicy == policy || (policy == None && childPolicy /= trump)  
    
    





