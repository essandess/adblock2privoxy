module Utils (
Struct2 (..),
Struct3 (..),
Struct4 (..),
Struct5 (..),
testSquare,
ZipListM,
getZipListM,
zipListM,
maxList,
minList,
compareList,
appendIf,
pure',
pure'',
(<<$>),
(<<<$>),
(<<*>>),
(<<<*>>>),
($>),
($>>),
($>>>),
(.*.)
) where
import Control.Applicative hiding (many)
import Control.Monad.Writer
import Control.Monad.State 

------------------------------------------------------------------------------------------
----------------------------- export -----------------------------------------------------
------------------------------------------------------------------------------------------

-- at least one list should be finite
compareList :: Ord a => [a] -> [a] -> Ordering
compareList = compareList' EQ   
    where
        compareList' lx [] [] = lx
        compareList' _ [] _ = LT
        compareList' _ _ [] = GT
        compareList' lx (x:xs) (y:ys) = compareList' (lx <> compare x y) xs ys 
    
maxList :: Ord a => [a] -> [a] -> [a]
maxList a b = if compareList a b == LT then b else a

minList :: Ord a => [a] -> [a] -> [a]
minList a b = if compareList a b == GT then b else a

appendIf :: Bool -> a -> [a] -> [a]
appendIf condition item list
    | condition = item : list
    | otherwise = list

newtype ZipListM a = ZipListM { getZipList' :: ZipList a } deriving (Functor, Applicative)
getZipListM :: ZipListM a -> [a]
getZipListM = getZipList.getZipList'

zipListM :: [a] -> ZipListM a
zipListM = ZipListM . ZipList

instance Monoid a => Monoid (ZipListM a) where
  mempty = pure mempty
  mappend x y = mappend <$> x <*> y

class Struct2 f where
        struct2 :: a1 -> a2 -> f a1 a2      
        square2 :: (Applicative g, Monoid a1, Monoid a2) => g a1 -> g a2 -> [g (f a1 a2)]
        square2 a1 a2  = makeSquare (pure'' struct2 <%> a1 <%> a2)


class Struct3 f where
        struct3 :: a1 -> a2 -> a3 -> f a1 a2 a3
        square3 :: (Applicative g, Monoid a1, Monoid a2, Monoid a3) => 
                    g a1 -> g a2 -> g a3 -> [g (f a1 a2 a3)]
        square3 a1 a2 a3  = makeSquare (pure'' struct3 <%> a1 <%> a2 <%> a3)
        
class Struct4 f where
        struct4 :: a1 -> a2 -> a3 -> a4 -> f a1 a2 a3 a4
        square4 :: (Applicative g, Monoid a1, Monoid a2, Monoid a3, Monoid a4) => 
                    g a1 -> g a2 -> g a3 -> g a4 -> [g (f a1 a2 a3 a4)]
        square4 a1 a2 a3 a4  = makeSquare (pure'' struct4 <%> a1 <%> a2 <%> a3 <%> a4)
        
class Struct5 f where
        struct5 :: a1 -> a2 -> a3 -> a4 -> a5 -> f a1 a2 a3 a4 a5
        square5 :: (Applicative g, Monoid a1, Monoid a2, Monoid a3, Monoid a4, Monoid a5) => 
                    g a1 -> g a2 -> g a3 -> g a4 -> g a5 -> [g (f a1 a2 a3 a4 a5)]
        square5 a1 a2 a3 a4 a5 = makeSquare (pure'' struct5 <%> a1 <%> a2 <%> a3 <%> a4 <%> a5)

instance Struct2 (,)    where struct2 = (,)
instance Struct3 (,,)   where struct3 = (,,)
instance Struct4 (,,,)  where struct4 = (,,,)
instance Struct5 (,,,,) where struct5 = (,,,,)


---------------------------------------------------------------------------------------------
------------------------- usage sample ------------------------------------------------------
---------------------------------------------------------------------------------------------

----- result is --------
-- [Just ( "a",  0 , False ),
--  Just ( "" ,  1,  False ),
--  Just ( "" ,  0 , True  )]
------------------------- 
testSquare :: [Maybe (String, Sum Int, Any)]
testSquare = square3 (Just "a") (Just (Sum $ length "")) (Just (Any True))

-----------------------------------------------------------------------------------------------
------------------------- implementation ------------------------------------------------------
-----------------------------------------------------------------------------------------------

-- wraps value to have something meaningfull only on diagonal places in a matrix like
-- [(V,m),
--  (m,V)]
-- where V is for value, m is for mempty
-- involves 2 applicatives/monads : 
-- State Int a - stores column number
-- Reader ((->) r) - provides row number from outside 
valueOnDiagonal :: (Applicative f, Monoid a) =>  f a -> State Int (Int -> f a)
valueOnDiagonal val = do
        col <- get
        put (col + 1)
        return (\row -> if row == col 
                                then val 
                                else pure mempty)

-- lifts right argument 2 levels up to become s (r (f a)) where s = State and r = Reader
-- then applies left arg to right one 
-- it's used to put items to a line in matrix
(<%>) :: (Applicative f, Monoid a) => State Int (Int -> f (a -> b))
                                       -> f a -- becomes State Int (Int -> f a) after lift with valueOnDiagonal
                                       -> State Int (Int -> f b)
(<%>) a b = a <<<*>>> valueOnDiagonal b

-- creates square matrix from given lines
-- values are on main diagonal
makeSquare :: State Int (Int -> a) -> [a]
makeSquare line = let   start = 0
                        (line', size) = runState line start
                  in    line' <$> [start .. size - 1]
                  
-- pure level 2
pure' :: (Applicative f, Applicative g) => a -> f (g a)
pure' = pure.pure

-- pure level 3
pure'' :: (Applicative f, Applicative g, Applicative h) => a -> f (g (h a))
pure'' = pure.pure.pure

infixl 4 .*., <<$>, <<<$>, $>, $>>, $>>>, <<*>>, <<<*>>>

(.*.) :: (c -> d) -> 
         (a -> b -> c) -> 
          a -> b -> d
(.*.) = (.).(.)

(<<$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<<$>) = fmap.fmap

(<<<$>) :: (Functor f, Functor g, Functor h) => (a -> b) -> f (g (h a)) -> f (g (h b))
(<<<$>) = fmap.fmap.fmap

($>) :: (Applicative f) => f (a -> b) -> a -> f b
($>) a b = a <*> pure b 

($>>) :: (Applicative f, Applicative g) => f (g (a -> b)) -> a -> f (g b)
($>>) a b = a <<*>> pure' b 

($>>>) :: (Applicative f, Applicative g, Applicative h) => f (g (h (a -> b))) -> a -> f (g (h b))
($>>>) a b = a <<<*>>> pure'' b 

(<<*>>) :: (Applicative f, Applicative g) => f (g (a -> b)) -> f (g a) -> f (g b)
(<<*>>) = liftA2 (<*>)

(<<<*>>>) :: (Applicative f, Applicative g, Applicative h) => f (g (h (a -> b))) ->  f (g (h a)) -> f (g (h b))
(<<<*>>>) = liftA2 (<<*>>)
