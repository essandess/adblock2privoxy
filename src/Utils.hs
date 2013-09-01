{-# LANGUAGE GeneralizedNewtypeDeriving, RankNTypes, ScopedTypeVariables #-}
module Utils (
ZipListM,
getZipListM,
zipListM,

--Struct2 (..),
Struct3 (..),
--Struct4 (..),
--Struct5 (..),
--testSquare
) where
import Control.Applicative hiding (many)
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad 
import Control.Monad.Identity


------------------------------------------------------------------------------------------
----------------------------- export -----------------------------------------------------
------------------------------------------------------------------------------------------

newtype ZipListM a = ZipListM { getZipList' :: ZipList a } deriving (Functor, Applicative)
getZipListM :: ZipListM a -> [a]
getZipListM = getZipList.getZipList'

zipListM = ZipListM . ZipList

instance Monoid a => Monoid (ZipListM a) where
  mempty = pure mempty
  mappend x y = mappend <$> x <*> y
  

--class (Monoid a) => Fff a where
--    fmapTup :: (Monoid a, Fff b, Monoid c) => (a,b) -> c -> c
--    fmapTup (a, b) c = c <> a <>   
--
--
--heap = (1,(2,3,()))

z :: (Applicative f, Applicative g, Monoid a) => f (g a)
z = (pure.pure) mempty

(<%%>) :: (Applicative f, Applicative g) =>
           f (g (a -> b)) -> f (g a) -> f (g b)
(<%%>) = (liftA2 (<*>))

(<$$>) :: (Functor f, Functor g) => (a->b) -> f (g a) -> f (g b)
(<$$>) = (<$>).(<$>)

--class Struct2 f where
--        struct2 :: a1 -> a2 -> f a1 a2
--        destruct2 :: (a1 -> a2 -> b) -> (f a1 a2 -> b)          
--
--square2 :: forall f m a1 a2.(Struct2 f, Applicative m, Monoid a1, Monoid a2) => 
--            (a1 -> m a1) -> (a2 -> m a2) -> [(f a1 a2) -> m (f a1 a2)]
--square2 a1 a2  = []
--    where 
--    
--    structLine = [struct2 <$$> a1 <%%> z]
--    --structLine' = makeSquare (pure'4 struct3 <%> a1 <%> (a2.toA2) <%> (a3.toA3))
--    destructLine = [\l a1' _ -> l a1']
          

class Struct3 f where
        struct3 :: a1 -> a2 -> a3 -> f a1 a2 a3
        destruct3 ::  (a1 -> a2 -> a3 -> b) -> (f a1 a2 a3 -> b)
        
       --des :: l -> (a -> b)
       
        
        
--square3 :: (Struct3 f, Applicative m, Monoid a1, Monoid a2, Monoid a3) => 
--            (a1 -> m a1) -> (a2 -> m a2) -> (a3 -> m a3) -> [(f a1 a2 a3) -> m (f a1 a2 a3)]
--square3 a1 a2 a3 = makeSquare (pure'4 struct3 <%> (a1.toA1) <%> (a2.toA2) <%> (a3.toA3))
--            where 
--                toA1' = (\a _ _ -> a)
--                toA1 = destruct3 toA1'
--                toA2' = (\_ a _ -> a)
--                toA2 = destruct3 toA2'
--                toA3' = (\_ _ a -> a)
--                toA3 = destruct3 toA3'
--                                   
        
        
        
--class Struct4 f where
--        struct4 :: a1 -> a2 -> a3 -> a4 -> f a1 a2 a3 a4
--        square4 :: (Applicative g, Monoid a1, Monoid a2, Monoid a3, Monoid a4) => 
--                    g a1 -> g a2 -> g a3 -> g a4 -> [g (f a1 a2 a3 a4)]
--        square4 a1 a2 a3 a4  = makeSquare (pure'' struct4 <%> a1 <%> a2 <%> a3 <%> a4)
--        
--class Struct5 f where
--        struct5 :: a1 -> a2 -> a3 -> a4 -> a5 -> f a1 a2 a3 a4 a5
--        square5 :: (Applicative g, Monoid a1, Monoid a2, Monoid a3, Monoid a4, Monoid a5) => 
--                    g a1 -> g a2 -> g a3 -> g a4 -> g a5 -> [g (f a1 a2 a3 a4 a5)]
--        square5 a1 a2 a3 a4 a5 = makeSquare (pure'' struct5 <%> a1 <%> a2 <%> a3 <%> a4 <%> a5)

--instance Struct2 (,)    where struct2 = (,)
instance Struct3 (,,)   where 
    struct3 = (,,)
    --destruct3 = id
--instance Struct4 (,,,)  where struct4 = (,,,)
--instance Struct5 (,,,,) where struct5 = (,,,,)

---------------------------------------------------------------------------------------------
------------------------- usage sample ------------------------------------------------------
---------------------------------------------------------------------------------------------

----- result is --------
-- [Just ( "a",  0 , False ),
--  Just ( "" ,  1,  False ),
--  Just ( "" ,  0 , True  )]
-------------------------
--testSquare :: [Maybe (String, Sum Int, Any)]
--testSquare = square3 (Just "a") (Just (Sum 1)) (Just (Any True))

-----------------------------------------------------------------------------------------------
------------------------- implementation ------------------------------------------------------
-----------------------------------------------------------------------------------------------

type Counter c = State Int (Int -> c)

-- wraps value to have something meaningfull only on diagonal places in a matrix like
-- [(V,m),
--  (m,V)]
-- where V is for value, m is for mempty
-- involves 2 applicatives/monads : 
-- State Int a - stores column number
-- Reader ((->) r) - provides row number from outside 
valueOnDiagonal :: (Applicative a, Monoid m) =>  (f -> a m) -> Counter (f -> a m)
valueOnDiagonal val = do
        col <- get
        put (col + 1)
        return (\row -> if row == col 
                                then val 
                                else (\_ -> pure mempty))

-- lifts right argument 2 levels up to become s (r (f a)) where s = State and r = Reader
-- then applies left arg to right one 
-- it's used to put items to a line in matrix
(<%>) :: (Applicative a, Monoid m) => Counter (c -> a (m -> n))
                                       -> (c -> a m)
                                       -> Counter (c -> a n)
(<%>) a b = res
            where
                 --b' :: State Int (Int -> (c -> m a))      
                b' = valueOnDiagonal $ b     
                res = (liftA2.liftA2.liftA2 $ (<*>)) a b' 
                
                

-- creates square matrix from given lines
-- values are on main diagonal
makeSquare :: Counter x -> [x]
makeSquare line = let   start = 0
                        (line', size) = runState line start
                  in    line' <$> [start .. size - 1]
                  
-- pure level 4
pure'4 :: (Applicative f, Applicative g, Applicative h, Applicative a) => b -> f (g (h (a b)))
pure'4 = pure.pure.pure.pure
















