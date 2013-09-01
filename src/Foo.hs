{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Foo where


l :: b -> (a -> b) 
l x _ = x

g12 = l -- first item: concatenate l with prev level first item 
g22 = l$id -- next items: pass prev level item to l

g13 :: a1 -> a2 -> a3 -> a1
g13 = l.l
g23 :: a1 -> a2 -> a3 -> a2
g23 = l$l
--g33 :: a1 -> a2 -> a3 -> a3
g33 = l$g23
  
g41 = l.l.l 
g42 = l$l.l
g43 = l$l$l
g44 = l$l$l$id

g51 = l.l.l.l 
g52 = l$l.l.l
g53 = l$l$l.l
g54 = l$l$l$l
g55 = l$l$l$l$id


