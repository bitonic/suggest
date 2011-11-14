module TST
       ( TST
       , empty
       , singleton
       , toList
       , fromList
       , insert
       , prefix
       ) where

import Control.Arrow (first)

data TST = Branch {-# UNPACK #-} !Char !TST !TST !TST
         | Null Int !TST
         | End

instance Show TST where
  show = ("fromList " ++) . show . toList

instance Eq TST where
  t1 == t2 = toList t1 == toList t2

empty :: TST
empty = End

singleton :: String -> Int -> TST
singleton [] v      = Null v End
singleton (c : s) v = Branch c End (singleton s v) End

toList :: TST -> [(String, Int)]
toList = prefix ""

fromList :: [(String, Int)] -> TST
fromList = foldr (uncurry insert) empty

insert :: String -> Int -> TST -> TST
insert []       v  End               = Null v End
insert []       v  (Null _ t)        = Null v t
insert []       v  (Branch c l m r)  = Branch c (insert [] v l) m r
insert s        v  End               = singleton s v
insert s        v1 (Null v2 t)       = Null v2 (insert s v1 t)
insert (c1 : s) v  (Branch c2 l m r) =
  case compare c1 c2 of
    LT -> Branch c2 (insert (c1 : s) v l) m r
    EQ -> Branch c2 l (insert s v m) r
    GT -> Branch c2 l m (insert (c1 : s) v r)

prefix :: String -> TST -> [(String, Int)]
prefix _        End               = []
prefix s        (Null v t)        = ([], v) : prefix s t
prefix []       (Branch c l m r)  =
  prefix [] l ++ map (first (c :)) (prefix [] m) ++ prefix [] r
prefix (c1 : s) (Branch c2 l m r) =
  case compare c1 c2 of
    LT -> prefix (c1 : s) l
    EQ -> map (first (c1 :)) (prefix s m)
    GT -> prefix (c1 : s) r
