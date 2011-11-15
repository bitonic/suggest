module TST
       ( TST
       , empty
       , singleton
       , toList
       , fromList
       , insert
       , prefix
       , lookup
       ) where

import Control.Arrow (first)

import Prelude hiding (lookup)

data TST a = Branch {-# UNPACK #-} !Char !(TST a) !(TST a) !(TST a)
           | Null a !(TST a)
           | End

instance Show a => Show (TST a) where
  show = ("fromList " ++) . show . toList

instance Eq a => Eq (TST a) where
  t1 == t2 = toList t1 == toList t2

empty :: TST a
empty = End

singleton :: String -> a -> TST a
singleton [] v      = Null v End
singleton (c : s) v = Branch c End (singleton s v) End

toList :: TST a -> [(String, a)]
toList = prefix ""

fromList :: [(String, a)] -> TST a
fromList = foldr (uncurry insert) empty

insert :: String -> a -> TST a -> TST a
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

prefix :: String -> TST a -> [(String, a)]
prefix _        End               = []
prefix s        (Null v t)        = ([], v) : prefix s t
prefix []       (Branch c l m r)  =
  prefix [] l ++ map (first (c :)) (prefix [] m) ++ prefix [] r
prefix (c1 : s) (Branch c2 l m r) =
  case compare c1 c2 of
    LT -> prefix (c1 : s) l
    EQ -> map (first (c1 :)) (prefix s m)
    GT -> prefix (c1 : s) r

lookup :: String -> TST a -> Maybe a
lookup s t =
  case prefix s t of
    ((s', v):_) -> if s == s' then Just v else Nothing
    _           -> Nothing
