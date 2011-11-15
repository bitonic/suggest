module TST
       ( TST
       , empty
       , singleton
       , toList
       , fromList
       , insert
       , prefix
       , prefixWild
       , lookup
       , matchWild
       ) where

import Control.Arrow (first)

import Wildcard

import Prelude hiding (lookup)

data TST c a = Branch c (TST c a) (TST c a) (TST c a)
             | Null a (TST c a)
             | End

instance (Ord c, Show c, Show a) => Show (TST c a) where
  show = ("fromList " ++) . show . toList

instance (Ord c, Eq c, Eq a) => Eq (TST c a) where
  t1 == t2 = toList t1 == toList t2

empty :: TST l a
empty = End

singleton :: [c] -> a -> TST c a
singleton [] v      = Null v End
singleton (c : s) v = Branch c End (singleton s v) End

toList :: Ord c => TST c a -> [([c], a)]
toList = prefix []

fromList :: Ord c => [([c], a)] -> TST c a
fromList = foldr (uncurry insert) empty

insert :: Ord c => [c] -> a -> TST c a -> TST c a
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

prefix :: Ord c => [c] -> TST c a -> [([c], a)]
prefix _        End               = []
prefix []       (Null v t)        = ([], v) : prefix [] t
prefix []       (Branch c l m r)  =
  prefix [] l ++ map (first (c :)) (prefix [] m) ++ prefix [] r
prefix s        (Null _ t)        = prefix s t
prefix (c1 : s) (Branch c2 l m r) =
  case compare c1 c2 of
    LT -> prefix (c1 : s) l
    EQ -> map (first (c1 :)) (prefix s m)
    GT -> prefix (c1 : s) r

prefixWild :: Ord c => WildList c -> TST c a -> [([c], a)]
prefixWild _        End               = []
prefixWild []       (Null v t)        = ([], v) : prefixWild [] t
prefixWild []       (Branch c l m r)  =
  prefixWild [] l ++ map (first (c :)) (prefixWild [] m) ++ prefixWild [] r
prefixWild s       (Null _ t)        = prefixWild s t
prefixWild (w : s) (Branch c2 l m r) =
  let left   = prefixWild (w : s) l
      middle = map (first (c2 :)) (prefixWild s m)
      right  = prefixWild (w : s) r
  in case w of
    Wildcard -> left ++ middle ++ right
    El c1 -> case compare c1 c2 of
      LT -> left
      EQ -> middle
      GT -> right

lookup :: Ord c => [c] -> TST c a -> Maybe a
lookup s t =
  case prefix s t of
    ((s', v):_) -> if s == s' then Just v else Nothing
    _           -> Nothing

matchWild :: Ord c => WildList c -> TST c a -> [([c], a)]
matchWild w t = filter ((== length w) . length . fst) . prefixWild w $ t
