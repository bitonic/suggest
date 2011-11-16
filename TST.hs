module TST
       ( TST
       , empty
       , singleton
       , toList
       , fromList
       , insert
       , prefix
       , matchWL
       , lookup
       ) where

import Control.Arrow (first)

import Wildcard

import Prelude hiding (lookup)

data TST c a = Branch c (TST c a) (TST c a) (TST c a)
             | End a (TST c a)
             | Null

instance (Ord c, Show c, Show a) => Show (TST c a) where
  show = ("fromList " ++) . show . toList

instance (Ord c, Eq c, Eq a) => Eq (TST c a) where
  t1 == t2 = toList t1 == toList t2

empty :: TST l a
empty = Null

singleton :: [c] -> a -> TST c a
singleton [] v      = End v Null
singleton (c : s) v = Branch c Null (singleton s v) Null

toList :: Ord c => TST c a -> [([c], a)]
toList = prefix []

fromList :: Ord c => [([c], a)] -> TST c a
fromList = foldr (uncurry insert) empty

insert :: Ord c => [c] -> a -> TST c a -> TST c a
insert []       v  Null              = End v Null
insert []       v  (End _ t)         = End v t
insert []       v  (Branch c l m r)  = Branch c (insert [] v l) m r
insert s        v  Null              = singleton s v
insert s        v1 (End v2 t)        = End v2 (insert s v1 t)
insert (c1 : s) v  (Branch c2 l m r) =
  case compare c1 c2 of
    LT -> Branch c2 (insert (c1 : s) v l) m r
    EQ -> Branch c2 l (insert s v m) r
    GT -> Branch c2 l m (insert (c1 : s) v r)

prefix :: Ord c => [c] -> TST c a -> [([c], a)]
prefix _        Null              = []
prefix []       (End v t)         = ([], v) : prefix [] t
prefix []       (Branch c l m r)  =
  prefix [] l ++ map (first (c :)) (prefix [] m) ++ prefix [] r
prefix s        (End _ t)         = prefix s t
prefix (c1 : s) (Branch c2 l m r) =
  case compare c1 c2 of
    LT -> prefix (c1 : s) l
    EQ -> map (first (c1 :)) (prefix s m)
    GT -> prefix (c1 : s) r

matchWL :: Ord c => WildList c -> TST c a -> [([c], a)]
matchWL _       Null              = []
matchWL []      (End v _)         = [([], v)]
matchWL []      (Branch _ l _ _)  = matchWL [] l
matchWL s       (End _ t)         = matchWL s t
matchWL (w : s) (Branch c2 l m r) =
  let left   = matchWL (w : s) l
      middle = map (first (c2 :)) (matchWL s m)
      right  = matchWL (w : s) r
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
