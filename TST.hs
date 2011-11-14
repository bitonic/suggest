module TST
       ( TST
       , empty
       , singleton
       , toList
       , fromList
       , insert
       , prefix
       ) where

data TST = Branch {-# UNPACK #-} !Char !TST !TST !TST
         | Null !TST
         | End

instance Show TST where
  show = ("fromList " ++) . show . toList

instance Eq TST where
  t1 == t2 = toList t1 == toList t2

empty :: TST
empty = End

singleton :: String -> TST
singleton [] = Null End
singleton (c : s) = Branch c End (singleton s) End

toList :: TST -> [String]
toList = prefix ""

fromList :: [String] -> TST
fromList = foldr insert empty

insert :: String -> TST -> TST
insert []       End               = Null End
insert []       t@(Null _)        = t
insert []       (Branch c l m r)  = Branch c (insert [] l) m r
insert s        End               = singleton s
insert s        (Null t)          = Null (insert s t)
insert (c1 : s) (Branch c2 l m r) =
  case compare c1 c2 of
    LT -> Branch c2 (insert (c1 : s) l) m r
    EQ -> Branch c2 l (insert s m) r
    GT -> Branch c2 l m (insert (c1 : s) r)

prefix :: String -> TST -> [String]
prefix _        End               = []
prefix s        (Null t)          = [] : prefix s t
prefix []       (Branch c l m r)  =
  map (c :) (prefix [] l ++ prefix [] m ++ prefix [] r)
prefix (c1 : s) (Branch c2 l m r) =
  case compare c1 c2 of
    LT -> prefix (c1 : s) l
    EQ -> map (c1 :) (prefix s m)
    GT -> prefix (c1 : s) r
