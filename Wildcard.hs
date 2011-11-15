module Wildcard where

data Wildcard a = Wildcard
                | El a
           deriving (Eq, Ord)


type WildList a = [Wildcard a]

instance Show a => Show (Wildcard a) where
  show Wildcard = "*"
  show (El c)   = show c

wildList :: [a] -> WildList a
wildList = map El

wildString :: String -> WildList Char
wildString = map (\c -> if c == '*' then Wildcard else El c)

matches :: Eq a => WildList a -> [a] -> Bool
matches []             []       = True
matches []             _        = False
matches _              []       = False
matches (Wildcard : w) (_ : s)  = matches w s
matches (El c1 : w)    (c2 : s) | c1 == c2  = matches w s
                                | otherwise = False

edits :: WildList a -> [WildList a]
edits s' = concatMap init [ deleted s'
                          , transposes s'
                          , replaces s'
                          , inserts s'
                          ]
  where
    deleted []      = [[]]
    deleted (c : s) = s : map (c :) (deleted s)

    transposes []  = [[]]
    transposes [x] = [[x]]
    transposes (x : y : s) = (y : x : s) : map (x :) (transposes (y : s))

    replaces [] = [[]]
    replaces (c : s) = (Wildcard : s) : map (c :) (replaces s)
    
    inserts [] = [[]]
    inserts (c : s) = (Wildcard : c : s) : map (c :) (inserts s)

