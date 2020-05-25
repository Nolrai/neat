module Utils (isAsc) where

isAsc :: Ord a => [a] -> Bool
isAsc (x : y : ys) = x < y && isAsc (y : ys)
isAsc [_] = True
isAsc [] = True
