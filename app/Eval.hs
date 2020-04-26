{-# LANGUAGE RecordWildCards #-}

module Eval
  ( evalXor,
    starterXor,
  )
where

-- import Grenade

import Data.Hashable
import Data.Hashable.Generic
import Data.IntMap as IM
import Neat

evalXor :: Genotype -> IO Double
evalXor Genotype {..} =
  pure 1

firstIntNotInDouble :: Int
firstIntNotInDouble = 16777217

instance Hashable a => Hashable (IntMap a) where
  hashWithSalt s m = IM.foldr (flip hashWithSalt) (hashWithSalt s $ IM.size m) m

instance Hashable Gene where
  hashWithSalt = genericHashWithSalt

instance Hashable NodeType where
  hashWithSalt = genericHashWithSalt

starterXor :: Genotype
starterXor =
  Genotype
    { _nodes =
        IM.fromAscList $ zip [0 ..] (replicate 8 Input <> replicate 8 Output),
      _connections = mempty
    }
