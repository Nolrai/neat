{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Neat.Types
  ( NodeType (..),
    Gene (..),
    Genotype (..),
    connections,
    nodes,
    inNode,
    outNode,
    weight,
    enabled,
    mutateWeights,
    toDoubleWeight,
    toInt8Weight,
    delta,
  )
where

import Control.Lens as Lens
import Data.IntMap.Strict as IM
import Linear.Metric as LM

data NodeType = Input | Output | Hidden
  deriving stock (Show, Read, Enum, Eq, Ord, Generic)

-- small int for starting nodes, hash for later ones
data Gene
  = Gene
      { _inNode :: !Int,
        _outNode :: !Int,
        _weight :: !Int8, -- minBound means -maxWeight, maxBound means +maxWeight
        _enabled :: !Bool
      }
  deriving stock (Show, Read, Eq, Ord, Generic)

data Genotype
  = Genotype
      { _nodes :: IntMap NodeType,
        _connections :: IntMap Gene
      }
  deriving stock (Show, Read, Eq, Ord)

makeLenses ''Genotype

makeLenses ''Gene

makeLenses ''NodeType

mutateWeights :: Monad m => (Int8 -> m Int8) -> Genotype -> m Genotype
mutateWeights = Lens.mapMOf (connections . each . weight)

-- a distance normalized over the number of genes.
delta :: Genotype -> Genotype -> Double
delta l r =
  sqrt $
    (rawDelta / max 1 numGenes) ^ 2
      + (keyDiff / max 1 numGenes) ^ 2
      + (nodeDiff / max 1 maxNodes) ^ 2
  where
    onNodes f = fromIntegral $ (f `on` (length . view nodes)) l r
    nodeDiff = onNodes ((abs .) . (-))
    maxNodes = onNodes max
    keyDiff :: Double
    keyDiff = fromIntegral $ sizeOnF union - sizeOnF intersection
    sizeOnF :: (forall a. IntMap a -> IntMap a -> IntMap a) -> Int
    sizeOnF f = ((IM.size .) . f `on` weights) l r
    weights :: Genotype -> IntMap Double
    weights g = toDoubleWeight . (^. weight) <$> g ^. connections
    rawDelta :: Double
    rawDelta = (LM.distance `on` weights) l r
    numGenes :: Double
    --TODO try (+) instead of max
    numGenes = fromIntegral $ (max `on` (IM.size . view connections)) l r

toDoubleWeight :: Int8 -> Double
toDoubleWeight x = (fromIntegral x / fromIntegral (maxBound :: Int8)) * maxWeight

toInt8Weight :: Double -> Int8
toInt8Weight x = floor ((x / maxWeight) * fromIntegral (maxBound :: Int8))

-- weights range from -maxWeight to +maxWeight
maxWeight :: Double
maxWeight = 3.0
