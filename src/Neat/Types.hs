{-# LANGUAGE DerivingStrategies #-}
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
    delta,
  )
where

import Control.Lens as Lens
import Data.IntMap.Strict as IM
import Linear.Metric as LM

data NodeType = Sensor | Output | Hidden
  deriving stock (Show, Read, Enum, Eq, Ord)

data Gene
  = Gene
      { _inNode :: Int,
        _outNode :: Int,
        _weight :: Double,
        _enabled :: Bool
      }
  deriving stock (Show, Read, Eq, Ord)

data Genotype
  = Genotype
      { _nodes :: IntMap NodeType,
        _connections :: IntMap Gene
      }
  deriving stock (Show, Read, Eq, Ord)

makeLenses ''Genotype

makeLenses ''Gene

makeLenses ''NodeType

mutateWeights :: Monad m => (Double -> m Double) -> Genotype -> m Genotype
mutateWeights = Lens.mapMOf (connections . each . weight)

-- a distance normalized over the number of genes.
delta :: Genotype -> Genotype -> Double
delta l r = rawDelta / fromIntegral numGenes
  where
    weights :: Genotype -> IntMap Double
    weights g = (^. weight) <$> g ^. connections
    rawDelta :: Double
    rawDelta = (LM.distance `on` weights) l r
    numGenes :: Int
    --TODO try (+) instead of max
    numGenes = (max `on` (IM.size . view connections)) l r
