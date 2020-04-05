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
  )
where

import Control.Lens

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
