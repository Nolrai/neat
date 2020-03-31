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

data Gene
  = Gene
      { _inNode :: Int,
        _outNode :: Int,
        _weight :: Double,
        _enabled :: Bool
      }

data Genotype
  = Genotype
      { _nodes :: IntMap NodeType,
        _connections :: IntMap Gene
      }

makeLenses ''Genotype

makeLenses ''Gene
