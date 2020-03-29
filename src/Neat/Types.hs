{-# LANGUAGE TemplateHaskell #-}

module Neat.Types where

import Control.Lens
import Data.Vector as V

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
      { _nodes :: Vector NodeType,
        _connections :: IntMap Gene
      }

makeLenses ''Genotype

makeLenses ''Gene
