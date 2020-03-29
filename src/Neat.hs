{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Neat where

import Control.Lens as Lens
import Control.Monad.Random as R
import Data.Hashable
import Data.IntMap.Strict as IM
import Data.List as L
import Data.Vector as V
import Neat.Types

mapMOfWeights_ :: Monad m => (Double -> m Double) -> Genotype -> m Genotype
mapMOfWeights_ = Lens.mapMOf (connections . each . weight)

-- the paper uses a counter + a list of this generations new connections, we Int instead.
toInnovationInt :: Int -> Gene -> Int
toInnovationInt generation gene = hash (generation, gene ^. inNode, gene ^. outNode)

addConnection :: forall g. (RandomGen g) => Int -> Genotype -> Double -> Rand g Genotype
addConnection generation parent newWeight =
  Genotype (parent ^. nodes) <$> newConnections
  where
    getRandomNodeInt :: Rand g Int
    getRandomNodeInt = getRandomR (0, V.length (parent ^. nodes) - 1)
    newConnections :: Rand g (IntMap Gene)
    newConnections =
      do
        new <- newConnection
        pure $ IM.insert (toInnovationInt generation new) new (parent ^. connections)
    newConnection :: Rand g Gene
    newConnection =
      do
        newInNode <- getRandomNodeInt
        newOutNode <- getRandomNodeInt
        pure Gene {_inNode = newInNode, _outNode = newOutNode, _enabled = True, _weight = newWeight}

splitConnection ::
  forall g. (RandomGen g) => Int -> Genotype -> NodeType -> Rand g Genotype
splitConnection generation parent nodeType =
  do
    key <- getRandomConnectionKey
    let oldConnection = (parent ^. connections) IM.! key
    let frontHalf = oldConnection & inNode .~ newNodeInt
    let backHalf =
          Gene
            { _outNode = newNodeInt,
              _enabled = True,
              _weight = 1,
              _inNode = oldConnection ^. inNode
            }
    let newConnections = IM.fromList [toKeyValuePair backHalf, (key, frontHalf)]
    pure $ Genotype newNodes (newConnections `IM.union` (parent ^. connections))
  where
    toKeyValuePair c = (toInt c, c)
    toInt = toInnovationInt generation
    newNodes :: Vector NodeType
    newNodes = nodeType `V.cons` (parent ^. nodes)
    newNodeInt :: Int
    newNodeInt = V.length (parent ^. nodes) -- is the same as `V.length newNodes - 1`
    getRandomConnectionKey :: Rand g Key
    getRandomConnectionKey = choose $ keys (parent ^. connections)
    choose :: [a] -> Rand g a
    choose = R.fromList . L.map (,1)
