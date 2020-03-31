{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Neat.Internals
  ( addConnection,
    splitConnection,
    mutateWeights,
    crossover,
  )
where

import Control.Lens as Lens
import Control.Monad.Random as R
import Data.Hashable
import Data.IntMap.Strict as IM
import Data.List as L
import Neat.Types

mutateWeights :: Monad m => (Double -> m Double) -> Genotype -> m Genotype
mutateWeights = Lens.mapMOf (connections . each . weight)

-- the paper uses a counter + a list of this generations new connections,
-- we a hash instead.
toInnovationHash :: Int -> Gene -> Int
toInnovationHash generation gene =
  hash (generation, gene ^. inNode, gene ^. outNode)

addConnection ::
  forall g.
  (RandomGen g) =>
  Int ->
  Genotype ->
  Double ->
  Rand g Genotype
addConnection generation parent newWeight =
  Genotype (parent ^. nodes) <$> newConnections
  where
    getRandomNodeInt :: Rand g Int
    getRandomNodeInt = R.fromList . fmap (,1) $ IM.keys (parent ^. nodes)
    newConnections :: Rand g (IntMap Gene)
    newConnections =
      do
        new <- newConnection
        pure $ IM.insert (toInnovationHash generation new) new (parent ^. connections)
    newConnection :: Rand g Gene
    newConnection =
      do
        newInNode <- getRandomNodeInt
        newOutNode <- getRandomNodeInt
        pure Gene {_inNode = newInNode, _outNode = newOutNode, _enabled = True, _weight = newWeight}

splitConnection ::
  forall g.
  (RandomGen g) =>
  Int ->
  Genotype ->
  NodeType ->
  Rand g Genotype
splitConnection generation parent nodeType =
  do
    key <- getRandomConnectionKey
    let oldConnection = (parent ^. connections) IM.! key
    let newNodeHash = toHash oldConnection
    let frontHalf = oldConnection & inNode .~ newNodeHash
    let backHalf =
          Gene
            { _outNode = newNodeHash,
              _enabled = True,
              _weight = 1,
              _inNode = oldConnection ^. inNode
            }
    let newConnections =
          IM.fromList
            [ toHashPair backHalf,
              toHashPair frontHalf,
              toHashPair (oldConnection & enabled .~ False)
            ]
    let newNodes =
          (parent ^. nodes)
            `IM.union` IM.fromAscList
              [(toHash oldConnection, nodeType)]
    -- union is left biased, so oldConnection gets over writen.
    pure
      . Genotype newNodes
      $ newConnections `IM.union` (parent ^. connections)
  where
    toHashPair gene = (toHash gene, gene)
    -- these down here don't rely on the value of "key".
    toHash :: Gene -> Int
    toHash = toInnovationHash generation
    getRandomConnectionKey :: Rand g Key
    getRandomConnectionKey = choose $ keys (parent ^. connections)
    choose :: [a] -> Rand g a
    choose = R.fromList . L.map (,1)

data OrBoth l r = LeftOnly l | Both l r | RightOnly r

orBoth :: (l -> t) -> (l -> r -> t) -> (r -> t) -> OrBoth l r -> t
orBoth onLeft _ _ (LeftOnly l) = onLeft l
orBoth _ onBoth _ (Both l r) = onBoth l r
orBoth _ _ onRight (RightOnly r) = onRight r

crossover ::
  forall g.
  (RandomGen g) =>
  (Rational, Rational) ->
  (Rational, Rational) ->
  (Rational, Rational) ->
  Genotype ->
  Genotype ->
  Rand g Genotype
crossover fitterOdds fitterOnlyOdds lessFitOnlyOdds fitter lessFit =
  do
    newNodes <- (mkNewNodes `on` (^. nodes)) fitter lessFit
    newConnections <- (mkNewConnections `on` (^. connections)) fitter lessFit
    pure $ Genotype newNodes newConnections
  where
    -- include all nodes that are in at least one parent
    mkNewNodes ::
      IntMap NodeType -> IntMap NodeType -> Rand g (IntMap NodeType)
    mkNewNodes = crossoverIntMap fitterOdds (1, 0) (1, 0)
    mkNewConnections :: IntMap Gene -> IntMap Gene -> Rand g (IntMap Gene)
    mkNewConnections =
      crossoverIntMap fitterOdds fitterOnlyOdds lessFitOnlyOdds

type Odds = (Rational, Rational)

crossoverIntMap ::
  RandomGen g =>
  Odds ->
  Odds ->
  Odds ->
  IntMap a ->
  IntMap a ->
  Rand g (IntMap a)
crossoverIntMap fitterOdds fitterOnlyOdds lessFitOnlyOdds =
  slowMerge
    (fromOdds (swap fitterOnlyOdds) Nothing . Just)
    (fromOdds fitterOdds)
    (fromOdds (swap lessFitOnlyOdds) Nothing . Just)

fromOdds ::
  forall g t.
  (RandomGen g) =>
  Odds ->
  t ->
  t ->
  Rand g t
fromOdds (aOdds, bOdds) a b = R.fromList [(a, aOdds), (b, bOdds)]

slowMerge ::
  forall t m.
  Monad m =>
  (t -> m (Maybe t)) ->
  (t -> t -> m t) ->
  (t -> m (Maybe t)) ->
  IntMap t ->
  IntMap t ->
  m (IntMap t)
slowMerge leftOnly bothSides rightOnly leftMap rightMap =
  fmap (IM.mapMaybe id)
    . R.mapM fromOrBoth
    $ toOrBoth leftMap rightMap
  where
    fromOrBoth :: OrBoth t t -> m (Maybe t)
    fromOrBoth = orBoth leftOnly bothSides' rightOnly
    bothSides' :: t -> t -> m (Maybe t)
    bothSides' l r = Just <$> bothSides l r
    toOrBoth :: IntMap a -> IntMap b -> IntMap (OrBoth a b)
    toOrBoth = mergeWithKey (\_ l r -> Just $ Both l r) (fmap LeftOnly) (fmap RightOnly)
