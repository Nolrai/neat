{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Neat.Internals
  ( addConnection,
    splitConnection,
    mutateWeights,
    crossover,
    mkNewGeneration,
  )
where

import Control.Lens as Lens
import Control.Monad.Random as R
import Data.Hashable
import Data.IntMap.Strict as IM
import Data.List as L
import Data.Maybe (catMaybes)
import Extras.Random
import Linear.Metric as LM
import Linear.V2
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
    getRandomNodeInt = choice $ IM.keys (parent ^. nodes)
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
  Genotype ->
  Genotype ->
  Rand g Genotype
crossover = crossover' (V2 1 1) (V2 1 0) (V2 0 1)

crossover' ::
  forall g.
  (RandomGen g) =>
  Odds ->
  Odds ->
  Odds ->
  Genotype ->
  Genotype ->
  Rand g Genotype
crossover' fitterOdds fitterOnlyOdds lessFitOnlyOdds fitter lessFit =
  do
    newNodes <- (mkNewNodes `on` (^. nodes)) fitter lessFit
    newConnections <- (mkNewConnections `on` (^. connections)) fitter lessFit
    pure $ Genotype newNodes newConnections
  where
    -- include all nodes that are in at least one parent
    mkNewNodes ::
      IntMap NodeType -> IntMap NodeType -> Rand g (IntMap NodeType)
    mkNewNodes = crossoverIntMap fitterOdds (V2 1 0) (V2 1 0)
    mkNewConnections :: IntMap Gene -> IntMap Gene -> Rand g (IntMap Gene)
    mkNewConnections =
      crossoverIntMap fitterOdds fitterOnlyOdds lessFitOnlyOdds

type Odds = V2 Rational

swapXY (V2 x y) = V2 y x

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
    (fromOdds (swapXY fitterOnlyOdds) Nothing . Just)
    (fromOdds fitterOdds)
    (fromOdds (swapXY lessFitOnlyOdds) Nothing . Just)

fromOdds ::
  forall g t.
  (RandomGen g) =>
  Odds ->
  t ->
  t ->
  Rand g t
fromOdds (V2 aOdds bOdds) a b = R.fromList [(a, aOdds), (b, bOdds)]

slowMerge ::
  forall l r t m.
  Monad m =>
  (l -> m (Maybe t)) ->
  (l -> r -> m t) ->
  (r -> m (Maybe t)) ->
  IntMap l ->
  IntMap r ->
  m (IntMap t)
slowMerge leftOnly bothSides rightOnly leftMap rightMap =
  fmap (IM.mapMaybe id)
    . R.mapM fromOrBoth
    $ toOrBoth leftMap rightMap
  where
    fromOrBoth :: OrBoth l r -> m (Maybe t)
    fromOrBoth = orBoth leftOnly bothSides' rightOnly
    bothSides' :: l -> r -> m (Maybe t)
    bothSides' l r = Just <$> bothSides l r
    toOrBoth :: IntMap a -> IntMap b -> IntMap (OrBoth a b)
    toOrBoth = mergeWithKey (\_ l r -> Just $ Both l r) (fmap LeftOnly) (fmap RightOnly)

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

splitIntoSpecies :: Double -> [Genotype] -> [[Genotype]]
splitIntoSpecies speciesRadius = fmap snd . L.foldr insertIntoSpecies []
  where
    insertIntoSpecies ::
      Genotype ->
      [(Genotype, [Genotype])] ->
      [(Genotype, [Genotype])]
    insertIntoSpecies x [] = [(x, [x])]
    insertIntoSpecies x ((y, ys) : yss) =
      if delta x y < speciesRadius
        then (y, x : ys) : yss
        else (y, ys) : insertIntoSpecies x yss

binFitness :: (Genotype -> m Double) -> [Genotype] -> m (Double, [(Double, Genotype)])
binFitness f bin =
  do
    withFitness <- bin
      `forM` \genotype ->
        do
          fitness <- f genotype
          pure (fitness, genotype)
    let meanFitness = L.sum (fst <$> withFitness) / fromIntegral (L.length bin)
    pure (meanFitness, withFitness)

normalizeFitness bins = fmap (first (/ total)) bins
  where
    total = L.sum $ fst <$> bins

shrinkToFit :: (Int, [(Double, Genotype)]) -> [Genotype]
shrinkToFit (size, parents) = snd <$> take size (sort parents)

portionSexual :: Rational
portionSexual = 0.75

interSpeciesMatingRate :: Rational
interSpeciesMatingRate = 0.001

newNodeRate :: Rational
newNodeRate = 0.03

newConnectionRate :: Rational
newConnectionRate = 0.05

perturbGenotypeRate :: Rational
perturbGenotypeRate = 0.8

newRandomWeightRate :: Rational
newRandomWeightRate = 0.1

deltaThreshould :: Double
deltaThreshould = 3.0

rateToOdds :: Rational -> V2 Rational
rateToOdds r = V2 r (1 - r)

mutate :: RandomGen g => Genotype -> Rand g Genotype
mutate g =
  do
    f <-
      fromOdds
        (rateToOdds perturbGenotypeRate)
        perturbe
        pure
    connections (mapM f) g
  where
    perturbe :: Gene -> Rand g Gene
    perturbe g =
      do
        f <- fromOdds (rateToOdds newRandomWeightRate) set addTo
        flip (f weight) g <$> getRandomR (-1, 1)
      where
        addTo :: Num a => ASetter' s a -> a -> s -> s
        addTo lens new = lens %~ (+ new)

mkBinKids :: RandomGen g => (Int, [Genotype]) -> Rand g [Genotype]
mkBinKids (popsize :: Int, parents@(champion : _)) =
  do
    let numAsexual = floor $ fromIntegral popsize * (1 - portionSexual)
    asexualParents <- take numAsexual . cycle <$> shuffle parents
    asexualKids <- mutate `mapM` asexualParents
    -- minus 1 for the champion
    let numSexual = popsize - numAsexual - (1 :: Int)
    let sexualParents = take numSexual . cycle $ parents
    sexualKids <- ( (`splitAt` sexualParents)
                      <$> [0 .. numSexual - 1]
                    )
      `forM` \case
        ([], _) -> pure Nothing
        (_, []) -> pure Nothing
        (before, after) ->
          do
            moreFit <- choice before
            lessFit <- choice after
            Just <$> crossover moreFit lessFit
    pure $ champion : asexualKids ++ catMaybes sexualKids

mkNewGeneration ::
  forall g.
  RandomGen g =>
  (Genotype -> IO Double) ->
  Int ->
  [Genotype] ->
  RandT g IO [Genotype]
mkNewGeneration toFitness totalPopSize oldGeneration =
  do
    let rawBins :: [[Genotype]] =
          splitIntoSpecies deltaThreshould oldGeneration
    (binsWithFitness :: [(Double, [Genotype])]) <-
      normalizeFitness <$> lift (binFitness toFitness <$> rawBins)
    let scaledBins :: [(Int, [(Double, Genotype)])] =
          first (floor . (* totalPopSize)) <$> binsWithFitness
    let resizedBins = shrinkToFit <$> scaledBins
    L.concat <$> (mkBinKids <$> resizedBins)
