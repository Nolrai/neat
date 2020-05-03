{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Neat.Generation
  ( addConnection,
    splitConnection,
    crossover,
    mkNewGeneration,
    rvar,
    toInnovationHash,
    delta,
  )
where

import Control.Lens as Lens hiding (ix)
import Data.Hashable
import Data.IntMap.Strict as IM
import Data.List as L
import Data.RVar
import Data.Random.Distribution
import Data.Random.Distribution.Normal (normal)
import Data.Random.Distribution.Uniform (uniform)
import Data.Random.Extras
import Linear.V2
import Neat.Types
import Neat.Utils

-- the paper uses a counter + a list of this generations new connections,
-- we a hash instead.
toInnovationHash :: Int -> Gene -> Int
toInnovationHash generation gene =
  hash (generation, gene ^. inNode, gene ^. outNode)

addConnection ::
  Int ->
  Int8 ->
  Genotype ->
  RVar Genotype
addConnection generation newWeight parent =
  Genotype (parent ^. nodes) <$> newConnections
  where
    randomNode :: RVar Int
    randomNode = choice $ IM.keys (parent ^. nodes)
    newConnections :: RVar (IntMap Gene)
    newConnections =
      do
        new <- newConnection
        pure $ IM.insert (toInnovationHash generation new) new (parent ^. connections)
    newConnection :: RVar Gene
    newConnection =
      do
        newInNode <- randomNode
        newOutNode <- randomNode
        pure Gene {_inNode = newInNode, _outNode = newOutNode, _enabled = True, _weight = newWeight}

splitConnection ::
  Int ->
  NodeType ->
  Genotype ->
  RVar Genotype
splitConnection generation nodeType parent =
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
    getRandomConnectionKey :: RVar Key
    getRandomConnectionKey = choice $ keys (parent ^. connections)

crossover ::
  Genotype ->
  Genotype ->
  RVar Genotype
crossover = crossover' (V2 1 1) (V2 1 0) (V2 0 1)

crossover' ::
  Odds ->
  Odds ->
  Odds ->
  Genotype ->
  Genotype ->
  RVar Genotype
crossover' fitterOdds fitterOnlyOdds lessFitOnlyOdds fitter lessFit =
  do
    newNodes <- (mkNewNodes `on` (^. nodes)) fitter lessFit
    newConnections <- (mkNewConnections `on` (^. connections)) fitter lessFit
    pure $ Genotype newNodes newConnections
  where
    -- include all nodes that are in at least one parent
    mkNewNodes ::
      IntMap NodeType -> IntMap NodeType -> RVar (IntMap NodeType)
    mkNewNodes = crossoverIntMap fitterOdds (V2 1 0) (V2 1 0)
    mkNewConnections :: IntMap Gene -> IntMap Gene -> RVar (IntMap Gene)
    mkNewConnections =
      crossoverIntMap fitterOdds fitterOnlyOdds lessFitOnlyOdds

crossoverIntMap ::
  Odds ->
  Odds ->
  Odds ->
  IntMap a ->
  IntMap a ->
  RVar (IntMap a)
crossoverIntMap fitterOdds fitterOnlyOdds lessFitOnlyOdds =
  slowMerge
    (fromOdds (swapXY fitterOnlyOdds) Nothing . Just)
    (fromOdds fitterOdds)
    (fromOdds (swapXY lessFitOnlyOdds) Nothing . Just)

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
    . mapM fromOrBoth
    $ toOrBoth leftMap rightMap
  where
    fromOrBoth :: OrBoth l r -> m (Maybe t)
    fromOrBoth = orBoth leftOnly bothSides' rightOnly
    bothSides' :: l -> r -> m (Maybe t)
    bothSides' l r = Just <$> bothSides l r
    toOrBoth :: IntMap a -> IntMap b -> IntMap (OrBoth a b)
    toOrBoth = mergeWithKey (\_ l r -> Just $ Both l r) (fmap LeftOnly) (fmap RightOnly)

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

binFitness ::
  Monad m =>
  (Genotype -> m Double) ->
  [Genotype] ->
  m (Double, [(Double, Genotype)])
binFitness f bin =
  do
    withFitness <- bin
      `forM` \genotype ->
        do
          fitness <- f genotype
          pure (fitness, genotype)
    let meanFitness = L.sum (fst <$> withFitness) / fromIntegral (L.length bin)
    pure (meanFitness, withFitness)

normalizeFitness :: [(Double, a)] -> [(Double, a)]
normalizeFitness bins = fmap (first (/ total)) bins
  where
    total = L.sum $ fst <$> bins

shrinkToFit :: (Int, [(Double, Genotype)]) -> (Int, [Genotype])
shrinkToFit (newSize, parents) = (newSize, culled)
  where
    culled = snd <$> take newSize (sortOn fst parents)

portionSexual :: Rational
portionSexual = 0.75

interSpeciesMatingRate :: Rational
interSpeciesMatingRate = 0.001

interSpeciesMatingOdds :: Odds
interSpeciesMatingOdds = rateToOdds interSpeciesMatingRate

newNodeRate :: Rational
newNodeRate = 0.03

addConnectionRate :: Rational
addConnectionRate = 0.05

perturbGenotypeRate :: Rational
perturbGenotypeRate = 0.8

newRandomWeightRate :: Rational
newRandomWeightRate = 0.1

deltaThreshould :: Double
deltaThreshould = 3.0

defaultWeight :: RVar Int8
defaultWeight = uniform (toInt8Weight (-1)) (toInt8Weight 1)

mutate :: Int -> Genotype -> RVar Genotype
mutate generation =
  step1 >=> step2 >=> step3
  where
    step1, step2, step3 :: Genotype -> RVar Genotype
    step1 = atRate perturbGenotypeRate (mutateWeights perturb)
    step2 = atRate addConnectionRate addConnection'
    step3 = atRate newNodeRate (splitConnection generation Hidden)
    perturb :: Int8 -> RVar Int8
    perturb old =
      do
        adjustment <- toInt8Weight <$> normal 0 1
        replacement <- defaultWeight
        fromOdds
          (rateToOdds newRandomWeightRate)
          replacement
          (adjustment + old)
    addConnection' genotype =
      do
        w <- defaultWeight
        addConnection generation w genotype

-- randomly slects pairs (a,b) such that a is earliar in the list than b.
selectPairs :: [t] -> Int -> RVar [(t, t)]
selectPairs l n =
  do
    ixs <- replicateM n rIndex
    jxs <- replicateM n rIndex
    zip ixs jxs
      `forM` \(ix', jx') ->
        do
          let ix = min ix' jx'
          let jx = 1 + max ix' jx'
          pure (l !! ix, l !! jx)
  where
    rIndex :: RVar Int
    rIndex = uniform 0 (length l - 2)

mkBinKids ::
  Int ->
  [Genotype] ->
  (Int, [Genotype]) ->
  RVar [Genotype]
mkBinKids _ _ (_, []) = error "Empty bin"
mkBinKids generation allGenomes (popsize :: Int, parents@(champion : _)) =
  do
    let numAsexual = floor $ fromIntegral popsize * (1 - portionSexual)
    asexualParents <- take numAsexual . cycle <$> shuffle parents
    asexualKids <- mutate generation `mapM` asexualParents
    -- minus 1 for the champion
    let numSexual = popsize - numAsexual - (1 :: Int)
    (sexualParents :: [(Genotype, Genotype)]) <-
      (interSpeciesMating allGenomes `mapM`)
        =<< selectPairs parents numSexual
    sexualKids <- uncurry crossover `mapM` sexualParents
    pure $ champion : asexualKids ++ sexualKids

interSpeciesMating ::
  [Genotype] ->
  (Genotype, Genotype) ->
  RVar (Genotype, Genotype)
interSpeciesMating allGenomes (a, b) =
  do
    mkB <-
      fromOdds
        interSpeciesMatingOdds
        (choice allGenomes)
        (pure b)
    b' <- mkB
    pure (a, b')

mkNewGeneration ::
  forall m.
  (MonadIO m, MonadRandom m) =>
  (Genotype -> IO Double) ->
  Int ->
  Int ->
  [Genotype] ->
  m [Genotype]
mkNewGeneration toFitness totalPopSize generationNumber oldGeneration =
  do
    let rawBins :: [[Genotype]] =
          splitIntoSpecies deltaThreshould oldGeneration
    liftIO . putTextLn $ "num species = " <> show (length rawBins)
    (binsWithFitness :: [(Double, [(Double, Genotype)])]) <-
      normalizeFitness <$> liftIO (binFitness toFitness `mapM` rawBins)
    liftIO . putTextLn $ "Fitness = " <> show (fst <$> binsWithFitness)
    let scaledBins :: [(Int, [(Double, Genotype)])] =
          first (floor . (* fromIntegral totalPopSize)) <$> binsWithFitness
    liftIO . putTextLn $ "sizes = " <> show (fst <$> scaledBins)
    let resizedBins = shrinkToFit <$> scaledBins
    L.concat
      <$> ( mkBinKids'
              `mapM` resizedBins
          )
  where
    mkBinKids' :: (Int, [Genotype]) -> m [Genotype]
    mkBinKids' before =
      do
        after <-
          sampleRVar $
            mkBinKids generationNumber oldGeneration before
        putTextLn $ "before: " <> show before
        putTextLn $ "after: " <> show after
        return after
