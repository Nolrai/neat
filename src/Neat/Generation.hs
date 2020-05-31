{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Neat.Generation
  ( addConnection,
    splitConnection,
    splitIntoBins,
    crossover,
    mkNewGeneration,
    rvar,
    toInnovationHash,
    delta,
    selectPairs,
  )
where

import Control.Exception (IOException, assert, catch)
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
    randomNode =
      fromMaybe (error "no nodes!") . safeChoice $
        IM.keys (parent ^. nodes)
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
  HasCallStack =>
  Int ->
  Genotype ->
  RVar Genotype
splitConnection generation parent =
  case maybeGetRandomConnectionKey of
    Nothing -> pure parent
    Just getRandomConnectionKey ->
      do
        key <- getRandomConnectionKey
        let oldConnection' = IM.lookup key oldConnections
        let errmsg =
              ("Generation: " <> show generation)
                <> ("key: " <> show key)
                <> ("\nkeys: \n" <> show keys')
                <> ("\nkey `L.elem` keys: " <> show (key `L.elem` keys'))
                <> ("\noldConnections: \n" <> show oldConnections)
                <> ("\noldConnection': " <> show oldConnection')
                <> ("\noldConnection': " <> show (oldConnections IM.!? key))
        let oldConnection = fromMaybe (error errmsg) oldConnection'
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
                  (key, oldConnection & enabled .~ False)
                ]
        let newNodes =
              (parent ^. nodes)
                `IM.union` IM.fromAscList
                  ( (\l -> assert (isAsc l) l)
                      [(toHash oldConnection, Hidden)]
                  )
        -- union is left biased, so oldConnection gets over writen.
        pure
          . Genotype newNodes
          $ newConnections `IM.union` (parent ^. connections)
  where
    -- let t = frontHalf `seq` trace (toString errmsg)
    -- pure $
    --   t `seq` parent

    toHashPair gene = (toHash gene, gene)
    -- these down here don't rely on the value of "key".
    toHash :: Gene -> Int
    toHash = toInnovationHash generation
    oldConnections :: IntMap Gene
    oldConnections = fst . L.head . reads . show $ parent ^. connections
    maybeGetRandomConnectionKey :: Maybe (RVar Key)
    maybeGetRandomConnectionKey = safeChoice keys'
    keys' :: [Key]
    keys' = keys oldConnections

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
splitIntoSpecies = splitIntoBins delta

splitIntoBins :: forall a. (a -> a -> Double) -> Double -> [a] -> [[a]]
splitIntoBins delta' radius' = fmap snd . L.foldr insertIntoBins []
  where
    insertIntoBins ::
      a ->
      [(a, [a])] ->
      [(a, [a])]
    insertIntoBins x [] = [(x, [x])]
    insertIntoBins x ((y, ys) : yss) =
      if delta' x y < radius'
        then (y, x : ys) : yss
        else (y, ys) : insertIntoBins x yss

binFitness ::
  HasCallStack =>
  (Genotype -> IO Double) ->
  [Genotype] ->
  IO (Double, [(Double, Genotype)])
binFitness f bin =
  do
    withFitness <- annotate getFitness `mapM` bin
    annotate (pure . meanFitness) withFitness
  where
    getFitness :: Genotype -> IO Double
    getFitness genotype = (f genotype >>= evaluateNF) `catch` (\e -> reportError e >> pure (- (10 ^ 8)))

annotate :: (a -> IO b) -> a -> IO (b, a)
annotate f x = (,x) <$> f x

meanFitness :: [(Double, Genotype)] -> Double
meanFitness individualFitnesses = L.sum (fst <$> individualFitnesses) / fromIntegral (L.length individualFitnesses)

reportError :: SomeException -> IO ()
reportError ex = print ex >> (putTextLn . toText) (prettyCallStack callStack)

normalizeFitness :: forall a. (HasCallStack, Show a) => [(Double, a)] -> [(Double, a)]
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
deltaThreshould = 1

defaultWeight :: RVar Int8
defaultWeight = uniform (toInt8Weight (-1)) (toInt8Weight 1)

mutate :: Int -> Genotype -> RVar Genotype
mutate generation =
  step1 >=> step2 >=> step3
  where
    step1, step2, step3 :: Genotype -> RVar Genotype
    step1 = atRate perturbGenotypeRate (mutateWeights perturb)
    step2 = atRate addConnectionRate addConnection'
    step3 = atRate newNodeRate (splitConnection generation)
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
selectPairs :: (HasCallStack, Show t) => [t] -> Int -> RVar [(t, t)]
selectPairs l n =
  do
    ixs <- replicateM n rIndex
    jxs <- replicateM n rIndex
    zip ixs jxs
      `forM` \(ix', jx') ->
        do
          let ix = min ix' jx'
          let jx = 1 + max ix' jx'
          if ix >= length l
            then error $ errmsg ix jx
            else
              pure
                ( l !! ix,
                  if jx >= length l
                    then l !! ix
                    else l !! jx
                )
  where
    rIndex :: RVar Int
    rIndex = uniform 0 (max 0 (length l - 2))
    errmsg ix jx =
      "ix: " <> show ix <> " jx: " <> show jx
        <> "\nl: "
        <> show l

mkBinKids ::
  Int ->
  [Genotype] ->
  (Int, [Genotype]) ->
  RVar [Genotype]
mkBinKids _ _ (0, _) = pure []
mkBinKids _ _ (n, []) = error $ "Empty bin when popsize = " <> show n
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
        (fromMaybe (error "empty allGenomes") $ safeChoice allGenomes)
        (pure b)
    b' <- mkB
    pure (a, b')

mkNewGeneration ::
  forall m.
  (HasCallStack, MonadIO m, MonadRandom m, MonadFail m) =>
  (Genotype -> IO Double) ->
  Int ->
  Int ->
  [Genotype] ->
  m [Genotype]
mkNewGeneration toFitness totalPopSize generationNumber oldGeneration =
  do
    rawBins <- evaluateNF $ splitIntoSpecies deltaThreshould oldGeneration
    liftIO . putTextLn $ "num species = " <> show (length rawBins)
    liftIO . putTextLn $ "raw sizes = " <> show (L.map length rawBins)
    binsWithFitness <-
      evaluateNF =<< normalizeFitness <$> liftIO (binFitness toFitness `mapM` rawBins)
    liftIO . putTextLn $ "Fitness = " <> show (fst <$> binsWithFitness)
    scaledBins <- evaluateNF $ first (max 0 . floor . (* fromIntegral totalPopSize)) <$> binsWithFitness
    liftIO . putTextLn $ "sizes = " <> show (fst <$> scaledBins, length . snd <$> scaledBins)
    resizedBins <- evaluateNF $ shrinkToFit <$> scaledBins
    evaluateNF
      =<< L.concat
      <$> ( mkBinKids'
              `mapM` resizedBins
          )
  where
    mkBinKids' :: (Int, [Genotype]) -> m [Genotype]
    mkBinKids' before =
      sampleRVar $
        mkBinKids generationNumber oldGeneration before
