{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Neat.GenerationSpec
  ( spec,
  )
where

import Control.Lens
import Data.IntMap as IM
import Data.List as List
-- import Data.List as L
import Data.RVar
import Data.Random ()
import Data.Random.Distribution.Uniform (uniform)
import Data.Random.Source (monadRandom)
-- crossover,
-- mkNewGeneration,
-- rvar,
-- splitConnection,
-- toInnovationHash,

import qualified Data.Vector as V
import Neat.Generation as G
  ( addConnection,
  )
import Neat.Types as T
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic as Q

$( monadRandom
     [d|
       instance MonadRandom Gen where
         getRandomWord8 = choose (0, maxBound)
         getRandomWord16 = choose (0, maxBound)
         getRandomWord32 = choose (0, maxBound)
         getRandomWord64 = choose (0, maxBound)
       |]
 )

instance Arbitrary Genotype where
  arbitrary =
    do
      s <- max 1 . (`div` 5) <$> getSize
      numInput <- choose (1, s)
      numOutput <- choose (1, s)
      numHidden <- choose (0, s * s)
      let _nodes =
            IM.fromAscList $
              zip
                [0 ..]
                ( concat $
                    uncurry List.replicate
                      <$> [(numInput, Input), (numOutput, Output), (numHidden, Hidden)]
                )
      let numConnections' = numOutput + numHidden
      numConnections <- choose (0, 2 * numConnections')
      let _connections = mempty
      sampleRVar $ addConnections numConnections Genotype {..}
  shrink g =
    List.drop 1 $
      do
        let entryVector1 = V.fromList . toAscList $ g ^. connections
        let onEntry ordIndex (hashIndex, gene) =
              (,) <$> [hashIndex, ordIndex] <*> shrink gene
        entryVector2 <- onEntry `V.imapM` entryVector1
        pure $ set connections (IM.fromList $ V.toList entryVector2) g

addConnections :: Int -> Genotype -> RVar Genotype
addConnections numConnections start =
  foldlM f start [0 .. numConnections - 1]
  where
    f :: Genotype -> Int -> RVar Genotype
    f genotype generation =
      do
        w <- uniform (-1) 1
        addConnection generation w genotype

instance Arbitrary Gene where
  arbitrary =
    Gene <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  shrink = recursivelyShrink

spec :: Spec
spec =
  do
    describe "addConnection" $
      do
        xit "stub for hlint" . property $ \b -> b && b `shouldBe` b
        it "non-stationary" . property $
          \x n ->
            monadicIO . run . sampleRVar $
              do
                w <- uniform (-1) 1
                y <- addConnection (n + 1) w x
                pure $ y `shouldNotBe` x
    describe "addConnections" $
      do
        xit "stub for hlint" . property $ \b -> b && b `shouldBe` b
        it "non-stationary" . property $
          \x (NonNegative n) ->
            monadicIO $
              do
                y <- run . sampleRVar $ addConnections (n + 1) x
                pure (y `shouldNotBe` x)
    describe "delta" $
      do
        it "is non-negative" . property $
          \x y -> delta x y `shouldSatisfy` (>= 0)
        it " is anti-reflexive" . property $
          \x -> delta x x `shouldBe` 0
        it " is discrimitive" . property $
          \x y -> x /= y ==> delta x y `shouldNotBe` 0
        it " is triangular" . property $
          \x y z ->
            (delta x y, delta y z, delta x z)
              `shouldSatisfy` (\(xy, yz, xz) -> xy + yz >= xz)
