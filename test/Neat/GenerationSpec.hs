{-# LANGUAGE BangPatterns #-}
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
    selectPairs,
    splitConnection,
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
      numConnections <- choose (0, s * numConnections')
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
          \x (NonNegative n) ->
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
    describe "splitConnection"
      $ it "doesn't throw" . property
      $ \(x :: Genotype) ->
        flip shouldReturn () $
          do
            (101, end) <-
              sampleRVar . flip execStateT (1 :: Int, x)
                $ replicateM_ 100
                $ do
                  (generation, !y) <- get
                  y' <- lift $ splitConnection generation y
                  put (generation + 1, y')
            writeFile "test.out" (show end <> "\n")
    describe "selectPairs"
      $ it "doesn't throw" . property
      $ \(Positive (m :: Int)) (Positive (n :: Int)) ->
        do
          pairs <- sampleRVar $ replicateM 100 $ selectPairs [0 .. m] n
          pairs
            `shouldSatisfy` all
              ( \subList ->
                  (length subList == n)
                    && all
                      (\(a, b) -> a < b && b <= m && a >= 0)
                      subList
              )
