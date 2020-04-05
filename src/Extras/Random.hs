{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module       : Control.Monad.Random.Extras
-- Copyright    : 2010 Aristid Breitkreuz
-- License      : BSD3
-- Stability    : experimental
-- Portability  : portable
--
-- Additional monadic random functions, based on 'MonadRandom'.
module Extras.Random
  ( -- * Random functions

    -- ** Shuffling
    shuffle,
    shuffleSeq,

    -- ** Sampling
    sample,
    sampleSeq,

    -- ** Choice
    choiceExtract,
    choiceExtractSeq,
    choice,
    choiceSeq,
    choiceArray,

    -- ** Choices
    choices,
    choicesArray,
  )
where

import Control.Monad.Random (MonadRandom, getRandomR, getRandomRs)
import qualified Data.Array
import qualified Data.Array.IArray as Arr
import Data.Array.IArray ((!))
import Data.List ((!!))
import Data.Maybe (fromJust)
import qualified Data.Sequence as Seq
import Data.Sequence ((><), ViewL ((:<)))
import System.Random (Random)

(.:) :: (c -> c') -> (a -> b -> c) -> (a -> b -> c')
(.:) = (.) . (.)

extract :: [a] -> Int -> Maybe ([a], a)
extract s i =
  case r of
    [] -> Nothing
    (b : c) -> Just (a ++ c, b)
  where
    (a, r) = splitAt i s

extractSeq :: Seq.Seq a -> Int -> Maybe (Seq.Seq a, a)
extractSeq s i =
  case Seq.viewl r of
    Seq.EmptyL -> Nothing
    (b :< c) -> Just (a >< c, b)
  where
    (a, r) = Seq.splitAt i s

getRandomR' :: (MonadRandom m, Random a) => a -> a -> m a
getRandomR' = curry getRandomR

getRandomRNums :: (MonadRandom m, Random a, Num a) => [a] -> m [a]
getRandomRNums = mapM (getRandomR' 0)

backsaw :: Int -> [Int]
backsaw n = [n - 1, n - 2 .. 0]

-- Shuffling

-- | Shuffle a list randomly. The method is based on Oleg Kiselyov's
-- /perfect shuffle/ <http://okmij.org/ftp/Haskell/perfect-shuffle.txt>,
-- but much simpler because it uses existing data structures. The efficiency
-- of both methods should be comparable.
--
-- Complexity: O(n * log n)
shuffle :: (MonadRandom m) => [a] -> m [a]
shuffle = shuffleSeq . Seq.fromList

-- | Shuffle a sequence randomly. This is being used by 'shuffle',
-- so it logically uses the same method.
--
-- Complexity: O(n * log n)
shuffleSeq :: (MonadRandom m) => Seq.Seq a -> m [a]
shuffleSeq s = do
  samples <- getRandomRNums . backsaw $ Seq.length s
  return (shuffleSeq' s samples)

shuffleSeq' :: Seq.Seq a -> [Int] -> [a]
shuffleSeq' = snd .: mapAccumL (fromJust .: extractSeq)

-- Sampling

-- | Take a random sample from a list.
--
-- Complexity: O(n + m * log n)
sample :: (MonadRandom m) => Int -> [a] -> m [a]
sample m = sampleSeq m . Seq.fromList

-- | Take a random sample from a sequence.
--
-- Complexity: O(m * log n)
sampleSeq :: (MonadRandom m) => Int -> Seq.Seq a -> m [a]
sampleSeq m s = do
  samples <- getRandomRNums . take m . backsaw $ Seq.length s
  return (shuffleSeq' s samples)

-- Choice

-- | Randomly choose and extract an element from a list.
--
-- Complexity: O(n)
choiceExtract :: (MonadRandom m) => [a] -> m (Maybe ([a], a))
choiceExtract [] = return Nothing
choiceExtract xs = extract xs <$> getRandomR (0, length xs - 1)

-- | Randomly choose and extract an element from a sequence.
--
-- Complexity: O(log n)
choiceExtractSeq :: (MonadRandom m) => Seq.Seq a -> m (Maybe (Seq.Seq a, a))
choiceExtractSeq s
  | Seq.null s = return Nothing
  | otherwise = extractSeq s <$> getRandomR (0, Seq.length s - 1)

-- | Select a random element from a list.
--
-- Complexity: O(n).
choice :: (MonadRandom m) => [a] -> m a
choice [] = error "Control.Monad.Random.Extras.choice: empty list"
choice xs = (xs !!) <$> getRandomR (0, length xs - 1)

-- | Select a random element from a sequence.
--
-- Complexity: O(log n).
choiceSeq :: (MonadRandom m) => Seq.Seq a -> m a
choiceSeq s
  | Seq.null s = error "Control.Monad.Random.Extras.choiceSeq: empty sequence"
  | otherwise = Seq.index s <$> getRandomR (0, Seq.length s - 1)

-- | Select a random element from an array.
--
-- Complexity: O(1).
choiceArray :: (MonadRandom m, Arr.IArray arr a, Arr.Ix i, Random i) => arr i a -> m a
choiceArray v = (v !) <$> getRandomR (Arr.bounds v)

-- Choices

-- | A stream of random elements from a list.
--
-- Complexity: O(n) base and O(1) per element
choices :: (MonadRandom m) => [a] -> m [a]
choices xs = choicesArray $ Data.Array.listArray (1, length xs) xs

-- | A stream of random elements from an array.
--
-- Complexity: O(1) per element
choicesArray :: (MonadRandom m, Arr.IArray arr a, Arr.Ix i, Random i) => arr i a -> m [a]
choicesArray v = map (v !) <$> getRandomRs (Arr.bounds v)
