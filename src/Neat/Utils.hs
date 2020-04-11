{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Neat.Utils where

import Data.RVar
import Data.Random.Distribution
import Data.Random.Distribution.Normal (normal)
import Data.Random.Distribution.Uniform (Uniform (..), uniform)
import Data.Ratio ((%))
import Linear.V2

data OrBoth l r = LeftOnly l | Both l r | RightOnly r

-- Destructor for the OrBoth data type.
orBoth :: (l -> t) -> (l -> r -> t) -> (r -> t) -> OrBoth l r -> t
orBoth onLeft _ _ (LeftOnly l) = onLeft l
orBoth _ onBoth _ (Both l r) = onBoth l r
orBoth _ _ onRight (RightOnly r) = onRight r

instance Distribution Uniform Rational where
  rvar (Uniform low high) =
    do
      let diff = high - low
      let bottom = denominator diff
      top <- uniform 0 (numerator diff)
      pure (low + top % bottom)

type Odds = V2 Rational

swapXY :: V2 a -> V2 a
swapXY (V2 x y) = V2 y x

inverseRate :: Rational -> Rational
inverseRate r = 1 - r

fromOdds ::
  Odds ->
  t ->
  t ->
  RVar t
fromOdds (V2 aOdds bOdds) a b =
  do
    x <- uniform 0 (aOdds + bOdds)
    if x < aOdds
      then pure a
      else pure b

rateToOdds :: Rational -> Odds
rateToOdds r = V2 r (1 - r)

oddsToRate :: Odds -> Rational
oddsToRate (V2 a b) = a / (a + b)

atRate :: Rational -> (x -> RVar x) -> x -> RVar x
atRate rate action x =
  do
    f <- fromOdds (rateToOdds rate) action pure
    f x
