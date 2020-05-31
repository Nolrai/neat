{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Neat.Run where

import Control.Lens
import Data.IntMap as IM
import Data.List as L
import Data.Vector.Storable as V
import Neat.Types
import Numeric.LinearAlgebra hiding ((<>))

toEntry :: HasCallStack => (Int -> Int) -> Gene -> ((Int, Int), Double)
toEntry toIndex gene = ((toIndex $ gene ^. inNode, toIndex $ gene ^. outNode), toDoubleWeight $ gene ^. weight)

toAssocMatrix :: HasCallStack => IntMap Int -> [Gene] -> AssocMatrix
toAssocMatrix toIndex = (toEntry (toIndex IM.!) <$>)

toGMatrix :: HasCallStack => Genotype -> GMatrix
toGMatrix g =
  mkSparse
    -- . (\a -> trace ("mkSparse " <> show a) a)
    -- add a zero weight connection from the last node to itself so the matrix is the right size.
    . ([((x, x), 0) | x <- [0 .. numNodes - 1]] <>)
    . toAssocMatrix toIndex
    . L.map snd
    . IM.toList
    $ g ^. connections
  where
    -- we need the nodes idenitified with an index instead of a hash so that the state vector isn't of an absurd size.
    (numNodes, toIndex) = mapAccum count 0 (g ^. nodes)
    count a _ = (1 + a, a)

type Vect = Vector Double

step ::
  (HasCallStack, MonadState Vect m) =>
  GMatrix ->
  Int ->
  Vect ->
  m Vect
step m outputSize input =
  do
    let inputSize = V.length input
    s <- (input <>) . V.drop inputSize <$> get -- set input nodes to input
    sum <- m !#> s -- apply connections
    let activations = logistic <$> sum -- apply activation function
    put activations -- 'record' results
    gets (V.take outputSize . V.drop inputSize) -- extract output
  where
    logistic x = 1 / (1 + exp (- x)) -- this is what they use in the paper. TODO: add command line options to change.
