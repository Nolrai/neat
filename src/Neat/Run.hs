{-# LANGUAGE FlexibleContexts #-}

module Neat.Run where

import Control.Lens
import Data.IntMap as IM
import Data.List as L
import Data.Vector.Storable as V
import Neat.Types
import Numeric.LinearAlgebra hiding ((<>))

toEntry :: Gene -> ((Int, Int), Double)
toEntry g = ((g ^. inNode, g ^. outNode), toDoubleWeight $ g ^. weight)

toAssocMatrix :: [Gene] -> AssocMatrix
toAssocMatrix = (toEntry <$>)

toGMatrix :: Genotype -> GMatrix
toGMatrix g =
  mkSparse . toAssocMatrix . L.map snd . IM.toList $ g ^. connections

type Vect = Vector Double

step ::
  (MonadState Vect m) =>
  GMatrix ->
  Int ->
  Vect ->
  m Vect
step m outputSize input =
  do
    let inputSize = V.length input
    s <- (input <>) . V.drop inputSize <$> get -- set input nodes to input
    put $ m !#> (input <> s) -- calculate new state
    gets (V.take outputSize . V.drop inputSize) -- extract output
