module Run where

import Data.IntMap as IM
import Neat.Types
import Numeric.LinearAlgebra

toEntry :: Gene -> ((Int, Int), Double)
toEntry g = ((g ^. inNode, g ^. outNode), g ^. weight)

toAssocMatrix :: [Gene] -> AssocMatrix
toAssocMatrix = (toEntry <$>)

toGMatrix g =
  mkSparse . toAssocMatrix . IM.toList $ g ^. connections

step input =
  do
    let n = length input
    s <- take n <$> get
