module Eval where

-- ( evalXor,
--   starterXor,
-- )

-- import Grenade

import Control.Exception (assert)
import Data.Hashable ()
import Data.Hashable.Generic
import Data.IntMap as IM
import qualified Data.List as L
import Data.Random
import Data.Random.Distribution ()
import Data.Random.Sample ()
import Data.Vector as BV -- Boxed Vectors
import Data.Vector.Storable as SV
import Neat
import Numeric.LinearAlgebra (GMatrix, dot, nCols)
import Utils

dataWidth :: Integral i => i
dataWidth = 8

evalXor :: Genotype -> IO Double
evalXor g =
  do
    a <- getRandomInput
    b <- getRandomInput
    evaluateNF (a, b)
    pure $ evalXorBody (toGMatrix g) a b

-- get a byte of random bits in a form the Neuro Network understands
getRandomInput :: (MonadIO m, MonadRandom m) => m Vect
getRandomInput = sample $ SV.replicateM dataWidth (randomElement [-1, 1])

-- This seems too complicated!
evalXorBody :: HasCallStack => GMatrix -> Vect -> Vect -> Double
evalXorBody gmatrix inputA inputB =
  BV.sum . BV.zipWith (*) fallOff $ scoreVector
  where
    -- We score later rounds less, and vectors need to have a finite length.
    fallOff :: BV.Vector Double
    fallOff = (0.5 ^) <$> BV.enumFromN (1 :: Int) (BV.length scoreVector)
    scoreVector :: BV.Vector Double
    scoreVector = makePositive . (expected `dot`) <$> railedOutput
    -- scale from [-1,1] to [0,1], this needs to happen _after_ the dot product with expected, not before.
    makePositive :: Double -> Double
    makePositive x = (x - (-1)) / (1 - (-1))
    -- this is isomorphic to xor (with -1 ~= False, +1 ~= True)
    expected :: Vect
    expected = ((-1) *) `SV.map` SV.zipWith (*) inputA inputB
    -- rail the output so it's meaningful.
    railedOutput :: BV.Vector Vect
    railedOutput = SV.map (max (-1) . min 1) <$> BV.drop 2 output
    output :: BV.Vector Vect
    output = evalState actions startState
    actions :: State Vect (BV.Vector Vect)
    actions = oneStep `BV.mapM` BV.enumFromN 0 (iterations * 2)
    iterations = 3 -- change if needed
    startState :: Vect
    startState = SV.replicate (nCols gmatrix) 0.0
    oneStep :: Int -> State Vect Vect
    oneStep n = Neat.step gmatrix dataWidth (input n)
    input n
      | n `mod` 2 == 0 = inputA
      | otherwise = inputB

instance Hashable a => Hashable (IntMap a) where
  hashWithSalt s m = IM.foldr (flip hashWithSalt) (hashWithSalt s $ IM.size m) m

instance Hashable Gene where
  hashWithSalt = genericHashWithSalt

instance Hashable NodeType where
  hashWithSalt = genericHashWithSalt

starterXor :: Genotype
starterXor =
  Genotype
    { _nodes =
        IM.fromAscList $ (\l -> assert (isAsc l) l) $ L.zip [0 ..] (L.replicate dataWidth Input <> L.replicate dataWidth Output),
      _connections = mempty
    }
