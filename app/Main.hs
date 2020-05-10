{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main
  ( main,
  )
where

import Control.Concurrent (forkIO, modifyMVar_)
import Data.IntMap as IM (keys)
import Data.Random ()
import Neat (Genotype (..), NodeType, mkNewGeneration)
import Opts (Opts (..), execOptions)

main :: IO ()
main = execOptions >>= body

body :: Opts -> IO ()
body Opts {..} =
  do
    popRef <- newMVar (replicate opts_popSize opts_startGenotype)
    _ <- forkIO (step popRef `mapM_` [0 ..])
    _ <- getLine
    end_pop <- takeMVar popRef
    _ <- testPop `mapM` (end_pop `zip` [0 ..])
    writeFile opts_output (show end_pop)
    exitSuccess
  where
    step :: MVar [Genotype] -> Int -> IO ()
    step popRef n =
      do
        putTextLn $ "start step " <> show n
        modifyMVar_ popRef (doGeneration n)
        putTextLn $ "end step " <> show n
    doGeneration = mkNewGeneration opts_eval opts_popSize

testPop :: (Genotype, Int) -> IO ()
testPop (Genotype {..}, n) =
  writeFile (show n <> ".log") (show . sort . IM.keys $ nodeMap)
  where
    nodeMap :: IntMap NodeType
    nodeMap = _nodes
