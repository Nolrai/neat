{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main
  ( main,
  )
where

import Control.Concurrent (forkIO, killThread, modifyMVar_)
import Data.IntMap as IM (toList)
import Data.List ((!!))
import Data.Matrix
import Data.Random ()
import Neat (Genotype (..), delta, mkNewGeneration)
import Opts (Opts (..), execOptions)

main :: IO ()
main =
  do
    opts <- execOptions
    print opts
    body opts

body :: Opts -> IO ()
body Opts {..} =
  do
    -- Set things up
    popRef <- newMVar (replicate opts_popSize opts_startGenotype)
    barrier <- newBarrier
    worker <- forkIO (step popRef `mapM_` [0 .. opts_generations] >> openBarrier barrier)
    putTextLn "Hit enter to stop"
    ui <- forkIO (getLine >> openBarrier barrier)
    -- wait until we are done, then print results.
    waitOn barrier
    killThread worker >> killThread ui -- clean up. This is okay because modifyMVar_ is exception safe.
    end_pop <- takeMVar popRef
    _ <- testPop `mapM` (end_pop `zip` [0 ..])
    writeFile opts_output . prettyMatrix $ deltaMatrix end_pop
    exitSuccess
  where
    step :: MVar [Genotype] -> Int -> IO ()
    step popRef n =
      do
        putTextLn $ "start step " <> show n
        modifyMVar_ popRef (doGeneration n)
        putTextLn $ "end step " <> show n
    doGeneration :: Int -> [Genotype] -> IO [Genotype]
    doGeneration = mkNewGeneration opts_eval opts_popSize
    deltaMatrix :: [Genotype] -> Matrix Double
    deltaMatrix p =
      let n = length p
       in matrix n n (\(i, j) -> delta (p !! (i - 1)) (p !! (j - 1)))
    newBarrier :: IO (MVar ())
    newBarrier = newEmptyMVar
    openBarrier :: MVar () -> IO ()
    openBarrier b = putMVar b ()
    waitOn :: MVar () -> IO ()
    waitOn = takeMVar

testPop :: (Genotype, Int) -> IO ()
testPop (Genotype {..}, n) =
  writeFileText (show n <> ".log") msg
  where
    msg =
      "nodes:\n" <> show (IM.toList _nodes)
        <> "\nconnections:\n"
        <> unlines (show <$> IM.toList _connections)
