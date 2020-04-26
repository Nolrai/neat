{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main
  ( main,
  )
where

import Control.Concurrent (forkIO, modifyMVar_)
import Data.RVar
import Data.Random ()
import Neat (Genotype, mkNewGeneration)
import Opts (Opts (..), execOptions)

main :: IO ()
main = execOptions >>= body

body :: Opts -> IO ()
body Opts {..} =
  do
    popRef <- newMVar (replicate opts_popSize opts_startGenotype)
    _ <- forkIO (step popRef `mapM_` [0 ..])
    _ <- getLine
    writeFile opts_output =<< (show <$> takeMVar popRef)
    exitSuccess
  where
    step :: MVar [Genotype] -> Int -> IO ()
    step popRef n =
      do
        putTextLn $ "start step " <> show n
        modifyMVar_ popRef (doGeneration n)
        putTextLn $ "end step " <> show n
    doGeneration = mkNewGeneration opts_eval opts_popSize
