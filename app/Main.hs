{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
  )
where

import Data.IORef
import Neat (mkNewGeneration)
import Opts
import System.Random

main = getOpts' >>= body

body Opts {popSize, eval, startGenotype} =
  do
    g <- getStdRandom
    popRef <- newIORef (replicateN popSize startGenotype)
    s <- fork (step popRef `mapM_` [0 ..])
    getLine
    kill s
    writeFile output =<< readIORef popRef
  where
    step n =
      do
        putTextLn $ "step " <> show n
        atomicModifyIORef' popRef $
          mkNewGeneration eval popSize n
