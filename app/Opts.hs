{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Opts
  ( execOptions,
    Opts (..),
  )
where

import Data.List as List
import Eval (evalXor, starterXor)
import Neat
import Options.Applicative as O

data Opts where
  Opts ::
    { opts_popSize :: Int,
      opts_eval :: Genotype -> IO Double,
      opts_startGenotype :: Genotype,
      opts_output :: FilePath,
      opts_generations :: Int
    } ->
    Opts

execOptions :: IO Opts
execOptions = execParser $ parseOptions `info` myInfo

myInfo :: InfoMod a
myInfo = briefDesc <> progDesc "test NEAT algorithem" <> failureCode (-1)

parseOptions :: O.Parser Opts
parseOptions =
  subparser
    ( command
        "run"
        ( runOptions
            `info` progDesc "load a ww file and run steps"
        )
    )

runOptions :: O.Parser Opts
runOptions =
  Opts
    <$> popSizeParser
    <*> evalParser
    <*> starterParser
    <*> fileParser
    <*> generationsParser

evalParser :: O.Parser (Genotype -> IO Double)
evalParser = flag' evalXor (long "xor")

popSizeParser :: O.Parser Int
popSizeParser =
  option
    auto
    ( short 'p'
        <> long "pop-size"
        <> metavar "INT"
        <> help "The number of genomes in the pool at once"
        <> value 100
        <> showDefault
    )

generationsParser :: O.Parser Int
generationsParser =
  option
    auto
    ( short 'g'
        <> long "generations"
        <> metavar "INT"
        <> help "The number of generations to run the evolution for"
        <> value 100
        <> showDefault
    )

fileParser :: O.Parser FilePath
fileParser =
  strOption
    ( short 'f'
        <> long "file"
        <> metavar "FILEPATH"
        <> help "The wireworld file to read in"
        <> action "file"
    )

starterParser = pure starterXor
