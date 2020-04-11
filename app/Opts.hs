module Opts where

{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Options
  ( execOptions,
    Options (..),
    RCO (..),
    ECO (..),
    GCO (..),
  )
where

import Data.List as List
import Data.Text as Text
import Options.Applicative as O
import TextDisplay as TD

data Options where
  Run :: RCO -> Options
  Eval :: ECO -> Options
  Evolve :: GCO -> Options

data RCO where
  RCO ::
    { rco_file :: FilePath,
      rco_steps :: Int,
      rco_inputStyle :: FromCell Text,
      rco_outputStyle :: FromCell Text,
      rco_exit :: Maybe (IO a)
    } ->
    RCO

data ECO where
  ECO ::
    { eco_file :: FilePath,
      eco_inputStyle :: FromCell Text,
      eco_dataWidth :: Int,
      eco_period :: Int,
      eco_maxDelay :: Int
    } ->
    ECO

data GCO where
  GCO ::
    { gco_size :: Int,
      gco_generations :: Int,
      gco_dataWidth :: Int,
      gco_period :: Int,
      gco_maxDelay :: Int,
      gco_outputStyle :: FromCell Text
    } ->
    GCO

execOptions :: IO Options
execOptions = execParser $ parseOptions `info` myInfo

myInfo :: InfoMod a
myInfo = briefDesc <> progDesc "run a wireworld simulation." <> failureCode (-1)

parseOptions :: O.Parser Options
parseOptions =
  subparser
    ( command
        "run"
        ( (Run <$> runOptions)
            `info` progDesc "load a ww file and run steps"
        )
        <> command
          "eval"
          ( (Eval <$> evalOptions)
              `info` progDesc "load a ww file and run it on test input"
          )
        <> command
          "evolve"
          ( (Evolve <$> evolveOptions)
              `info` progDesc "evolve ww metal to run an op."
          )
    )

evolveOptions :: O.Parser GCO
evolveOptions =
  GCO
    <$> popSizeParser
    <*> generationsParser
    <*> dataWidthParser
    <*> periodParser
    <*> maxDelayParser
    <*> outputStyleParser

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

evalOptions :: O.Parser ECO
evalOptions =
  ECO
    <$> fileParser
      <*> inputStyleParser
      <*> dataWidthParser
      <*> periodParser
      <*> maxDelayParser

dataWidthParser :: O.Parser Int
dataWidthParser =
  option
    auto
    ( short 'd'
        <> long "data-width"
        <> metavar "INT"
        <> help "The number of bits of input the gate is expecting"
        <> value 2
        <> showDefault
    )

periodParser :: O.Parser Int
periodParser =
  option
    auto
    ( short 'p'
        <> long "period"
        <> metavar "INT"
        <> help "The number of steps between inputs. Under 3 not recomended."
        <> value 5
        <> showDefault
    )

maxDelayParser :: O.Parser Int
maxDelayParser =
  option
    auto
    ( short 'm'
        <> long "max-delay"
        <> metavar "INT"
        <> help "The number of steps to keep running after the period after the last input."
        <> value 5
        <> showDefault
    )

runOptions :: O.Parser RCO
runOptions =
  RCO
    <$> fileParser
    <*> stepsParser
    <*> inputStyleParser
    <*> outputStyleParser
    <*> exitParser

fileParser :: O.Parser FilePath
fileParser =
  strOption
    ( short 'f'
        <> long "file"
        <> metavar "FILEPATH"
        <> help "The wireworld file to read in"
        <> action "file"
    )

stepsParser :: O.Parser Int
stepsParser =
  option
    auto
    (short 'n' <> long "steps" <> metavar "INT" <> help "the number of steps of simulation to run before exiting/waiting")

styleParser :: Char -> O.Parser (FromCell Text)
styleParser c = List.foldr (\(f, name) others -> others <|> flag' f (long $ [c] <> name)) customStyleParser styles

inputStyleParser :: O.Parser (FromCell Text)
inputStyleParser = styleParser 'i'

outputStyleParser :: O.Parser (FromCell Text)
outputStyleParser = styleParser 'o'

styles :: [(FromCell Text, String)]
styles = [(rosettaCell, "rosetta"), (boxCell, "box"), (coloredCell, "color")]

customStyleParser :: O.Parser (FromCell Text)
customStyleParser = option readCustomStyle (short 'c' <> long "custom")

readCustomStyle :: ReadM (FromCell Text)
readCustomStyle = maybeReader $
  \(string :: String) ->
    do
      let text = toText string
      let [emptyCell, headCell, tailCell, metalCell] = chop (Text.length text `div` 4) text
      pure FromCell {..}

chop :: Int -> Text -> [Text]
chop chunkSize string =
  let (here, there) = Text.splitAt chunkSize string
   in here : chop chunkSize there

exitParser :: O.Parser (Maybe (IO Text))
exitParser = flag (Just getLine) Nothing (short 'e' <> long "exit")
