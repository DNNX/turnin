module Interface.CommandLineParser where

import Options.Applicative
import Interface.Lexicon
import Interface.CommandLineParser.Config
import Interface.CommandLineParser.Utils

data Global = Global Cmd     deriving (Show)
data Cmd = Config ConfigOpts deriving (Show)
 
globalInfo = info (myHelper <*> global) (progDesc globalDesc <> header globalHeader)
 
global = Global <$> subparser (
 command configSub (Config <$> configInfo))
