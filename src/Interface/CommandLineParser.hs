module Interface.CommandLineParser where

import Options.Applicative
import Interface.Lexicon
import Interface.CommandLineParser.Utils
import Interface.CommandLineParser.Config
import Interface.CommandLineParser.Repo

data Global = Global Cmd     deriving (Show)
data Cmd = Config ConfigOpts 
         | Repo   RepoOpts   deriving (Show)
 
globalInfo = info (myHelper <*> global) (progDesc globalDesc <> header globalHeader)
 
global = Global <$> subparser (
 command configSub (Config <$> configInfo) <>
 command repoSub   (Repo   <$> repoInfo))
 