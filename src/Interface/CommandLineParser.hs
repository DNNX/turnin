module Interface.CommandLineParser where

import Options.Applicative
import Interface.Lexicon
import Interface.CommandLineParser.Utils
import Interface.CommandLineParser.Config
import Interface.CommandLineParser.Repo
import Interface.CommandLineParser.Term

data Global = Global Cmd     deriving (Show)
data Cmd = Config ConfigOpts 
         | Repo   RepoOpts
         | Term   TermOpts   deriving (Show)
 
globalInfo = info (myHelper <*> global) (progDesc globalDesc <> header globalHeader)
 
global = Global <$> subparser (
 command configSub (Config <$> configInfo) <>
 command repoSub   (Repo   <$> repoInfo) <>
 command termSub   (Term   <$> termInfo))
  