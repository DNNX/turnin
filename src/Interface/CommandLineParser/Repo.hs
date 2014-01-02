module Interface.CommandLineParser.Repo where

import Options.Applicative
import Interface.Lexicon
import Interface.CommandLineParser.Utils

data RepoOpts = RepoOpts   RepoCmd        deriving (Show)
data RepoCmd  = RepoAdd    RepoAddOpts
              | RepoRemove RepoRemoveOpts
              | RepoList   RepoListOpts   deriving (Show)

data RepoAddOpts = RepoAddOpts
 { repoAddName :: String }                deriving (Show)
 
data RepoRemoveOpts = RepoRemoveOpts
 { repoRemoveRepoNN :: Maybe String }             deriving (Show)
 
data RepoListOpts = RepoListOpts          deriving (Show) 
 
repoInfo =        info (myHelper <*> repo)       (progDesc repoDesc)
repoAddInfo =     info (myHelper <*> repoAdd)    (progDesc repoAddDesc)
repoRemoveInfo =  info (myHelper <*> repoRemove) (progDesc repoRemoveDesc)  
repoListInfo =    info (myHelper <*> repoList)   (progDesc repoListDesc)

repo = RepoOpts <$> subparser (
 command addSub    repoAddInfo <>
 command removeSub repoRemoveInfo <>
 command listSub   repoListInfo)

repoAdd = RepoAdd <$> (RepoAddOpts
 <$> argument str (metavar repoAddNameMeta <> help repoAddNameHelp))

repoRemove = RepoRemove <$> (RepoRemoveOpts
 <$> optional (strOption $ toMod repoNodeOpt <> metavar repoNodeMeta <> help repoNodeHelp))
 
repoList = RepoList <$> pure RepoListOpts
 