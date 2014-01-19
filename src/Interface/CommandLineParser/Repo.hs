module Interface.CommandLineParser.Repo where

import Options.Applicative
import Interface.Lexicon
import Interface.CommandLineParser.Utils
import Security.SecurityManager

data RepoOpts = RepoOpts   RepoCmd        deriving (Show, Eq)
data RepoCmd  = RepoAdd    RepoAddOpts
              | RepoRemove RepoRemoveOpts
              | RepoList   RepoListOpts   deriving (Show, Eq)

data RepoAddOpts = RepoAddOpts
 { repoAddName :: String }                deriving (Show, Eq)

data RepoRemoveOpts = RepoRemoveOpts
 { repoRemoveRepoNN :: Maybe String }     deriving (Show, Eq)

data RepoListOpts = RepoListOpts          deriving (Show, Eq)

repoInfo role =        info (myHelper <*> repo role)       (progDesc repoDesc)
repoAddInfo =     info (myHelper <*> repoAdd)    (progDesc repoAddDesc)
repoRemoveInfo =  info (myHelper <*> repoRemove) (progDesc repoRemoveDesc)
repoListInfo =    info (myHelper <*> repoList)   (progDesc repoListDesc)

repo role = RepoOpts <$> subparser (
 hasRepoWriteRights role (command addSub    repoAddInfo) <>
 hasRepoWriteRights role (command removeSub repoRemoveInfo) <>
 hasRepoReadRights  role (command listSub   repoListInfo))

repoAdd = RepoAdd <$> (RepoAddOpts
 <$> argument str (metavar repoAddMeta <> help repoAddHelp))

repoRemove = RepoRemove <$> (RepoRemoveOpts
 <$> optional (strOption $ toMod repoNodeOpt <> metavar repoNodeMeta <> help repoNodeHelp))

repoList = RepoList <$> pure RepoListOpts
 