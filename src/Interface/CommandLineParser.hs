module Interface.CommandLineParser where

import Options.Applicative
import Interface.CommandLineLexicon

data Global = Global Cmd                                                         deriving (Show)
data Cmd  = Config    ConfigOpts
--          | Repo      RepoOpts                                                   
          deriving (Show)
   
-- config
data ConfigOpts = ConfigOpts        ConfigCmd                                    deriving (Show)
data ConfigCmd  = ConfigThreshold   ConfigThresholdOpts
                | ConfigTermDate    ConfigTermDateOpts                           deriving (Show)
   
data ConfigThresholdOpts = ConfigThresholdOpts ConfigThresholdCmd                deriving (Show)
data ConfigThresholdCmd  = ConfigThresholdSet  ConfigThresholdSetOpts
                         | ConfigThresholdList ConfigThresholdListOpts           deriving (Show)
   
data ConfigThresholdSetOpts = ConfigThresholdSetOpts
 { configThresholdSetCurrent :: Maybe String
 , configThresholdSetChoose  :: Maybe String }                                   deriving (Show)
data ConfigThresholdListOpts = ConfigThresholdListOpts                           deriving (Show)
   
data ConfigTermDateOpts = ConfigTermDateOpts ConfigTermDateCmd                   deriving (Show)
data ConfigTermDateCmd  = ConfigTermDateSet  ConfigTermDateSetOpts
                        | ConfigTermDateList ConfigTermDateListOpts              deriving (Show)
   
data ConfigTermDateSetOpts = ConfigTermDateSetOpts
 { configTermDateSetTerm1 :: Maybe String
 , configTermDateSetTerm2 :: Maybe String
 , configTermDateSetTerm3 :: Maybe String }                                      deriving (Show)
data ConfigTermDateListOpts = ConfigTermDateListOpts                             deriving (Show)
   
---- repo
--data RepoOpts = RepoOpts   RepoCmd                                               deriving (Show)
--data RepoCmd  = RepoAdd    RepoAddOpts
--              | RepoRemove RepoRemoveOpts
--              | RepoList   RepoListOpts                                          deriving (Show)
--   
--data RepoAddOpts = RepoAddOpts
-- { repoAddName :: String }                                                       deriving (Show)
--data RepoRemoveOpts = RepoRemoveOpts 
-- { repoRemoveRepoNode :: Maybe String }                                          deriving (Show)
--data RepoListOpts = RepoListOpts                                                 deriving (Show)

-- parser info
globalInfo =              info (myHelper <*> global)              (progDesc globalDesc <> header globalHeader)
 
configInfo =              info (myHelper <*> config)              (progDesc configDesc)
configThresholdInfo =     info (myHelper <*> configThreshold)     (progDesc configThresholdDesc)
configThresholdSetInfo =  info (myHelper <*> configThresholdSet)  (progDesc configThresholdSetDesc)
configThresholdListInfo = info (myHelper <*> configThresholdList) (progDesc configThresholdListDesc) 
configTermDateInfo =      info (myHelper <*> configTermDate)      (progDesc configTermDateDesc)
configTermDateSetInfo =   info (myHelper <*> configTermDateSet)   (progDesc configTermDateSetDesc)
configTermDateListInfo =  info (myHelper <*> configTermDateList)  (progDesc configTermDateListDesc)

--repoInfo =                info (myHelper <*> repo)                (progDesc "Repo desc")
--repoAddInfo =             info (myHelper <*> repoAdd)             (progDesc "Repo add desc")
--repoRemoveInfo =          info (myHelper <*> repoRemove)          (progDesc "Repo remove desc")
--repoListInfo =            info (myHelper <*> repoList)            (progDesc "Repo list desc")

-- Change the -h for a -?
myHelper ::  Parser (a -> a)
myHelper = abortOption ShowHelpText $ (toMod helpOpt) <> help helpHelp 

-- parsers
global = Global <$> subparser (
 command configSub configInfo -- <>
-- command "repo"   repoInfo
 )
 
config = Config <$> ConfigOpts  <$> subparser (
 command thresholdSub configThresholdInfo <>
 command termDateSub  configTermDateInfo)

configThreshold = ConfigThreshold <$> ConfigThresholdOpts <$> subparser (
 command setSub  configThresholdSetInfo <>
 command listSub configThresholdListInfo)

configThresholdSet = ConfigThresholdSet <$> (ConfigThresholdSetOpts
  <$> optional (strOption $ toMod configThresholdSetCurrentOpt <> metavar configThresholdSetCurrentMeta <> help configThresholdSetCurrentHelp)
  <*> optional (strOption $ toMod configThresholdSetChooseOpt  <> metavar configThresholdSetChooseMeta  <> help configThresholdSetChooseHelp))

configThresholdList = ConfigThresholdList <$> pure ConfigThresholdListOpts

configTermDate = ConfigTermDate <$> ConfigTermDateOpts <$> subparser (
 command setSub  configTermDateSetInfo <>
 command listSub configTermDateListInfo)

configTermDateSet = ConfigTermDateSet <$> (ConfigTermDateSetOpts
  <$> optional (strOption $ toMod configTermDateSetTerm1Opt <> metavar configTermDateSetTerm1Meta <> help configTermDateSetTerm1Help)
  <*> optional (strOption $ toMod configTermDateSetTerm2Opt <> metavar configTermDateSetTerm2Meta <> help configTermDateSetTerm2Help)
  <*> optional (strOption $ toMod configTermDateSetTerm3Opt <> metavar configTermDateSetTerm3Meta <> help configTermDateSetTerm3Help))

configTermDateList = ConfigTermDateList <$> pure ConfigTermDateListOpts 

--repo = Repo <$> RepoOpts <$> subparser (
-- command "add"    repoAddInfo <>
-- command "remove" repoRemoveInfo <> 
-- command "list"   repoListInfo)
--
--repoAdd    = RepoAdd    <$> RepoAddOpts    <$>
-- argument str (metavar "NAME")
--
--repoRemove = RepoRemove <$> RepoRemoveOpts <$>
-- optional (strOption $ short 'r' <> long "repo" <> metavar "REPO" <> help "Repo node help")
--
--repoList   = RepoList   <$> pure RepoListOpts

toMod (O s l) = short s <> long l              