module Interface.CommandLineParser where

import Options.Applicative
import Data.Monoid
import Interface.CommandLineLexicon

data Global = Global Cmd                                                         deriving (Show)
data Cmd  = Config    ConfigOpts
--          | Repo      RepoOpts                                                   
          deriving (Show)
   
-- config
data ConfigOpts = ConfigOpts        ConfigCmd                                    deriving (Show)
data ConfigCmd  = ConfigThreshold   ConfigThresholdOpts
--                | ConfigTermDate    ConfigTermDateOpts                           
                deriving (Show)
   
data ConfigThresholdOpts = ConfigThresholdOpts ConfigThresholdCmd                deriving (Show)
data ConfigThresholdCmd  = ConfigThresholdSet  ConfigThresholdSetOpts
--                         | ConfigThresholdList ConfigThresholdListOpts           
                         deriving (Show)
   
data ConfigThresholdSetOpts = ConfigThresholdSetOpts
 { configThresholdSetCurrent :: Maybe String
 , configThresholdSetChoose  :: Maybe String }                                   deriving (Show)
--data ConfigThresholdListOpts = ConfigThresholdListOpts                           deriving (Show)
--   
--data ConfigTermDateOpts = ConfigTermDateOpts ConfigTermDateCmd                   deriving (Show)
--data ConfigTermDateCmd  = ConfigTermDateSet  ConfigTermDateSetOpts
--                        | ConfigTermDateList ConfigTermDateListOpts              deriving (Show)
--   
--data ConfigTermDateSetOpts = ConfigTermDateSetOpts
-- { configTermDateSetTerm1 :: Maybe String
-- , configTermDateSetTerm2 :: Maybe String
-- , configTermDateSetTerm3 :: Maybe String }                                      deriving (Show)
--data ConfigTermDateListOpts = ConfigTermDateListOpts                             deriving (Show)
--   
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
--configThresholdListInfo = info (myHelper <*> configThresholdList) (progDesc "Config threshold list desc")
--configTermDateInfo =      info (myHelper <*> configTermDate)      (progDesc "Config term date desc")
--configTermDateSetInfo =   info (myHelper <*> configTermDateSet)   (progDesc "Config term date set desc")
--configTermDateListInfo =  info (myHelper <*> configTermDateList)  (progDesc "Config term date list desc")
--
--repoInfo =                info (myHelper <*> repo)                (progDesc "Repo desc")
--repoAddInfo =             info (myHelper <*> repoAdd)             (progDesc "Repo add desc")
--repoRemoveInfo =          info (myHelper <*> repoRemove)          (progDesc "Repo remove desc")
--repoListInfo =            info (myHelper <*> repoList)            (progDesc "Repo list desc")

-- Change the -h for a -?
myHelper ::  Parser (a -> a)
myHelper = abortOption ShowHelpText $ mconcat
  [ long "help"
  , short '?'
  , help "Show this help text" ]

-- parsers
global = Global <$> subparser (
 command configSub configInfo -- <>
-- command "repo"   repoInfo
 )
 
config = Config <$> ConfigOpts  <$> subparser (
 command thresholdSub configThresholdInfo -- <>
-- command "termDate"  configTermDateInfo
 )

configThreshold = ConfigThreshold <$> ConfigThresholdOpts <$> subparser (
 command setSub  configThresholdSetInfo -- <>
-- command "list" configThresholdListInfo
 )

configThresholdSet = ConfigThresholdSet <$> (ConfigThresholdSetOpts
  <$> optional (strOption $ short 'u' <> long "cu" <> long "current" <> metavar "CURRENT" <> help "Config threshold set current help")
  <*> optional (strOption $ short 'o' <> long "ch" <> long "choose"  <> metavar "CHOOSE"  <> help "Config threshold set choose help"))

--configThresholdList = ConfigThresholdList <$> pure ConfigThresholdListOpts
--
--configTermDate = ConfigTermDate <$> ConfigTermDateOpts <$> subparser (
-- command "set"  configTermDateSetInfo <>
-- command "list" configTermDateListInfo)
--
--configTermDateSet = ConfigTermDateSet <$> (ConfigTermDateSetOpts
--  <$> optional (strOption $ short '1' <> long "term1" <> metavar "TERM1" <> help "Config term date set term1 help")
--  <*> optional (strOption $ short '2' <> long "term2" <> metavar "TERM2" <> help "Config term date set term2 help")
--  <*> optional (strOption $ short '3' <> long "term3" <> metavar "TERM3" <> help "Config term date set term3 help"))
--
--configTermDateList = ConfigTermDateList <$> pure ConfigTermDateListOpts
--
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

              