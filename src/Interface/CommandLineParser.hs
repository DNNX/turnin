module Interface.CommandLineParser where

import Options.Applicative
import Interface.CommandLineLexicon

data Global = Global Cmd                                                         deriving (Show)
data Cmd  = Config    ConfigOpts                                                 deriving (Show)
 
-- config
data ConfigOpts = ConfigOpts        ConfigCmd                                    deriving (Show)
data ConfigCmd  = ConfigThreshold   ConfigThresholdOpts
                | ConfigTermDate    ConfigTermDateOpts
                | ConfigProjectDate ConfigProjectDateOpts                        deriving (Show)
 
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
 
data ConfigProjectDateOpts = ConfigProjectDateOpts ConfigProjectDateCmd          deriving (Show)
data ConfigProjectDateCmd  = ConfigProjectDateSet  ConfigProjectDateSetOpts
                           | ConfigProjectDateList ConfigProjectDateListOpts     deriving (Show)
 
data ConfigProjectDateSetOpts = ConfigProjectDateSetOpts
 { configProjectDateSetEnd  :: Maybe String
 , configProjectDateSetLate :: Maybe String }                                    deriving (Show)
data ConfigProjectDateListOpts = ConfigProjectDateListOpts                       deriving (Show)
 
-- parser info
globalInfo =                info (myHelper <*> global)              (progDesc globalDesc <> header globalHeader)
 
configInfo =                info (myHelper <*> config)              (progDesc configDesc)
configThresholdInfo =       info (myHelper <*> configThreshold)     (progDesc configThresholdDesc)
configThresholdSetInfo =    info (myHelper <*> configThresholdSet)  (progDesc configThresholdSetDesc)
configThresholdListInfo =   info (myHelper <*> configThresholdList) (progDesc configThresholdListDesc) 
configTermDateInfo =        info (myHelper <*> configTermDate)      (progDesc configTermDateDesc)
configTermDateSetInfo =     info (myHelper <*> configTermDateSet)   (progDesc configTermDateSetDesc)
configTermDateListInfo =    info (myHelper <*> configTermDateList)  (progDesc configTermDateListDesc)
configProjectDateInfo =     info (myHelper <*> configProjectDate)      (progDesc configProjectDateDesc)
configProjectDateSetInfo =  info (myHelper <*> configProjectDateSet)   (progDesc configProjectDateSetDesc)
configProjectDateListInfo = info (myHelper <*> configProjectDateList)  (progDesc configProjectDateListDesc)

-- Custom help command 
myHelper ::  Parser (a -> a)
myHelper = abortOption ShowHelpText $ toMod helpOpt <> help helpHelp 

-- parsers
global = Global <$> subparser (
 command configSub configInfo)
 
config = Config <$> ConfigOpts  <$> subparser (
 command thresholdSub   configThresholdInfo <>
 command termDateSub    configTermDateInfo <>
 command projectDateSub configProjectDateInfo)

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

configProjectDate = ConfigProjectDate <$> ConfigProjectDateOpts <$> subparser (
 command setSub  configProjectDateSetInfo <>
 command listSub configProjectDateListInfo)
 
configProjectDateSet = ConfigProjectDateSet <$> (ConfigProjectDateSetOpts
  <$> optional (strOption $ toMod configProjectDateSetEndOpt  <> metavar configProjectDateSetEndMeta  <> help configProjectDateSetEndHelp)
  <*> optional (strOption $ toMod configProjectDateSetLateOpt <> metavar configProjectDateSetLateMeta <> help configProjectDateSetLateHelp))

configProjectDateList = ConfigProjectDateList <$> pure ConfigProjectDateListOpts 

toMod (O s l) = short s <> long l              
