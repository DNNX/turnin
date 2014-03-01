module Interface.CommandLineParser.Config where

import Options.Applicative
import Interface.Lexicon
import Interface.CommandLineParser.Utils
import Security.SecurityManager

data ConfigOpts = ConfigOpts          ConfigCmd                                    deriving (Show, Eq)
data ConfigCmd  = ConfigThreshold     ConfigThresholdOpts
                | ConfigTermDate      ConfigTermDateOpts
                | ConfigProjectDate   ConfigProjectDateOpts
                | ConfigAcceptExec    ConfigAcceptExecOpts
                | ConfigTimeLimit     ConfigTimeLimitOpts
                | ConfigSpaceLimit    ConfigSpaceLimitOpts
                | ConfigAdminGroups   ConfigAdminGroupsOpts
                | ConfigTeacherGroups ConfigTeacherGroupsOpts
                | ConfigCorrector     ConfigCorrectorOpts                          deriving (Show, Eq)

data ConfigThresholdOpts = ConfigThresholdOpts ConfigThresholdCmd                  deriving (Show, Eq)
data ConfigThresholdCmd  = ConfigThresholdSet  ConfigThresholdSetOpts
                         | ConfigThresholdList ConfigThresholdListOpts             deriving (Show, Eq)

data ConfigThresholdSetOpts = ConfigThresholdSetOpts
 { configThresholdSetCurrent :: Maybe String
 , configThresholdSetChoose  :: Maybe String }                                     deriving (Show, Eq)
data ConfigThresholdListOpts = ConfigThresholdListOpts                             deriving (Show, Eq)

data ConfigTermDateOpts = ConfigTermDateOpts ConfigTermDateCmd                     deriving (Show, Eq)
data ConfigTermDateCmd  = ConfigTermDateSet  ConfigTermDateSetOpts
                        | ConfigTermDateList ConfigTermDateListOpts                deriving (Show, Eq)

data ConfigTermDateSetOpts = ConfigTermDateSetOpts
 { configTermDateSetTerm1 :: Maybe String
 , configTermDateSetTerm2 :: Maybe String
 , configTermDateSetTerm3 :: Maybe String }                                        deriving (Show, Eq)
data ConfigTermDateListOpts = ConfigTermDateListOpts                               deriving (Show, Eq)

data ConfigProjectDateOpts = ConfigProjectDateOpts ConfigProjectDateCmd            deriving (Show, Eq)
data ConfigProjectDateCmd  = ConfigProjectDateSet  ConfigProjectDateSetOpts
                           | ConfigProjectDateList ConfigProjectDateListOpts       deriving (Show, Eq)

data ConfigProjectDateSetOpts = ConfigProjectDateSetOpts
 { configProjectDateSetEnd  :: Maybe String
 , configProjectDateSetLate :: Maybe String }                                      deriving (Show, Eq)
data ConfigProjectDateListOpts = ConfigProjectDateListOpts                         deriving (Show, Eq)

data ConfigAcceptExecOpts = ConfigAcceptExecOpts ConfigAcceptExecCmd               deriving (Show, Eq)
data ConfigAcceptExecCmd  = ConfigAcceptExecSet  ConfigAcceptExecSetOpts
                          | ConfigAcceptExecList ConfigAcceptExecListOpts          deriving (Show, Eq)

data ConfigAcceptExecSetOpts = ConfigAcceptExecSetOpts
 { configAcceptExecSetVal :: String }                                              deriving (Show, Eq)
data ConfigAcceptExecListOpts = ConfigAcceptExecListOpts                           deriving (Show, Eq)

data ConfigTimeLimitOpts = ConfigTimeLimitOpts ConfigTimeLimitCmd                  deriving (Show, Eq)
data ConfigTimeLimitCmd  = ConfigTimeLimitSet  ConfigTimeLimitSetOpts
                         | ConfigTimeLimitList ConfigTimeLimitListOpts             deriving (Show, Eq)

data ConfigTimeLimitSetOpts = ConfigTimeLimitSetOpts
 { configTimeLimitSetVal :: String }                                               deriving (Show, Eq)
data ConfigTimeLimitListOpts = ConfigTimeLimitListOpts                             deriving (Show, Eq)

data ConfigSpaceLimitOpts = ConfigSpaceLimitOpts ConfigSpaceLimitCmd               deriving (Show, Eq)
data ConfigSpaceLimitCmd  = ConfigSpaceLimitSet  ConfigSpaceLimitSetOpts
                          | ConfigSpaceLimitList ConfigSpaceLimitListOpts          deriving (Show, Eq)

data ConfigSpaceLimitSetOpts = ConfigSpaceLimitSetOpts
 { configSpaceLimitSetVal :: String }                                              deriving (Show, Eq)
data ConfigSpaceLimitListOpts = ConfigSpaceLimitListOpts                           deriving (Show, Eq)

data ConfigAdminGroupsOpts = ConfigAdminGroupsOpts ConfigAdminGroupsCmd            deriving (Show, Eq)
data ConfigAdminGroupsCmd  = ConfigAdminGroupsSet  ConfigAdminGroupsSetOpts
                           | ConfigAdminGroupsList ConfigAdminGroupsListOpts       deriving (Show, Eq)

data ConfigAdminGroupsSetOpts = ConfigAdminGroupsSetOpts
 { configAdminGroupsSetGroups :: [String] }                                        deriving (Show, Eq)
data ConfigAdminGroupsListOpts = ConfigAdminGroupsListOpts                         deriving (Show, Eq)

data ConfigTeacherGroupsOpts = ConfigTeacherGroupsOpts ConfigTeacherGroupsCmd      deriving (Show, Eq)
data ConfigTeacherGroupsCmd  = ConfigTeacherGroupsSet  ConfigTeacherGroupsSetOpts
                             | ConfigTeacherGroupsList ConfigTeacherGroupsListOpts deriving (Show, Eq)

data ConfigTeacherGroupsSetOpts = ConfigTeacherGroupsSetOpts
 { configTeacherGroupsSetGroups :: [String] }                                      deriving (Show, Eq)
data ConfigTeacherGroupsListOpts = ConfigTeacherGroupsListOpts                     deriving (Show, Eq)

data ConfigCorrectorOpts = ConfigCorrectorOpts   ConfigCorrectorCmd                deriving (Show, Eq)
data ConfigCorrectorCmd  = ConfigCorrectorIs     ConfigCorrectorIsOpts
                         | ConfigCorrectorAdd    ConfigCorrectorAddOpts
                         | ConfigCorrectorRemove ConfigCorrectorRemoveOpts         deriving (Show, Eq)

data ConfigCorrectorIsOpts = ConfigCorrectorIsOpts
 { configCorrectorIsName :: String }                                               deriving (Show, Eq)
data ConfigCorrectorAddOpts = ConfigCorrectorAddOpts
 { configCorrectorAddName :: String }                                              deriving (Show, Eq)
data ConfigCorrectorRemoveOpts = ConfigCorrectorRemoveOpts
 { configCorrectorRemoveName :: String }                                           deriving (Show, Eq)


configInfo role =              info (myHelper <*> config role)              (progDesc configDesc)
configThresholdInfo role =     info (myHelper <*> configThreshold role)     (progDesc configThresholdDesc)
configThresholdSetInfo =       info (myHelper <*> configThresholdSet)       (progDesc configThresholdSetDesc)
configThresholdListInfo =      info (myHelper <*> configThresholdList)      (progDesc configThresholdListDesc)
configTermDateInfo role =      info (myHelper <*> configTermDate role)      (progDesc configTermDateDesc)
configTermDateSetInfo =        info (myHelper <*> configTermDateSet)        (progDesc configTermDateSetDesc)
configTermDateListInfo =       info (myHelper <*> configTermDateList)       (progDesc configTermDateListDesc)
configProjectDateInfo role =   info (myHelper <*> configProjectDate role)   (progDesc configProjectDateDesc)
configProjectDateSetInfo =     info (myHelper <*> configProjectDateSet)     (progDesc configProjectDateSetDesc)
configProjectDateListInfo =    info (myHelper <*> configProjectDateList)    (progDesc configProjectDateListDesc)
configAcceptExecInfo role =    info (myHelper <*> configAcceptExec role)    (progDesc configAcceptExecDesc)
configAcceptExecSetInfo =      info (myHelper <*> configAcceptExecSet)      (progDesc configAcceptExecSetDesc)
configAcceptExecListInfo =     info (myHelper <*> configAcceptExecList)     (progDesc configAcceptExecListDesc)
configTimeLimitInfo role =     info (myHelper <*> configTimeLimit role)     (progDesc configTimeLimitDesc)
configTimeLimitSetInfo =       info (myHelper <*> configTimeLimitSet)       (progDesc configTimeLimitSetDesc)
configTimeLimitListInfo =      info (myHelper <*> configTimeLimitList)      (progDesc configTimeLimitListDesc)
configSpaceLimitInfo role =    info (myHelper <*> configSpaceLimit role)    (progDesc configSpaceLimitDesc)
configSpaceLimitSetInfo =      info (myHelper <*> configSpaceLimitSet)      (progDesc configSpaceLimitSetDesc)
configSpaceLimitListInfo =     info (myHelper <*> configSpaceLimitList)     (progDesc configSpaceLimitListDesc)
configAdminGroupsInfo role =   info (myHelper <*> configAdminGroups role)   (progDesc configAdminGroupsDesc)
configAdminGroupsSetInfo =     info (myHelper <*> configAdminGroupsSet)     (progDesc configAdminGroupsSetDesc)
configAdminGroupsListInfo =    info (myHelper <*> configAdminGroupsList)    (progDesc configAdminGroupsListDesc)
configTeacherGroupsInfo role = info (myHelper <*> configTeacherGroups role) (progDesc configTeacherGroupsDesc)
configTeacherGroupsSetInfo =   info (myHelper <*> configTeacherGroupsSet)   (progDesc configTeacherGroupsSetDesc)
configTeacherGroupsListInfo =  info (myHelper <*> configTeacherGroupsList)  (progDesc configTeacherGroupsListDesc)
configCorrectorInfo role =     info (myHelper <*> configCorrector role)     (progDesc configCorrectorDesc)
configCorrectorIsInfo =        info (myHelper <*> configCorrectorIs)        (progDesc configCorrectorIsDesc)
configCorrectorAddInfo =       info (myHelper <*> configCorrectorAdd)       (progDesc configCorrectorAddDesc)
configCorrectorRemoveInfo =    info (myHelper <*> configCorrectorRemove)    (progDesc configCorrectorRemoveDesc)


config role = ConfigOpts  <$> subparser (
 command thresholdSub     (configThresholdInfo role) <>
 command termDateSub      (configTermDateInfo role) <>
 command projectDateSub   (configProjectDateInfo role) <>
 command acceptExecSub    (configAcceptExecInfo role) <>
 command timeLimitSub     (configTimeLimitInfo role) <>
 command spaceLimitSub    (configSpaceLimitInfo role) <>
 command adminGroupsSub   (configAdminGroupsInfo role) <>
 command teacherGroupsSub (configTeacherGroupsInfo role) <>
 command correctorSub     (configCorrectorInfo role))

configThreshold role = ConfigThreshold <$> ConfigThresholdOpts <$> subparser (
 hasConfigWriteRights role (command setSub  configThresholdSetInfo) <>
 hasConfigReadRights  role (command listSub configThresholdListInfo))

configThresholdSet = ConfigThresholdSet <$> (ConfigThresholdSetOpts
  <$> optional (strOption $ toMod configThresholdSetCurrentOpt <> metavar configThresholdSetCurrentMeta <> help configThresholdSetCurrentHelp)
  <*> optional (strOption $ toMod configThresholdSetChooseOpt  <> metavar configThresholdSetChooseMeta  <> help configThresholdSetChooseHelp))

configThresholdList = ConfigThresholdList <$> pure ConfigThresholdListOpts
  
configTermDate role = ConfigTermDate <$> ConfigTermDateOpts <$> subparser (
 hasConfigWriteRights role (command setSub  configTermDateSetInfo) <>
 hasConfigReadRights  role (command listSub configTermDateListInfo))

configTermDateSet = ConfigTermDateSet <$> (ConfigTermDateSetOpts
  <$> optional (strOption $ toMod configTermDateSetTerm1Opt <> metavar configTermDateSetTerm1Meta <> help configTermDateSetTerm1Help)
  <*> optional (strOption $ toMod configTermDateSetTerm2Opt <> metavar configTermDateSetTerm2Meta <> help configTermDateSetTerm2Help)
  <*> optional (strOption $ toMod configTermDateSetTerm3Opt <> metavar configTermDateSetTerm3Meta <> help configTermDateSetTerm3Help))

configTermDateList = ConfigTermDateList <$> pure ConfigTermDateListOpts

configProjectDate role = ConfigProjectDate <$> ConfigProjectDateOpts <$> subparser (
 hasConfigWriteRights role (command setSub  configProjectDateSetInfo) <>
 hasConfigReadRights  role (command listSub configProjectDateListInfo))

configProjectDateSet = ConfigProjectDateSet <$> (ConfigProjectDateSetOpts
  <$> optional (strOption $ toMod configProjectDateSetEndOpt  <> metavar configProjectDateSetEndMeta  <> help configProjectDateSetEndHelp)
  <*> optional (strOption $ toMod configProjectDateSetLateOpt <> metavar configProjectDateSetLateMeta <> help configProjectDateSetLateHelp))

configProjectDateList = ConfigProjectDateList <$> pure ConfigProjectDateListOpts

configAcceptExec role = ConfigAcceptExec <$> ConfigAcceptExecOpts <$> subparser (
 hasConfigWriteRights role (command setSub  configAcceptExecSetInfo) <>
 hasConfigReadRights  role (command listSub configAcceptExecListInfo))

configAcceptExecSet = ConfigAcceptExecSet <$> (ConfigAcceptExecSetOpts
  <$> argument str (metavar configAcceptExecSetMeta <> help configAcceptExecSetHelp))

configAcceptExecList = ConfigAcceptExecList <$> pure ConfigAcceptExecListOpts

configTimeLimit role = ConfigTimeLimit <$> ConfigTimeLimitOpts <$> subparser (
 hasConfigWriteRights role (command setSub  configTimeLimitSetInfo) <>
 hasConfigReadRights  role (command listSub configTimeLimitListInfo))

configTimeLimitSet = ConfigTimeLimitSet <$> (ConfigTimeLimitSetOpts
  <$> argument str (metavar configTimeLimitSetMeta <> help configTimeLimitSetHelp))

configTimeLimitList = ConfigTimeLimitList <$> pure ConfigTimeLimitListOpts

configSpaceLimit role = ConfigSpaceLimit <$> ConfigSpaceLimitOpts <$> subparser (
 hasConfigWriteRights role (command setSub  configSpaceLimitSetInfo) <>
 hasConfigReadRights  role (command listSub configSpaceLimitListInfo))

configSpaceLimitSet = ConfigSpaceLimitSet <$> (ConfigSpaceLimitSetOpts
  <$> argument str (metavar configSpaceLimitSetMeta <> help configSpaceLimitSetHelp))

configSpaceLimitList = ConfigSpaceLimitList <$> pure ConfigSpaceLimitListOpts

configAdminGroups role = ConfigAdminGroups <$> ConfigAdminGroupsOpts <$> subparser (
 hasConfigWriteRights role (command setSub  configAdminGroupsSetInfo) <>
 hasConfigReadRights  role (command listSub configAdminGroupsListInfo))

configAdminGroupsSet = ConfigAdminGroupsSet <$> (ConfigAdminGroupsSetOpts
  <$> some (argument str (metavar configAdminGroupsSetMeta <> help configAdminGroupsSetHelp)))

configAdminGroupsList = ConfigAdminGroupsList <$> pure ConfigAdminGroupsListOpts

configTeacherGroups role = ConfigTeacherGroups <$> ConfigTeacherGroupsOpts <$> subparser (
 hasConfigWriteRights role (command setSub  configTeacherGroupsSetInfo) <>
 hasConfigReadRights  role (command listSub configTeacherGroupsListInfo))

configTeacherGroupsSet = ConfigTeacherGroupsSet <$> (ConfigTeacherGroupsSetOpts
  <$> many (argument str (metavar configTeacherGroupsSetMeta <> help configTeacherGroupsSetHelp)))

configTeacherGroupsList = ConfigTeacherGroupsList <$> pure ConfigTeacherGroupsListOpts

configCorrector role = ConfigCorrector <$> ConfigCorrectorOpts <$> subparser (
 hasConfigReadRights  role (command isSub     configCorrectorIsInfo) <>
 hasConfigReadRights role (command addSub    configCorrectorAddInfo) <>
 hasConfigReadRights role (command removeSub configCorrectorRemoveInfo))

configCorrectorIs = ConfigCorrectorIs <$> (ConfigCorrectorIsOpts
 <$> argument str (metavar configCorrectorIsMeta <> help configCorrectorIsHelp))

configCorrectorAdd = ConfigCorrectorAdd <$> (ConfigCorrectorAddOpts
 <$> argument str (metavar configCorrectorAddMeta <> help configCorrectorAddHelp))

configCorrectorRemove = ConfigCorrectorRemove <$> (ConfigCorrectorRemoveOpts
 <$> argument str (metavar configCorrectorRemoveMeta <> help configCorrectorRemoveHelp))

 