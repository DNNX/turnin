module Interface.CommandLineParser where

import Options.Applicative
import Interface.CommandLineLexicon

data Global = Global Cmd                                                           deriving (Show)
data Cmd  = Config    ConfigOpts                                                   deriving (Show)
                                                                                   
-- config                                                                          
data ConfigOpts = ConfigOpts          ConfigCmd                                    deriving (Show)
data ConfigCmd  = ConfigThreshold     ConfigThresholdOpts                          
                | ConfigTermDate      ConfigTermDateOpts                           
                | ConfigProjectDate   ConfigProjectDateOpts                        
                | ConfigAcceptExec    ConfigAcceptExecOpts                         
                | ConfigTimeLimit     ConfigTimeLimitOpts                          
                | ConfigSpaceLimit    ConfigSpaceLimitOpts                         
                | ConfigAdminGroups   ConfigAdminGroupsOpts                        
                | ConfigTeacherGroups ConfigTeacherGroupsOpts                      deriving (Show)
                                                                                   
data ConfigThresholdOpts = ConfigThresholdOpts ConfigThresholdCmd                  deriving (Show)
data ConfigThresholdCmd  = ConfigThresholdSet  ConfigThresholdSetOpts              
                         | ConfigThresholdList ConfigThresholdListOpts             deriving (Show)
                                                                                   
data ConfigThresholdSetOpts = ConfigThresholdSetOpts                               
 { configThresholdSetCurrent :: Maybe String                                       
 , configThresholdSetChoose  :: Maybe String }                                     deriving (Show)
data ConfigThresholdListOpts = ConfigThresholdListOpts                             deriving (Show)
                                                                                   
data ConfigTermDateOpts = ConfigTermDateOpts ConfigTermDateCmd                     deriving (Show)
data ConfigTermDateCmd  = ConfigTermDateSet  ConfigTermDateSetOpts                 
                        | ConfigTermDateList ConfigTermDateListOpts                deriving (Show)
                                                                                   
data ConfigTermDateSetOpts = ConfigTermDateSetOpts                                 
 { configTermDateSetTerm1 :: Maybe String                                          
 , configTermDateSetTerm2 :: Maybe String                                          
 , configTermDateSetTerm3 :: Maybe String }                                        deriving (Show)
data ConfigTermDateListOpts = ConfigTermDateListOpts                               deriving (Show)
                                                                                   
data ConfigProjectDateOpts = ConfigProjectDateOpts ConfigProjectDateCmd            deriving (Show)
data ConfigProjectDateCmd  = ConfigProjectDateSet  ConfigProjectDateSetOpts        
                           | ConfigProjectDateList ConfigProjectDateListOpts       deriving (Show)
                                                                                   
data ConfigProjectDateSetOpts = ConfigProjectDateSetOpts                           
 { configProjectDateSetEnd  :: Maybe String                                        
 , configProjectDateSetLate :: Maybe String }                                      deriving (Show)
data ConfigProjectDateListOpts = ConfigProjectDateListOpts                         deriving (Show)
                                                                                   
data ConfigAcceptExecOpts = ConfigAcceptExecOpts ConfigAcceptExecCmd               deriving (Show)
data ConfigAcceptExecCmd  = ConfigAcceptExecSet  ConfigAcceptExecSetOpts           
                          | ConfigAcceptExecList ConfigAcceptExecListOpts          deriving (Show)
                                                                                   
data ConfigAcceptExecSetOpts = ConfigAcceptExecSetOpts                             
 { configAcceptExecSetVal  :: String }                                             deriving (Show)
data ConfigAcceptExecListOpts = ConfigAcceptExecListOpts                           deriving (Show)
                                                                                   
data ConfigTimeLimitOpts = ConfigTimeLimitOpts ConfigTimeLimitCmd                  deriving (Show)
data ConfigTimeLimitCmd  = ConfigTimeLimitSet  ConfigTimeLimitSetOpts              
                         | ConfigTimeLimitList ConfigTimeLimitListOpts             deriving (Show)
                                                                                   
data ConfigTimeLimitSetOpts = ConfigTimeLimitSetOpts                               
 { configTimeLimitSetVal  :: String }                                              deriving (Show)
data ConfigTimeLimitListOpts = ConfigTimeLimitListOpts                             deriving (Show)
                                                                                   
data ConfigSpaceLimitOpts = ConfigSpaceLimitOpts ConfigSpaceLimitCmd               deriving (Show)
data ConfigSpaceLimitCmd  = ConfigSpaceLimitSet  ConfigSpaceLimitSetOpts           
                          | ConfigSpaceLimitList ConfigSpaceLimitListOpts          deriving (Show)
                                                                                   
data ConfigSpaceLimitSetOpts = ConfigSpaceLimitSetOpts                             
 { configSpaceLimitSetVal  :: String }                                             deriving (Show)
data ConfigSpaceLimitListOpts = ConfigSpaceLimitListOpts                           deriving (Show)
                                                                                   
data ConfigAdminGroupsOpts = ConfigAdminGroupsOpts ConfigAdminGroupsCmd            deriving (Show)
data ConfigAdminGroupsCmd  = ConfigAdminGroupsSet  ConfigAdminGroupsSetOpts        
                           | ConfigAdminGroupsList ConfigAdminGroupsListOpts       deriving (Show)
                                                                                   
data ConfigAdminGroupsSetOpts = ConfigAdminGroupsSetOpts                           
 { configAdminGroupsSetGroups  :: [String] }                                       deriving (Show)
data ConfigAdminGroupsListOpts = ConfigAdminGroupsListOpts                         deriving (Show)
                                                                                   
data ConfigTeacherGroupsOpts = ConfigTeacherGroupsOpts ConfigTeacherGroupsCmd      deriving (Show)
data ConfigTeacherGroupsCmd  = ConfigTeacherGroupsSet  ConfigTeacherGroupsSetOpts
                             | ConfigTeacherGroupsList ConfigTeacherGroupsListOpts deriving (Show)
 
data ConfigTeacherGroupsSetOpts = ConfigTeacherGroupsSetOpts
 { configTeacherGroupsSetGroups  :: [String] }                                     deriving (Show)
data ConfigTeacherGroupsListOpts = ConfigTeacherGroupsListOpts                     deriving (Show)
 
-- parser info
globalInfo =                  info (myHelper <*> global)                  (progDesc globalDesc <> header globalHeader)
                                                                          
configInfo =                  info (myHelper <*> config)                  (progDesc configDesc)
configThresholdInfo =         info (myHelper <*> configThreshold)         (progDesc configThresholdDesc)
configThresholdSetInfo =      info (myHelper <*> configThresholdSet)      (progDesc configThresholdSetDesc)
configThresholdListInfo =     info (myHelper <*> configThresholdList)     (progDesc configThresholdListDesc) 
configTermDateInfo =          info (myHelper <*> configTermDate)          (progDesc configTermDateDesc)
configTermDateSetInfo =       info (myHelper <*> configTermDateSet)       (progDesc configTermDateSetDesc)
configTermDateListInfo =      info (myHelper <*> configTermDateList)      (progDesc configTermDateListDesc)
configProjectDateInfo =       info (myHelper <*> configProjectDate)       (progDesc configProjectDateDesc)
configProjectDateSetInfo =    info (myHelper <*> configProjectDateSet)    (progDesc configProjectDateSetDesc)
configProjectDateListInfo =   info (myHelper <*> configProjectDateList)   (progDesc configProjectDateListDesc)
configAcceptExecInfo =        info (myHelper <*> configAcceptExec)        (progDesc configAcceptExecDesc)
configAcceptExecSetInfo =     info (myHelper <*> configAcceptExecSet)     (progDesc configAcceptExecSetDesc) 
configAcceptExecListInfo =    info (myHelper <*> configAcceptExecList)    (progDesc configAcceptExecListDesc)
configTimeLimitInfo =         info (myHelper <*> configTimeLimit)         (progDesc configTimeLimitDesc)
configTimeLimitSetInfo =      info (myHelper <*> configTimeLimitSet)      (progDesc configTimeLimitSetDesc) 
configTimeLimitListInfo =     info (myHelper <*> configTimeLimitList)     (progDesc configTimeLimitListDesc)
configSpaceLimitInfo =        info (myHelper <*> configSpaceLimit)        (progDesc configSpaceLimitDesc)
configSpaceLimitSetInfo =     info (myHelper <*> configSpaceLimitSet)     (progDesc configSpaceLimitSetDesc) 
configSpaceLimitListInfo =    info (myHelper <*> configSpaceLimitList)    (progDesc configSpaceLimitListDesc)
configAdminGroupsInfo =       info (myHelper <*> configAdminGroups)       (progDesc configAdminGroupsDesc)
configAdminGroupsSetInfo =    info (myHelper <*> configAdminGroupsSet)    (progDesc configAdminGroupsSetDesc) 
configAdminGroupsListInfo =   info (myHelper <*> configAdminGroupsList)   (progDesc configAdminGroupsListDesc)
configTeacherGroupsInfo =     info (myHelper <*> configTeacherGroups)     (progDesc configTeacherGroupsDesc)
configTeacherGroupsSetInfo =  info (myHelper <*> configTeacherGroupsSet)  (progDesc configTeacherGroupsSetDesc) 
configTeacherGroupsListInfo = info (myHelper <*> configTeacherGroupsList) (progDesc configTeacherGroupsListDesc)

-- Custom help command  
myHelper ::  Parser (a -> a)
myHelper = abortOption ShowHelpText $ toMod helpOpt <> help helpHelp 

-- parsers 
global = Global <$> subparser (
 command configSub configInfo)
 
config = Config <$> ConfigOpts  <$> subparser (
 command thresholdSub     configThresholdInfo <>
 command termDateSub      configTermDateInfo <>
 command projectDateSub   configProjectDateInfo <>
 command acceptExecSub    configAcceptExecInfo <>
 command timeLimitSub     configTimeLimitInfo <>
 command spaceLimitSub    configSpaceLimitInfo <>
 command adminGroupsSub   configAdminGroupsInfo <>
 command teacherGroupsSub configTeacherGroupsInfo)

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

configAcceptExec = ConfigAcceptExec <$> ConfigAcceptExecOpts <$> subparser (
 command setSub  configAcceptExecSetInfo <>
 command listSub configAcceptExecListInfo)
 
configAcceptExecSet = ConfigAcceptExecSet <$> (ConfigAcceptExecSetOpts
  <$> argument str (metavar configAcceptExecSetMeta  <> help configAcceptExecSetWhetherHelp))

configAcceptExecList = ConfigAcceptExecList <$> pure ConfigAcceptExecListOpts 

configTimeLimit = ConfigTimeLimit <$> ConfigTimeLimitOpts <$> subparser (
 command setSub  configTimeLimitSetInfo <>
 command listSub configTimeLimitListInfo)
 
configTimeLimitSet = ConfigTimeLimitSet <$> (ConfigTimeLimitSetOpts
  <$> argument str (metavar configTimeLimitSetMeta  <> help configTimeLimitSetSecondsHelp))

configTimeLimitList = ConfigTimeLimitList <$> pure ConfigTimeLimitListOpts 

configSpaceLimit = ConfigSpaceLimit <$> ConfigSpaceLimitOpts <$> subparser (
 command setSub  configSpaceLimitSetInfo <>
 command listSub configSpaceLimitListInfo)
 
configSpaceLimitSet = ConfigSpaceLimitSet <$> (ConfigSpaceLimitSetOpts
  <$> argument str (metavar configSpaceLimitSetMeta  <> help configSpaceLimitSetBytesHelp))

configSpaceLimitList = ConfigSpaceLimitList <$> pure ConfigSpaceLimitListOpts 

configAdminGroups = ConfigAdminGroups <$> ConfigAdminGroupsOpts <$> subparser (
 command setSub  configAdminGroupsSetInfo <>
 command listSub configAdminGroupsListInfo)
 
configAdminGroupsSet = ConfigAdminGroupsSet <$> (ConfigAdminGroupsSetOpts
  <$> some (argument str (metavar configAdminGroupsSetMeta  <> help configAdminGroupsSetGroupsHelp)))

configAdminGroupsList = ConfigAdminGroupsList <$> pure ConfigAdminGroupsListOpts 
 
configTeacherGroups = ConfigTeacherGroups <$> ConfigTeacherGroupsOpts <$> subparser (
 command setSub  configTeacherGroupsSetInfo <>
 command listSub configTeacherGroupsListInfo)
 
configTeacherGroupsSet = ConfigTeacherGroupsSet <$> (ConfigTeacherGroupsSetOpts
  <$> many (argument str (metavar configTeacherGroupsSetMeta  <> help configTeacherGroupsSetGroupsHelp)))

configTeacherGroupsList = ConfigTeacherGroupsList <$> pure ConfigTeacherGroupsListOpts 

toMod (O s l) = short s <> long l              
