module Interface.CommandLineParser.Project.Worktrain where

import Options.Applicative
import Interface.Lexicon
import Interface.CommandLineParser.Utils
import Security.SecurityManager

data ProjectWorktrainOpts = ProjectWorktrainOpts       ProjectWorktrainCmd            deriving (Show, Eq)
data ProjectWorktrainCmd  = ProjectWorktrainScript     ProjectWorktrainScriptOpts
                          | ProjectWorktrainFile       ProjectWorktrainFileOpts
                          | ProjectWorktrainTimeLimit  ProjectWorktrainTimeLimitOpts
                          | ProjectWorktrainSpaceLimit ProjectWorktrainSpaceLimitOpts deriving (Show, Eq)

data ProjectWorktrainScriptOpts = ProjectWorktrainScriptOpts    ProjectWorktrainScriptCmd deriving (Show, Eq)
data ProjectWorktrainScriptCmd  = ProjectWorktrainScriptSet     ProjectWorktrainScriptSetOpts
                                | ProjectWorktrainScriptUnset   ProjectWorktrainScriptUnsetOpts
                                | ProjectWorktrainScriptList    ProjectWorktrainScriptListOpts
                                | ProjectWorktrainScriptExtract ProjectWorktrainScriptExtractOpts deriving (Show, Eq)

data ProjectWorktrainScriptSetOpts = ProjectWorktrainScriptSetOpts
 { projectWorktrainScriptSetRepoNN    :: Maybe String
 , projectWorktrainScriptSetTermNN    :: Maybe String
 , projectWorktrainScriptSetCourseNN  :: Maybe String
 , projectWorktrainScriptSetGroupNN   :: Maybe String
 , projectWorktrainScriptSetProjectNN :: Maybe String
 , projectWorktrainScriptSetName      :: String       } deriving (Show, Eq)

data ProjectWorktrainScriptUnsetOpts = ProjectWorktrainScriptUnsetOpts
 { projectWorktrainScriptUnsetRepoNN    :: Maybe String
 , projectWorktrainScriptUnsetTermNN    :: Maybe String
 , projectWorktrainScriptUnsetCourseNN  :: Maybe String
 , projectWorktrainScriptUnsetGroupNN   :: Maybe String
 , projectWorktrainScriptUnsetProjectNN :: Maybe String } deriving (Show, Eq)

data ProjectWorktrainScriptListOpts = ProjectWorktrainScriptListOpts
 { projectWorktrainScriptListRepoNN    :: Maybe String
 , projectWorktrainScriptListTermNN    :: Maybe String
 , projectWorktrainScriptListCourseNN  :: Maybe String
 , projectWorktrainScriptListGroupNN   :: Maybe String
 , projectWorktrainScriptListProjectNN :: Maybe String } deriving (Show, Eq)

data ProjectWorktrainScriptExtractOpts = ProjectWorktrainScriptExtractOpts
 { projectWorktrainScriptExtractRepoNN    :: Maybe String
 , projectWorktrainScriptExtractTermNN    :: Maybe String
 , projectWorktrainScriptExtractCourseNN  :: Maybe String
 , projectWorktrainScriptExtractGroupNN   :: Maybe String
 , projectWorktrainScriptExtractProjectNN :: Maybe String
 , projectWorktrainScriptExtractDir       :: String       } deriving (Show, Eq)

data ProjectWorktrainFileOpts = ProjectWorktrainFileOpts    ProjectWorktrainFileCmd deriving (Show, Eq)
data ProjectWorktrainFileCmd  = ProjectWorktrainFileAdd     ProjectWorktrainFileAddOpts
                              | ProjectWorktrainFileRemove  ProjectWorktrainFileRemoveOpts
                              | ProjectWorktrainFileList    ProjectWorktrainFileListOpts
                              | ProjectWorktrainFileExtract ProjectWorktrainFileExtractOpts deriving (Show, Eq)

data ProjectWorktrainFileAddOpts = ProjectWorktrainFileAddOpts
 { projectWorktrainFileAddRepoNN    :: Maybe String
 , projectWorktrainFileAddTermNN    :: Maybe String
 , projectWorktrainFileAddCourseNN  :: Maybe String
 , projectWorktrainFileAddGroupNN   :: Maybe String
 , projectWorktrainFileAddProjectNN :: Maybe String
 , projectWorktrainFileAddNames     :: [String]     } deriving (Show, Eq)

data ProjectWorktrainFileRemoveOpts = ProjectWorktrainFileRemoveOpts
 { projectWorktrainFileRemoveRepoNN    :: Maybe String
 , projectWorktrainFileRemoveTermNN    :: Maybe String
 , projectWorktrainFileRemoveCourseNN  :: Maybe String
 , projectWorktrainFileRemoveGroupNN   :: Maybe String
 , projectWorktrainFileRemoveProjectNN :: Maybe String
 , projectWorktrainFileRemoveNames     :: [String]     } deriving (Show, Eq)

data ProjectWorktrainFileListOpts = ProjectWorktrainFileListOpts
 { projectWorktrainFileListRepoNN    :: Maybe String
 , projectWorktrainFileListTermNN    :: Maybe String
 , projectWorktrainFileListCourseNN  :: Maybe String
 , projectWorktrainFileListGroupNN   :: Maybe String
 , projectWorktrainFileListProjectNN :: Maybe String } deriving (Show, Eq)

data ProjectWorktrainFileExtractOpts = ProjectWorktrainFileExtractOpts
 { projectWorktrainFileExtractRepoNN    :: Maybe String
 , projectWorktrainFileExtractTermNN    :: Maybe String
 , projectWorktrainFileExtractCourseNN  :: Maybe String
 , projectWorktrainFileExtractGroupNN   :: Maybe String
 , projectWorktrainFileExtractProjectNN :: Maybe String
 , projectWorktrainFileExtractDir       :: String
 , projectWorktrainFileExtractNames     :: [String]     } deriving (Show, Eq)

data ProjectWorktrainTimeLimitOpts = ProjectWorktrainTimeLimitOpts ProjectWorktrainTimeLimitCmd      deriving (Show, Eq)
data ProjectWorktrainTimeLimitCmd  = ProjectWorktrainTimeLimitSet  ProjectWorktrainTimeLimitSetOpts
                                   | ProjectWorktrainTimeLimitList ProjectWorktrainTimeLimitListOpts deriving (Show, Eq)

data ProjectWorktrainTimeLimitSetOpts = ProjectWorktrainTimeLimitSetOpts
 { projectWorktrainTimeLimitSetRepoNN    :: Maybe String
 , projectWorktrainTimeLimitSetTermNN    :: Maybe String
 , projectWorktrainTimeLimitSetCourseNN  :: Maybe String
 , projectWorktrainTimeLimitSetGroupNN   :: Maybe String
 , projectWorktrainTimeLimitSetProjectNN :: Maybe String
 , projectWorktrainTimeLimitSetVal       :: String       } deriving (Show, Eq)

data ProjectWorktrainTimeLimitListOpts = ProjectWorktrainTimeLimitListOpts
 { projectWorktrainTimeLimitListRepoNN    :: Maybe String
 , projectWorktrainTimeLimitListTermNN    :: Maybe String
 , projectWorktrainTimeLimitListCourseNN  :: Maybe String
 , projectWorktrainTimeLimitListGroupNN   :: Maybe String
 , projectWorktrainTimeLimitListProjectNN :: Maybe String } deriving (Show, Eq)

data ProjectWorktrainSpaceLimitOpts = ProjectWorktrainSpaceLimitOpts ProjectWorktrainSpaceLimitCmd      deriving (Show, Eq)
data ProjectWorktrainSpaceLimitCmd  = ProjectWorktrainSpaceLimitSet  ProjectWorktrainSpaceLimitSetOpts
                                    | ProjectWorktrainSpaceLimitList ProjectWorktrainSpaceLimitListOpts deriving (Show, Eq)

data ProjectWorktrainSpaceLimitSetOpts = ProjectWorktrainSpaceLimitSetOpts
 { projectWorktrainSpaceLimitSetRepoNN    :: Maybe String
 , projectWorktrainSpaceLimitSetTermNN    :: Maybe String
 , projectWorktrainSpaceLimitSetCourseNN  :: Maybe String
 , projectWorktrainSpaceLimitSetGroupNN   :: Maybe String
 , projectWorktrainSpaceLimitSetProjectNN :: Maybe String
 , projectWorktrainSpaceLimitSetVal       :: String       } deriving (Show, Eq)

data ProjectWorktrainSpaceLimitListOpts = ProjectWorktrainSpaceLimitListOpts
 { projectWorktrainSpaceLimitListRepoNN    :: Maybe String
 , projectWorktrainSpaceLimitListTermNN    :: Maybe String
 , projectWorktrainSpaceLimitListCourseNN  :: Maybe String
 , projectWorktrainSpaceLimitListGroupNN   :: Maybe String
 , projectWorktrainSpaceLimitListProjectNN :: Maybe String } deriving (Show, Eq)

projectWorktrainInfo role =           info (myHelper <*> projectWorktrain role)           (progDesc projectWorktrainDesc)
projectWorktrainScriptInfo role =     info (myHelper <*> projectWorktrainScript role)     (progDesc projectWorktrainScriptDesc)
projectWorktrainScriptSetInfo =       info (myHelper <*> projectWorktrainScriptSet)       (progDesc projectWorktrainScriptSetDesc)
projectWorktrainScriptUnsetInfo =     info (myHelper <*> projectWorktrainScriptUnset)     (progDesc projectWorktrainScriptUnsetDesc)
projectWorktrainScriptListInfo =      info (myHelper <*> projectWorktrainScriptList)      (progDesc projectWorktrainScriptListDesc)
projectWorktrainScriptExtractInfo =   info (myHelper <*> projectWorktrainScriptExtract)   (progDesc projectWorktrainScriptExtractDesc)
projectWorktrainFileInfo role =       info (myHelper <*> projectWorktrainFile role)       (progDesc projectWorktrainFileDesc)
projectWorktrainFileAddInfo =         info (myHelper <*> projectWorktrainFileAdd)         (progDesc projectWorktrainFileAddDesc)
projectWorktrainFileRemoveInfo =      info (myHelper <*> projectWorktrainFileRemove)      (progDesc projectWorktrainFileRemoveDesc)
projectWorktrainFileListInfo =        info (myHelper <*> projectWorktrainFileList)        (progDesc projectWorktrainFileListDesc)
projectWorktrainFileExtractInfo =     info (myHelper <*> projectWorktrainFileExtract)     (progDesc projectWorktrainFileExtractDesc)
projectWorktrainTimeLimitInfo role =  info (myHelper <*> projectWorktrainTimeLimit role)  (progDesc projectWorktrainTimeLimitDesc)
projectWorktrainTimeLimitSetInfo =    info (myHelper <*> projectWorktrainTimeLimitSet)    (progDesc projectWorktrainTimeLimitSetDesc)
projectWorktrainTimeLimitListInfo =   info (myHelper <*> projectWorktrainTimeLimitList)   (progDesc projectWorktrainTimeLimitListDesc)
projectWorktrainSpaceLimitInfo role = info (myHelper <*> projectWorktrainSpaceLimit role) (progDesc projectWorktrainSpaceLimitDesc)
projectWorktrainSpaceLimitSetInfo =   info (myHelper <*> projectWorktrainSpaceLimitSet)   (progDesc projectWorktrainSpaceLimitSetDesc)
projectWorktrainSpaceLimitListInfo =  info (myHelper <*> projectWorktrainSpaceLimitList)  (progDesc projectWorktrainSpaceLimitListDesc)

projectWorktrain role = ProjectWorktrainOpts <$> subparser (
 command scriptSub     (projectWorktrainScriptInfo role) <>
 command fileSub       (projectWorktrainFileInfo role) <>
 command timeLimitSub  (projectWorktrainTimeLimitInfo role)<>
 command spaceLimitSub (projectWorktrainSpaceLimitInfo role))

projectWorktrainScript role = ProjectWorktrainScript <$> ProjectWorktrainScriptOpts <$> subparser (
 hasProjectWriteRights role (command setSub     projectWorktrainScriptSetInfo) <>
 hasProjectWriteRights role (command unsetSub   projectWorktrainScriptUnsetInfo) <>
 hasProjectReadRights  role (command listSub    projectWorktrainScriptListInfo) <>
 hasProjectReadRights  role (command extractSub projectWorktrainScriptExtractInfo))

projectWorktrainScriptSet = ProjectWorktrainScriptSet <$> (ProjectWorktrainScriptSetOpts
 <$> optional (strOption $ toMod repoNodeOpt <> metavar repoNodeMeta <> help repoNodeHelp)
 <*> optional (strOption $ toMod termNodeOpt <> metavar termNodeMeta <> help termNodeHelp)
 <*> optional (strOption $ toMod courseNodeOpt <> metavar courseNodeMeta <> help courseNodeHelp)
 <*> optional (strOption $ toMod groupNodeOpt <> metavar groupNodeMeta <> help groupNodeHelp)
 <*> optional (strOption $ toMod projectNodeOpt <> metavar projectNodeMeta <> help projectNodeHelp)
 <*> argument str (metavar projectWorktrainScriptSetMeta <> help projectWorktrainScriptSetHelp))

projectWorktrainScriptUnset = ProjectWorktrainScriptUnset <$> (ProjectWorktrainScriptUnsetOpts
 <$> optional (strOption $ toMod repoNodeOpt <> metavar repoNodeMeta <> help repoNodeHelp)
 <*> optional (strOption $ toMod termNodeOpt <> metavar termNodeMeta <> help termNodeHelp)
 <*> optional (strOption $ toMod courseNodeOpt <> metavar courseNodeMeta <> help courseNodeHelp)
 <*> optional (strOption $ toMod groupNodeOpt <> metavar groupNodeMeta <> help groupNodeHelp)
 <*> optional (strOption $ toMod projectNodeOpt <> metavar projectNodeMeta <> help projectNodeHelp))

projectWorktrainScriptList = ProjectWorktrainScriptList <$> (ProjectWorktrainScriptListOpts
 <$> optional (strOption $ toMod repoNodeOpt <> metavar repoNodeMeta <> help repoNodeHelp)
 <*> optional (strOption $ toMod termNodeOpt <> metavar termNodeMeta <> help termNodeHelp)
 <*> optional (strOption $ toMod courseNodeOpt <> metavar courseNodeMeta <> help courseNodeHelp)
 <*> optional (strOption $ toMod groupNodeOpt <> metavar groupNodeMeta <> help groupNodeHelp)
 <*> optional (strOption $ toMod projectNodeOpt <> metavar projectNodeMeta <> help projectNodeHelp))

projectWorktrainScriptExtract = ProjectWorktrainScriptExtract <$> (ProjectWorktrainScriptExtractOpts
 <$> optional (strOption $ toMod repoNodeOpt <> metavar repoNodeMeta <> help repoNodeHelp)
 <*> optional (strOption $ toMod termNodeOpt <> metavar termNodeMeta <> help termNodeHelp)
 <*> optional (strOption $ toMod courseNodeOpt <> metavar courseNodeMeta <> help courseNodeHelp)
 <*> optional (strOption $ toMod groupNodeOpt <> metavar groupNodeMeta <> help groupNodeHelp)
 <*> optional (strOption $ toMod projectNodeOpt <> metavar projectNodeMeta <> help projectNodeHelp)
 <*> argument str (metavar projectWorktrainScriptExtractMeta <> help projectWorktrainScriptExtractHelp))

projectWorktrainFile role = ProjectWorktrainFile <$> ProjectWorktrainFileOpts <$> subparser (
 hasProjectWriteRights role (command addSub     projectWorktrainFileAddInfo) <>
 hasProjectWriteRights role (command removeSub  projectWorktrainFileRemoveInfo) <>
 hasProjectReadRights  role (command listSub    projectWorktrainFileListInfo) <>
 hasProjectReadRights  role (command extractSub projectWorktrainFileExtractInfo))

projectWorktrainFileAdd = ProjectWorktrainFileAdd <$> (ProjectWorktrainFileAddOpts
 <$> optional (strOption $ toMod repoNodeOpt <> metavar repoNodeMeta <> help repoNodeHelp)
 <*> optional (strOption $ toMod termNodeOpt <> metavar termNodeMeta <> help termNodeHelp)
 <*> optional (strOption $ toMod courseNodeOpt <> metavar courseNodeMeta <> help courseNodeHelp)
 <*> optional (strOption $ toMod groupNodeOpt <> metavar groupNodeMeta <> help groupNodeHelp)
 <*> optional (strOption $ toMod projectNodeOpt <> metavar projectNodeMeta <> help projectNodeHelp)
 <*> some (argument str (metavar projectWorktrainFileAddMeta <> help projectWorktrainFileAddHelp)))

projectWorktrainFileRemove = ProjectWorktrainFileRemove <$> (ProjectWorktrainFileRemoveOpts
 <$> optional (strOption $ toMod repoNodeOpt <> metavar repoNodeMeta <> help repoNodeHelp)
 <*> optional (strOption $ toMod termNodeOpt <> metavar termNodeMeta <> help termNodeHelp)
 <*> optional (strOption $ toMod courseNodeOpt <> metavar courseNodeMeta <> help courseNodeHelp)
 <*> optional (strOption $ toMod groupNodeOpt <> metavar groupNodeMeta <> help groupNodeHelp)
 <*> optional (strOption $ toMod projectNodeOpt <> metavar projectNodeMeta <> help projectNodeHelp)
 <*> some (argument str (metavar projectWorktrainFileRemoveMeta <> help projectWorktrainFileRemoveHelp)))

projectWorktrainFileList = ProjectWorktrainFileList <$> (ProjectWorktrainFileListOpts
 <$> optional (strOption $ toMod repoNodeOpt <> metavar repoNodeMeta <> help repoNodeHelp)
 <*> optional (strOption $ toMod termNodeOpt <> metavar termNodeMeta <> help termNodeHelp)
 <*> optional (strOption $ toMod courseNodeOpt <> metavar courseNodeMeta <> help courseNodeHelp)
 <*> optional (strOption $ toMod groupNodeOpt <> metavar groupNodeMeta <> help groupNodeHelp)
 <*> optional (strOption $ toMod projectNodeOpt <> metavar projectNodeMeta <> help projectNodeHelp))

projectWorktrainFileExtract = ProjectWorktrainFileExtract <$> (ProjectWorktrainFileExtractOpts
 <$> optional (strOption $ toMod repoNodeOpt <> metavar repoNodeMeta <> help repoNodeHelp)
 <*> optional (strOption $ toMod termNodeOpt <> metavar termNodeMeta <> help termNodeHelp)
 <*> optional (strOption $ toMod courseNodeOpt <> metavar courseNodeMeta <> help courseNodeHelp)
 <*> optional (strOption $ toMod groupNodeOpt <> metavar groupNodeMeta <> help groupNodeHelp)
 <*> optional (strOption $ toMod projectNodeOpt <> metavar projectNodeMeta <> help projectNodeHelp)
 <*> argument str (metavar projectWorktrainFileExtractDirMeta <> help projectWorktrainFileExtractDirHelp)
 <*> some (argument str (metavar projectWorktrainFileExtractNamesMeta <> help projectWorktrainFileExtractNamesHelp)))

projectWorktrainTimeLimit role = ProjectWorktrainTimeLimit <$> ProjectWorktrainTimeLimitOpts <$> subparser (
 hasProjectWriteRights role (command setSub     projectWorktrainTimeLimitSetInfo) <>
 hasProjectReadRights role (command listSub    projectWorktrainTimeLimitListInfo))

projectWorktrainTimeLimitSet = ProjectWorktrainTimeLimitSet <$> (ProjectWorktrainTimeLimitSetOpts
 <$> optional (strOption $ toMod repoNodeOpt <> metavar repoNodeMeta <> help repoNodeHelp)
 <*> optional (strOption $ toMod termNodeOpt <> metavar termNodeMeta <> help termNodeHelp)
 <*> optional (strOption $ toMod courseNodeOpt <> metavar courseNodeMeta <> help courseNodeHelp)
 <*> optional (strOption $ toMod groupNodeOpt <> metavar groupNodeMeta <> help groupNodeHelp)
 <*> optional (strOption $ toMod projectNodeOpt <> metavar projectNodeMeta <> help projectNodeHelp)
 <*> argument str (metavar projectWorktrainTimeLimitSetMeta <> help projectWorktrainTimeLimitSetHelp))

projectWorktrainTimeLimitList = ProjectWorktrainTimeLimitList <$> (ProjectWorktrainTimeLimitListOpts
 <$> optional (strOption $ toMod repoNodeOpt <> metavar repoNodeMeta <> help repoNodeHelp)
 <*> optional (strOption $ toMod termNodeOpt <> metavar termNodeMeta <> help termNodeHelp)
 <*> optional (strOption $ toMod courseNodeOpt <> metavar courseNodeMeta <> help courseNodeHelp)
 <*> optional (strOption $ toMod groupNodeOpt <> metavar groupNodeMeta <> help groupNodeHelp)
 <*> optional (strOption $ toMod projectNodeOpt <> metavar projectNodeMeta <> help projectNodeHelp))

projectWorktrainSpaceLimit role = ProjectWorktrainSpaceLimit <$> ProjectWorktrainSpaceLimitOpts <$> subparser (
 hasProjectWriteRights role (command setSub     projectWorktrainSpaceLimitSetInfo) <>
 hasProjectReadRights  role (command listSub    projectWorktrainSpaceLimitListInfo))

projectWorktrainSpaceLimitSet = ProjectWorktrainSpaceLimitSet <$> (ProjectWorktrainSpaceLimitSetOpts
 <$> optional (strOption $ toMod repoNodeOpt <> metavar repoNodeMeta <> help repoNodeHelp)
 <*> optional (strOption $ toMod termNodeOpt <> metavar termNodeMeta <> help termNodeHelp)
 <*> optional (strOption $ toMod courseNodeOpt <> metavar courseNodeMeta <> help courseNodeHelp)
 <*> optional (strOption $ toMod groupNodeOpt <> metavar groupNodeMeta <> help groupNodeHelp)
 <*> optional (strOption $ toMod projectNodeOpt <> metavar projectNodeMeta <> help projectNodeHelp)
 <*> argument str (metavar projectWorktrainSpaceLimitSetMeta <> help projectWorktrainSpaceLimitSetHelp))

projectWorktrainSpaceLimitList = ProjectWorktrainSpaceLimitList <$> (ProjectWorktrainSpaceLimitListOpts
 <$> optional (strOption $ toMod repoNodeOpt <> metavar repoNodeMeta <> help repoNodeHelp)
 <*> optional (strOption $ toMod termNodeOpt <> metavar termNodeMeta <> help termNodeHelp)
 <*> optional (strOption $ toMod courseNodeOpt <> metavar courseNodeMeta <> help courseNodeHelp)
 <*> optional (strOption $ toMod groupNodeOpt <> metavar groupNodeMeta <> help groupNodeHelp)
 <*> optional (strOption $ toMod projectNodeOpt <> metavar projectNodeMeta <> help projectNodeHelp))






 