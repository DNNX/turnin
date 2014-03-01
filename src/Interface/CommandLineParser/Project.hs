module Interface.CommandLineParser.Project where

import Options.Applicative
import Interface.Lexicon
import Interface.CommandLineParser.Utils
import Interface.CommandLineParser.Project.Validate
import Interface.CommandLineParser.Project.Worktrain
import Interface.CommandLineParser.Project.Submit
import Security.SecurityManager

data ProjectOpts = ProjectOpts      ProjectCmd                deriving (Show, Eq)
data ProjectCmd  = ProjectAdd       ProjectAddOpts
                 | ProjectRemove    ProjectRemoveOpts
                 | ProjectList      ProjectListOpts
                 | ProjectDate      ProjectDateOpts
                 | ProjectValidate  ProjectValidateOpts
                 | ProjectWorktrain ProjectWorktrainOpts
                 | ProjectSubmit    ProjectSubmitOpts        deriving (Show, Eq)

data ProjectAddOpts = ProjectAddOpts
 { projectAddRepoNN   :: Maybe String
 , projectAddTermNN   :: Maybe String
 , projectAddCourseNN :: Maybe String
 , projectAddGroupNN  :: Maybe String
 , projectAddStart    :: Maybe String
 , projectAddEnd      :: Maybe String
 , projectAddLate     :: Maybe String
 , projectAddName     :: String       }                    deriving (Show, Eq)

data ProjectRemoveOpts = ProjectRemoveOpts
 { projectRemoveRepoNN    :: Maybe String
 , projectRemoveTermNN    :: Maybe String
 , projectRemoveCourseNN  :: Maybe String
 , projectRemoveGroupNN   :: Maybe String
 , projectRemoveProjectNN :: Maybe String }                 deriving (Show, Eq)

data ProjectListOpts = ProjectListOpts
 { projectListRepoNN   :: Maybe String
 , projectListTermNN   :: Maybe String
 , projectListCourseNN :: Maybe String
 , projectListGroupNN  :: Maybe String }                   deriving (Show, Eq)

data ProjectDateOpts = ProjectDateOpts ProjectDateCmd      deriving (Show, Eq)
data ProjectDateCmd  = ProjectDateSet  ProjectDateSetOpts
                     | ProjectDateList ProjectDateListOpts deriving (Show, Eq)

data ProjectDateSetOpts = ProjectDateSetOpts
 { projectDateSetRepoNN    :: Maybe String
 , projectDateSetTermNN    :: Maybe String
 , projectDateSetCourseNN  :: Maybe String
 , projectDateSetGroupNN   :: Maybe String
 , projectDateSetProjectNN :: Maybe String
 , projectDateSetStart     :: Maybe String
 , projectDateSetEnd       :: Maybe String
 , projectDateSetLate      :: Maybe String }               deriving (Show, Eq)

data ProjectDateListOpts = ProjectDateListOpts
 { projectDateListRepoNN    :: Maybe String
 , projectDateListTermNN    :: Maybe String
 , projectDateListCourseNN  :: Maybe String
 , projectDateListGroupNN   :: Maybe String
 , projectDateListProjectNN :: Maybe String }              deriving (Show, Eq)

projectInfo role =         info (myHelper <*> project role) (progDesc projectDesc)
projectAddInfo =       info (myHelper <*> projectAdd)       (progDesc projectAddDesc)
projectRemoveInfo =    info (myHelper <*> projectRemove)    (progDesc projectRemoveDesc)
projectListInfo =      info (myHelper <*> projectList)      (progDesc projectListDesc)
projectDateInfo role = info (myHelper <*> projectDate role) (progDesc projectDateDesc)
projectDateSetInfo =   info (myHelper <*> projectDateSet)   (progDesc projectDateSetDesc)
projectDateListInfo =  info (myHelper <*> projectDateList)  (progDesc projectDateListDesc)

project role = ProjectOpts <$> subparser (
 hasProjectWriteRights role (command addSub       projectAddInfo) <>
 hasProjectWriteRights role (command removeSub    projectRemoveInfo) <>
 hasProjectReadRights  role (command listSub      projectListInfo) <>
 command dateSub      (projectDateInfo role) <>
 command validateSub  (ProjectValidate <$> projectValidateInfo role) <>
 command worktrainSub (ProjectWorktrain <$> projectWorktrainInfo role) <>
 command submitSub    (ProjectSubmit <$> projectSubmitInfo role))

projectAdd = ProjectAdd <$> (ProjectAddOpts
 <$> optional (strOption $ toMod repoNodeOpt <> metavar repoNodeMeta <> help repoNodeHelp)
 <*> optional (strOption $ toMod termNodeOpt <> metavar termNodeMeta <> help termNodeHelp)
 <*> optional (strOption $ toMod courseNodeOpt <> metavar courseNodeMeta <> help courseNodeHelp)
 <*> optional (strOption $ toMod groupNodeOpt <> metavar groupNodeMeta <> help groupNodeHelp)
 <*> optional (strOption $ toMod projectAddStartOpt <> metavar projectAddStartMeta <> help projectAddStartHelp)
 <*> optional (strOption $ toMod projectAddEndOpt <> metavar projectAddEndMeta <> help projectAddEndHelp)
 <*> optional (strOption $ toMod projectAddLateOpt <> metavar projectAddLateMeta <> help projectAddLateHelp)
 <*> argument str (metavar projectAddNameMeta <> help projectAddNameHelp))

projectRemove = ProjectRemove <$> (ProjectRemoveOpts
 <$> optional (strOption $ toMod repoNodeOpt <> metavar repoNodeMeta <> help repoNodeHelp)
 <*> optional (strOption $ toMod termNodeOpt <> metavar termNodeMeta <> help termNodeHelp)
 <*> optional (strOption $ toMod courseNodeOpt <> metavar courseNodeMeta <> help courseNodeHelp)
 <*> optional (strOption $ toMod groupNodeOpt <> metavar groupNodeMeta <> help groupNodeHelp)
 <*> optional (strOption $ toMod projectNodeOpt <> metavar projectNodeMeta <> help projectNodeHelp))

projectList = ProjectList <$> (ProjectListOpts
 <$> optional (strOption $ toMod repoNodeOpt <> metavar repoNodeMeta <> help repoNodeHelp)
 <*> optional (strOption $ toMod termNodeOpt <> metavar termNodeMeta <> help termNodeHelp)
 <*> optional (strOption $ toMod courseNodeOpt <> metavar courseNodeMeta <> help courseNodeHelp)
 <*> optional (strOption $ toMod groupNodeOpt <> metavar groupNodeMeta <> help groupNodeHelp))

projectDate role = ProjectDate <$> ProjectDateOpts <$> subparser (
 hasProjectWriteRights role (command setSub  projectDateSetInfo) <>
 hasProjectReadRights  role (command listSub projectDateListInfo))

projectDateSet = ProjectDateSet <$> (ProjectDateSetOpts
 <$> optional (strOption $ toMod repoNodeOpt <> metavar repoNodeMeta <> help repoNodeHelp)
 <*> optional (strOption $ toMod termNodeOpt <> metavar termNodeMeta <> help termNodeHelp)
 <*> optional (strOption $ toMod courseNodeOpt <> metavar courseNodeMeta <> help courseNodeHelp)
 <*> optional (strOption $ toMod groupNodeOpt <> metavar groupNodeMeta <> help groupNodeHelp)
 <*> optional (strOption $ toMod projectNodeOpt <> metavar projectNodeMeta <> help projectNodeHelp)
 <*> optional (strOption $ toMod projectDateSetStartOpt <> metavar projectDateSetStartMeta <> help projectDateSetStartHelp)
 <*> optional (strOption $ toMod projectDateSetEndOpt <> metavar projectDateSetEndMeta <> help projectDateSetEndHelp)
 <*> optional (strOption $ toMod projectDateSetLateOpt <> metavar projectDateSetLateMeta <> help projectDateSetLateHelp))

projectDateList = ProjectDateList <$> (ProjectDateListOpts
 <$> optional (strOption $ toMod repoNodeOpt <> metavar repoNodeMeta <> help repoNodeHelp)
 <*> optional (strOption $ toMod termNodeOpt <> metavar termNodeMeta <> help termNodeHelp)
 <*> optional (strOption $ toMod courseNodeOpt <> metavar courseNodeMeta <> help courseNodeHelp)
 <*> optional (strOption $ toMod groupNodeOpt <> metavar groupNodeMeta <> help groupNodeHelp)
 <*> optional (strOption $ toMod projectNodeOpt <> metavar projectNodeMeta <> help projectNodeHelp))

 