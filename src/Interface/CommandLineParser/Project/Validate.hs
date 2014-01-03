module Interface.CommandLineParser.Project.Validate where

import Options.Applicative
import Interface.Lexicon
import Interface.CommandLineParser.Utils

data ProjectValidateOpts = ProjectValidateOpts       ProjectValidateCmd                              deriving (Show, Eq)
data ProjectValidateCmd  = ProjectValidateAcceptExec ProjectValidateAcceptExecOpts
                         | ProjectValidateName       ProjectValidateNameOpts
                         | ProjectValidateCommand    ProjectValidateCommandOpts
                         | ProjectValidateScript     ProjectValidateScriptOpts                       deriving (Show, Eq)
                         
data ProjectValidateAcceptExecOpts = ProjectValidateAcceptExecOpts ProjectValidateAcceptExecCmd      deriving (Show, Eq)
data ProjectValidateAcceptExecCmd  = ProjectValidateAcceptExecSet  ProjectValidateAcceptExecSetOpts
                                   | ProjectValidateAcceptExecList ProjectValidateAcceptExecListOpts deriving (Show, Eq)
                         
data ProjectValidateAcceptExecSetOpts = ProjectValidateAcceptExecSetOpts
 { projectValidateAcceptExecSetRepoNN    :: Maybe String                    
 , projectValidateAcceptExecSetTermNN    :: Maybe String                    
 , projectValidateAcceptExecSetCourseNN  :: Maybe String                    
 , projectValidateAcceptExecSetGroupNN   :: Maybe String                    
 , projectValidateAcceptExecSetProjectNN :: Maybe String
 , projectValidateAcceptExecSetWhether   :: String       }                                           deriving (Show, Eq)                   
  
data ProjectValidateAcceptExecListOpts = ProjectValidateAcceptExecListOpts
 { projectValidateAcceptExecListRepoNN    :: Maybe String                    
 , projectValidateAcceptExecListTermNN    :: Maybe String                    
 , projectValidateAcceptExecListCourseNN  :: Maybe String                    
 , projectValidateAcceptExecListGroupNN   :: Maybe String                    
 , projectValidateAcceptExecListProjectNN :: Maybe String }                                          deriving (Show, Eq)                   
                         
data ProjectValidateNameOpts = ProjectValidateNameOpts   ProjectValidateNameCmd                      deriving (Show, Eq)
data ProjectValidateNameCmd  = ProjectValidateNameAdd    ProjectValidateNameAddOpts
                             | ProjectValidateNameRemove ProjectValidateNameRemoveOpts 
                             | ProjectValidateNameList   ProjectValidateNameListOpts                 deriving (Show, Eq)                     
                                     
data ProjectValidateNameAddOpts = ProjectValidateNameAddOpts
 { projectValidateNameAddRepoNN    :: Maybe String                    
 , projectValidateNameAddTermNN    :: Maybe String                    
 , projectValidateNameAddCourseNN  :: Maybe String                    
 , projectValidateNameAddGroupNN   :: Maybe String                    
 , projectValidateNameAddProjectNN :: Maybe String
 , projectValidateNameAddNames     :: [String]     }                                                 deriving (Show, Eq)                   
  
data ProjectValidateNameRemoveOpts = ProjectValidateNameRemoveOpts
 { projectValidateNameRemoveRepoNN    :: Maybe String                    
 , projectValidateNameRemoveTermNN    :: Maybe String                    
 , projectValidateNameRemoveCourseNN  :: Maybe String                    
 , projectValidateNameRemoveGroupNN   :: Maybe String                    
 , projectValidateNameRemoveProjectNN :: Maybe String
 , projectValidateNameRemoveNames     :: [String]     }                                              deriving (Show, Eq)                   
                   
data ProjectValidateNameListOpts = ProjectValidateNameListOpts
 { projectValidateNameListRepoNN    :: Maybe String                    
 , projectValidateNameListTermNN    :: Maybe String                    
 , projectValidateNameListCourseNN  :: Maybe String                    
 , projectValidateNameListGroupNN   :: Maybe String                    
 , projectValidateNameListProjectNN :: Maybe String }                                                deriving (Show, Eq)                   
                                  
data ProjectValidateCommandOpts = ProjectValidateCommandOpts  ProjectValidateCommandCmd              deriving (Show, Eq)
data ProjectValidateCommandCmd  = ProjectValidateCommandSet   ProjectValidateCommandSetOpts
                                | ProjectValidateCommandUnset ProjectValidateCommandUnsetOpts 
                                | ProjectValidateCommandList  ProjectValidateCommandListOpts         deriving (Show, Eq)                     
                                       
data ProjectValidateCommandSetOpts = ProjectValidateCommandSetOpts
 { projectValidateCommandSetRepoNN    :: Maybe String                    
 , projectValidateCommandSetTermNN    :: Maybe String                    
 , projectValidateCommandSetCourseNN  :: Maybe String                    
 , projectValidateCommandSetGroupNN   :: Maybe String                    
 , projectValidateCommandSetProjectNN :: Maybe String
 , projectValidateCommandSetCommand   :: String     }                                              deriving (Show, Eq)                   
  
data ProjectValidateCommandUnsetOpts = ProjectValidateCommandUnsetOpts
 { projectValidateCommandUnsetRepoNN    :: Maybe String                    
 , projectValidateCommandUnsetTermNN    :: Maybe String                    
 , projectValidateCommandUnsetCourseNN  :: Maybe String                    
 , projectValidateCommandUnsetGroupNN   :: Maybe String                    
 , projectValidateCommandUnsetProjectNN :: Maybe String }                                            deriving (Show, Eq)                   
                   
data ProjectValidateCommandListOpts = ProjectValidateCommandListOpts
 { projectValidateCommandListRepoNN    :: Maybe String                    
 , projectValidateCommandListTermNN    :: Maybe String                    
 , projectValidateCommandListCourseNN  :: Maybe String                    
 , projectValidateCommandListGroupNN   :: Maybe String                    
 , projectValidateCommandListProjectNN :: Maybe String }                                             deriving (Show, Eq)                   
                                 
data ProjectValidateScriptOpts = ProjectValidateScriptOpts    ProjectValidateScriptCmd               deriving (Show, Eq)
data ProjectValidateScriptCmd  = ProjectValidateScriptSet     ProjectValidateScriptSetOpts
                               | ProjectValidateScriptUnset   ProjectValidateScriptUnsetOpts 
                               | ProjectValidateScriptList    ProjectValidateScriptListOpts
                               | ProjectValidateScriptExtract ProjectValidateScriptExtractOpts       deriving (Show, Eq)                     

data ProjectValidateScriptSetOpts = ProjectValidateScriptSetOpts
 { projectValidateScriptSetRepoNN     :: Maybe String                    
 , projectValidateScriptSetTermNN     :: Maybe String                    
 , projectValidateScriptSetCourseNN   :: Maybe String                    
 , projectValidateScriptSetGroupNN    :: Maybe String                    
 , projectValidateScriptSetProjectNN  :: Maybe String
 , projectValidateScriptSetScriptName :: String       }                                              deriving (Show, Eq)                   
  
data ProjectValidateScriptUnsetOpts = ProjectValidateScriptUnsetOpts
 { projectValidateScriptUnsetRepoNN    :: Maybe String                    
 , projectValidateScriptUnsetTermNN    :: Maybe String                    
 , projectValidateScriptUnsetCourseNN  :: Maybe String                    
 , projectValidateScriptUnsetGroupNN   :: Maybe String                    
 , projectValidateScriptUnsetProjectNN :: Maybe String }                                             deriving (Show, Eq)                   
                   
data ProjectValidateScriptListOpts = ProjectValidateScriptListOpts
 { projectValidateScriptListRepoNN    :: Maybe String                    
 , projectValidateScriptListTermNN    :: Maybe String                    
 , projectValidateScriptListCourseNN  :: Maybe String                    
 , projectValidateScriptListGroupNN   :: Maybe String                    
 , projectValidateScriptListProjectNN :: Maybe String }                                              deriving (Show, Eq)                   
                            
data ProjectValidateScriptExtractOpts = ProjectValidateScriptExtractOpts
 { projectValidateScriptExtractRepoNN     :: Maybe String                    
 , projectValidateScriptExtractTermNN     :: Maybe String                    
 , projectValidateScriptExtractCourseNN   :: Maybe String                    
 , projectValidateScriptExtractGroupNN    :: Maybe String                    
 , projectValidateScriptExtractProjectNN  :: Maybe String
 , projectValidateScriptExtractScriptName :: String       }                                          deriving (Show, Eq)                   
                   
projectValidateInfo =               info (myHelper <*> projectValidate)               (progDesc projectValidateDesc)
projectValidateAcceptExecInfo =     info (myHelper <*> projectValidateAcceptExec)     (progDesc projectValidateAcceptExecDesc)
projectValidateAcceptExecSetInfo =  info (myHelper <*> projectValidateAcceptExecSet)  (progDesc projectValidateAcceptExecSetDesc)
projectValidateAcceptExecListInfo = info (myHelper <*> projectValidateAcceptExecList) (progDesc projectValidateAcceptExecListDesc)
projectValidateNameInfo =           info (myHelper <*> projectValidateName)           (progDesc projectValidateNameDesc)
projectValidateNameAddInfo =        info (myHelper <*> projectValidateNameAdd)        (progDesc projectValidateNameAddDesc)
projectValidateNameRemoveInfo =     info (myHelper <*> projectValidateNameRemove)     (progDesc projectValidateNameRemoveDesc)
projectValidateNameListInfo =       info (myHelper <*> projectValidateNameList)       (progDesc projectValidateNameListDesc)
projectValidateCommandInfo =        info (myHelper <*> projectValidateCommand)        (progDesc projectValidateCommandDesc)
projectValidateCommandSetInfo =     info (myHelper <*> projectValidateCommandSet)     (progDesc projectValidateCommandSetDesc)
projectValidateCommandUnsetInfo =   info (myHelper <*> projectValidateCommandUnset)   (progDesc projectValidateCommandUnsetDesc)
projectValidateCommandListInfo =    info (myHelper <*> projectValidateCommandList)    (progDesc projectValidateCommandListDesc)
projectValidateScriptInfo =         info (myHelper <*> projectValidateScript)         (progDesc projectValidateScriptDesc)
projectValidateScriptSetInfo =      info (myHelper <*> projectValidateScriptSet)      (progDesc projectValidateScriptSetDesc)
projectValidateScriptUnsetInfo =    info (myHelper <*> projectValidateScriptUnset)    (progDesc projectValidateScriptUnsetDesc)
projectValidateScriptListInfo =     info (myHelper <*> projectValidateScriptList)     (progDesc projectValidateScriptListDesc)
projectValidateScriptExtractInfo =  info (myHelper <*> projectValidateScriptExtract)  (progDesc projectValidateScriptExtractDesc)

projectValidate = ProjectValidateOpts <$> subparser (
 command acceptExecSub projectValidateAcceptExecInfo <>
 command nameSub       projectValidateNameInfo <> 
 command commandSub    projectValidateCommandInfo <>
 command scriptSub     projectValidateScriptInfo)
 
projectValidateAcceptExec = ProjectValidateAcceptExec <$> ProjectValidateAcceptExecOpts <$> subparser (
 command setSub  projectValidateAcceptExecSetInfo <>
 command listSub projectValidateAcceptExecListInfo)

projectValidateAcceptExecSet = ProjectValidateAcceptExecSet <$> (ProjectValidateAcceptExecSetOpts
 <$> optional (strOption $ toMod repoNodeOpt <> metavar repoNodeMeta <> help repoNodeHelp)
 <*> optional (strOption $ toMod termNodeOpt <> metavar termNodeMeta <> help termNodeHelp)
 <*> optional (strOption $ toMod courseNodeOpt <> metavar courseNodeMeta <> help courseNodeHelp)
 <*> optional (strOption $ toMod groupNodeOpt <> metavar groupNodeMeta <> help groupNodeHelp)
 <*> optional (strOption $ toMod projectNodeOpt <> metavar projectNodeMeta <> help projectNodeHelp)
 <*> argument str (metavar projectValidateAcceptExecSetMeta <> help projectValidateAcceptExecSetHelp) )
  
projectValidateAcceptExecList = ProjectValidateAcceptExecList <$> (ProjectValidateAcceptExecListOpts
 <$> optional (strOption $ toMod repoNodeOpt <> metavar repoNodeMeta <> help repoNodeHelp)
 <*> optional (strOption $ toMod termNodeOpt <> metavar termNodeMeta <> help termNodeHelp)
 <*> optional (strOption $ toMod courseNodeOpt <> metavar courseNodeMeta <> help courseNodeHelp)
 <*> optional (strOption $ toMod groupNodeOpt <> metavar groupNodeMeta <> help groupNodeHelp)
 <*> optional (strOption $ toMod projectNodeOpt <> metavar projectNodeMeta <> help projectNodeHelp))

projectValidateName = ProjectValidateName <$> ProjectValidateNameOpts <$> subparser (
 command addSub    projectValidateNameAddInfo <>
 command removeSub projectValidateNameRemoveInfo <>
 command listSub   projectValidateNameListInfo)

projectValidateNameAdd = ProjectValidateNameAdd <$> (ProjectValidateNameAddOpts
 <$> optional (strOption $ toMod repoNodeOpt <> metavar repoNodeMeta <> help repoNodeHelp)
 <*> optional (strOption $ toMod termNodeOpt <> metavar termNodeMeta <> help termNodeHelp)
 <*> optional (strOption $ toMod courseNodeOpt <> metavar courseNodeMeta <> help courseNodeHelp)
 <*> optional (strOption $ toMod groupNodeOpt <> metavar groupNodeMeta <> help groupNodeHelp)
 <*> optional (strOption $ toMod projectNodeOpt <> metavar projectNodeMeta <> help projectNodeHelp)
 <*> some (argument str (metavar projectValidateNameAddMeta <> help projectValidateNameAddHelp)))

projectValidateNameRemove = ProjectValidateNameRemove <$> (ProjectValidateNameRemoveOpts
 <$> optional (strOption $ toMod repoNodeOpt <> metavar repoNodeMeta <> help repoNodeHelp)
 <*> optional (strOption $ toMod termNodeOpt <> metavar termNodeMeta <> help termNodeHelp)
 <*> optional (strOption $ toMod courseNodeOpt <> metavar courseNodeMeta <> help courseNodeHelp)
 <*> optional (strOption $ toMod groupNodeOpt <> metavar groupNodeMeta <> help groupNodeHelp)
 <*> optional (strOption $ toMod projectNodeOpt <> metavar projectNodeMeta <> help projectNodeHelp)
 <*> some (argument str (metavar projectValidateNameRemoveMeta <> help projectValidateNameRemoveHelp)))

projectValidateNameList = ProjectValidateNameList <$> (ProjectValidateNameListOpts
 <$> optional (strOption $ toMod repoNodeOpt <> metavar repoNodeMeta <> help repoNodeHelp)
 <*> optional (strOption $ toMod termNodeOpt <> metavar termNodeMeta <> help termNodeHelp)
 <*> optional (strOption $ toMod courseNodeOpt <> metavar courseNodeMeta <> help courseNodeHelp)
 <*> optional (strOption $ toMod groupNodeOpt <> metavar groupNodeMeta <> help groupNodeHelp)
 <*> optional (strOption $ toMod projectNodeOpt <> metavar projectNodeMeta <> help projectNodeHelp))

projectValidateCommand = ProjectValidateCommand <$> ProjectValidateCommandOpts <$> subparser (
 command setSub    projectValidateCommandSetInfo <>
 command unsetSub  projectValidateCommandUnsetInfo <>
 command listSub   projectValidateCommandListInfo)

projectValidateCommandSet = ProjectValidateCommandSet <$> (ProjectValidateCommandSetOpts
 <$> optional (strOption $ toMod repoNodeOpt <> metavar repoNodeMeta <> help repoNodeHelp)
 <*> optional (strOption $ toMod termNodeOpt <> metavar termNodeMeta <> help termNodeHelp)
 <*> optional (strOption $ toMod courseNodeOpt <> metavar courseNodeMeta <> help courseNodeHelp)
 <*> optional (strOption $ toMod groupNodeOpt <> metavar groupNodeMeta <> help groupNodeHelp)
 <*> optional (strOption $ toMod projectNodeOpt <> metavar projectNodeMeta <> help projectNodeHelp)
 <*> argument str (metavar projectValidateCommandSetMeta <> help projectValidateCommandSetHelp))

projectValidateCommandUnset = ProjectValidateCommandUnset <$> (ProjectValidateCommandUnsetOpts
 <$> optional (strOption $ toMod repoNodeOpt <> metavar repoNodeMeta <> help repoNodeHelp)
 <*> optional (strOption $ toMod termNodeOpt <> metavar termNodeMeta <> help termNodeHelp)
 <*> optional (strOption $ toMod courseNodeOpt <> metavar courseNodeMeta <> help courseNodeHelp)
 <*> optional (strOption $ toMod groupNodeOpt <> metavar groupNodeMeta <> help groupNodeHelp)
 <*> optional (strOption $ toMod projectNodeOpt <> metavar projectNodeMeta <> help projectNodeHelp))

projectValidateCommandList = ProjectValidateCommandList <$> (ProjectValidateCommandListOpts
 <$> optional (strOption $ toMod repoNodeOpt <> metavar repoNodeMeta <> help repoNodeHelp)
 <*> optional (strOption $ toMod termNodeOpt <> metavar termNodeMeta <> help termNodeHelp)
 <*> optional (strOption $ toMod courseNodeOpt <> metavar courseNodeMeta <> help courseNodeHelp)
 <*> optional (strOption $ toMod groupNodeOpt <> metavar groupNodeMeta <> help groupNodeHelp)
 <*> optional (strOption $ toMod projectNodeOpt <> metavar projectNodeMeta <> help projectNodeHelp))

projectValidateScript = ProjectValidateScript <$> ProjectValidateScriptOpts <$> subparser (
 command setSub     projectValidateScriptSetInfo <>
 command unsetSub   projectValidateScriptUnsetInfo <>
 command listSub    projectValidateScriptListInfo <>
 command extractSub projectValidateScriptExtractInfo)

projectValidateScriptSet = ProjectValidateScriptSet <$> (ProjectValidateScriptSetOpts
 <$> optional (strOption $ toMod repoNodeOpt <> metavar repoNodeMeta <> help repoNodeHelp)
 <*> optional (strOption $ toMod termNodeOpt <> metavar termNodeMeta <> help termNodeHelp)
 <*> optional (strOption $ toMod courseNodeOpt <> metavar courseNodeMeta <> help courseNodeHelp)
 <*> optional (strOption $ toMod groupNodeOpt <> metavar groupNodeMeta <> help groupNodeHelp)
 <*> optional (strOption $ toMod projectNodeOpt <> metavar projectNodeMeta <> help projectNodeHelp)
 <*> argument str (metavar projectValidateScriptSetMeta <> help projectValidateScriptSetHelp))

projectValidateScriptUnset = ProjectValidateScriptUnset <$> (ProjectValidateScriptUnsetOpts
 <$> optional (strOption $ toMod repoNodeOpt <> metavar repoNodeMeta <> help repoNodeHelp)
 <*> optional (strOption $ toMod termNodeOpt <> metavar termNodeMeta <> help termNodeHelp)
 <*> optional (strOption $ toMod courseNodeOpt <> metavar courseNodeMeta <> help courseNodeHelp)
 <*> optional (strOption $ toMod groupNodeOpt <> metavar groupNodeMeta <> help groupNodeHelp)
 <*> optional (strOption $ toMod projectNodeOpt <> metavar projectNodeMeta <> help projectNodeHelp))

projectValidateScriptList = ProjectValidateScriptList <$> (ProjectValidateScriptListOpts
 <$> optional (strOption $ toMod repoNodeOpt <> metavar repoNodeMeta <> help repoNodeHelp)
 <*> optional (strOption $ toMod termNodeOpt <> metavar termNodeMeta <> help termNodeHelp)
 <*> optional (strOption $ toMod courseNodeOpt <> metavar courseNodeMeta <> help courseNodeHelp)
 <*> optional (strOption $ toMod groupNodeOpt <> metavar groupNodeMeta <> help groupNodeHelp)
 <*> optional (strOption $ toMod projectNodeOpt <> metavar projectNodeMeta <> help projectNodeHelp))

projectValidateScriptExtract = ProjectValidateScriptExtract <$> (ProjectValidateScriptExtractOpts
 <$> optional (strOption $ toMod repoNodeOpt <> metavar repoNodeMeta <> help repoNodeHelp)
 <*> optional (strOption $ toMod termNodeOpt <> metavar termNodeMeta <> help termNodeHelp)
 <*> optional (strOption $ toMod courseNodeOpt <> metavar courseNodeMeta <> help courseNodeHelp)
 <*> optional (strOption $ toMod groupNodeOpt <> metavar groupNodeMeta <> help groupNodeHelp)
 <*> optional (strOption $ toMod projectNodeOpt <> metavar projectNodeMeta <> help projectNodeHelp)
 <*> argument str (metavar projectValidateScriptExtractMeta <> help projectValidateScriptExtractHelp))


 












