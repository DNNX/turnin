module Interface.CommandLineParser.Project.Submit where

import Options.Applicative
import Interface.Lexicon
import Interface.CommandLineParser.Utils
import Security.SecurityManager

data ProjectSubmitOpts = ProjectSubmitOpts       ProjectSubmitCmd         deriving (Show, Eq)
data ProjectSubmitCmd  = ProjectSubmitList       ProjectSubmitListOpts
                       | ProjectSubmitLate       ProjectSubmitLateOpts
                       | ProjectSubmitInspect    ProjectSubmitInspectOpts
                       | ProjectSubmitExtract    ProjectSubmitExtractOpts deriving (Show, Eq)

data ProjectSubmitListOpts = ProjectSubmitListOpts
 { projectSubmitListRepoNN    :: Maybe String
 , projectSubmitListTermNN    :: Maybe String
 , projectSubmitListCourseNN  :: Maybe String
 , projectSubmitListGroupNN   :: Maybe String
 , projectSubmitListProjectNN :: Maybe String } deriving (Show, Eq)       
                                                          
data ProjectSubmitLateOpts = ProjectSubmitLateOpts
 { projectSubmitLateRepoNN    :: Maybe String
 , projectSubmitLateTermNN    :: Maybe String
 , projectSubmitLateCourseNN  :: Maybe String
 , projectSubmitLateGroupNN   :: Maybe String
 , projectSubmitLateProjectNN :: Maybe String } deriving (Show, Eq)                                    
 
data ProjectSubmitInspectOpts = ProjectSubmitInspectOpts
 { projectSubmitInspectRepoNN    :: Maybe String
 , projectSubmitInspectTermNN    :: Maybe String
 , projectSubmitInspectCourseNN  :: Maybe String
 , projectSubmitInspectGroupNN   :: Maybe String
 , projectSubmitInspectProjectNN :: Maybe String
 , projectSubmitInspectKeys      :: [String]     } deriving (Show, Eq)                                    
 
data ProjectSubmitExtractOpts = ProjectSubmitExtractOpts
 { projectSubmitExtractRepoNN    :: Maybe String
 , projectSubmitExtractTermNN    :: Maybe String
 , projectSubmitExtractCourseNN  :: Maybe String
 , projectSubmitExtractGroupNN   :: Maybe String
 , projectSubmitExtractProjectNN :: Maybe String
 , projectSubmitExtractDir       :: String       
 , projectSubmitExtractNames     :: [String]     } deriving (Show, Eq)       
                    
projectSubmitInfo role =   info (myHelper <*> projectSubmit role)   (progDesc projectSubmitDesc)
projectSubmitListInfo =    info (myHelper <*> projectSubmitList)    (progDesc projectSubmitListDesc)
projectSubmitLateInfo =    info (myHelper <*> projectSubmitLate)    (progDesc projectSubmitLateDesc)
projectSubmitInspectInfo = info (myHelper <*> projectSubmitInspect) (progDesc projectSubmitInspectDesc)
projectSubmitExtractInfo = info (myHelper <*> projectSubmitExtract) (progDesc projectSubmitExtractDesc)
 
projectSubmit role = ProjectSubmitOpts <$> subparser (
 hasProjectReadRights role (command listSub    projectSubmitListInfo) <>
 hasProjectReadRights role (command lateSub    projectSubmitLateInfo) <>
 hasProjectReadRights role (command inspectSub projectSubmitInspectInfo) <>  
 hasProjectReadRights role (command extractSub projectSubmitExtractInfo))

projectSubmitList = ProjectSubmitList <$> (ProjectSubmitListOpts
 <$> optional (strOption $ toMod repoNodeOpt <> metavar repoNodeMeta <> help repoNodeHelp)
 <*> optional (strOption $ toMod termNodeOpt <> metavar termNodeMeta <> help termNodeHelp)
 <*> optional (strOption $ toMod courseNodeOpt <> metavar courseNodeMeta <> help courseNodeHelp)
 <*> optional (strOption $ toMod groupNodeOpt <> metavar groupNodeMeta <> help groupNodeHelp)
 <*> optional (strOption $ toMod projectNodeOpt <> metavar projectNodeMeta <> help projectNodeHelp))
   
projectSubmitLate = ProjectSubmitLate <$> (ProjectSubmitLateOpts
 <$> optional (strOption $ toMod repoNodeOpt <> metavar repoNodeMeta <> help repoNodeHelp)
 <*> optional (strOption $ toMod termNodeOpt <> metavar termNodeMeta <> help termNodeHelp)
 <*> optional (strOption $ toMod courseNodeOpt <> metavar courseNodeMeta <> help courseNodeHelp)
 <*> optional (strOption $ toMod groupNodeOpt <> metavar groupNodeMeta <> help groupNodeHelp)
 <*> optional (strOption $ toMod projectNodeOpt <> metavar projectNodeMeta <> help projectNodeHelp))

projectSubmitInspect = ProjectSubmitInspect <$> (ProjectSubmitInspectOpts
 <$> optional (strOption $ toMod repoNodeOpt <> metavar repoNodeMeta <> help repoNodeHelp)
 <*> optional (strOption $ toMod termNodeOpt <> metavar termNodeMeta <> help termNodeHelp)
 <*> optional (strOption $ toMod courseNodeOpt <> metavar courseNodeMeta <> help courseNodeHelp)
 <*> optional (strOption $ toMod groupNodeOpt <> metavar groupNodeMeta <> help groupNodeHelp)
 <*> optional (strOption $ toMod projectNodeOpt <> metavar projectNodeMeta <> help projectNodeHelp)
 <*> some (argument str (metavar projectSubmitInspectMeta <> help projectSubmitInspectHelp)))
 
projectSubmitExtract = ProjectSubmitExtract <$> (ProjectSubmitExtractOpts
 <$> optional (strOption $ toMod repoNodeOpt <> metavar repoNodeMeta <> help repoNodeHelp)
 <*> optional (strOption $ toMod termNodeOpt <> metavar termNodeMeta <> help termNodeHelp)
 <*> optional (strOption $ toMod courseNodeOpt <> metavar courseNodeMeta <> help courseNodeHelp)
 <*> optional (strOption $ toMod groupNodeOpt <> metavar groupNodeMeta <> help groupNodeHelp)
 <*> optional (strOption $ toMod projectNodeOpt <> metavar projectNodeMeta <> help projectNodeHelp)
 <*> argument str (metavar projectSubmitExtractDirMeta <> help projectSubmitExtractDirHelp)
 <*> some (argument str (metavar projectSubmitExtractKeysMeta <> help projectSubmitExtractKeysHelp)))
 
 
  
 
  
 
  
 