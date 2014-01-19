module Interface.CommandLineParser.Group where

import Options.Applicative
import Interface.Lexicon
import Interface.CommandLineParser.Utils
import Security.SecurityManager

data GroupOpts = GroupOpts      GroupCmd                                deriving (Show, Eq)
data GroupCmd  = GroupAdd       GroupAddOpts
               | GroupRemove    GroupRemoveOpts
               | GroupList      GroupListOpts
               | GroupTeacher   GroupTeacherOpts
               | GroupCorrector GroupCorrectorOpts                      deriving (Show, Eq)

data GroupAddOpts = GroupAddOpts
 { groupAddRepoNN   :: Maybe String
 , groupAddTermNN   :: Maybe String
 , groupAddCourseNN :: Maybe String
 , groupAddName     :: String }                                         deriving (Show, Eq)

data GroupRemoveOpts = GroupRemoveOpts
 { groupRemoveRepoNN   :: Maybe String
 , groupRemoveTermNN   :: Maybe String
 , groupRemoveCourseNN :: Maybe String
 , groupRemoveGroupNN  :: Maybe String }                                deriving (Show, Eq)

data GroupListOpts = GroupListOpts
 { groupListRepoNN   :: Maybe String
 , groupListTermNN   :: Maybe String
 , groupListCourseNN :: Maybe String }                                  deriving (Show, Eq)

data GroupTeacherOpts = GroupTeacherOpts   GroupTeacherCmd              deriving (Show, Eq)
data GroupTeacherCmd  = GroupTeacherAdd    GroupTeacherAddOpts
                      | GroupTeacherRemove GroupTeacherRemoveOpts
                      | GroupTeacherList   GroupTeacherListOpts         deriving (Show, Eq)

data GroupTeacherAddOpts = GroupTeacherAddOpts
 { groupTeacherAddRepoNN   :: Maybe String
 , groupTeacherAddTermNN   :: Maybe String
 , groupTeacherAddCourseNN :: Maybe String
 , groupTeacherAddGroupNN  :: Maybe String
 , groupTeacherAddNames    :: [String] }                                deriving (Show, Eq)

data GroupTeacherRemoveOpts = GroupTeacherRemoveOpts
 { groupTeacherRemoveRepoNN   :: Maybe String
 , groupTeacherRemoveTermNN   :: Maybe String
 , groupTeacherRemoveCourseNN :: Maybe String
 , groupTeacherRemoveGroupNN  :: Maybe String
 , groupTeacherRemoveNames    :: [String] }                             deriving (Show, Eq)

data GroupTeacherListOpts = GroupTeacherListOpts
 { groupTeacherListRepoNN   :: Maybe String
 , groupTeacherListTermNN   :: Maybe String
 , groupTeacherListCourseNN :: Maybe String
 , groupTeacherListGroupNN  :: Maybe String }                           deriving (Show, Eq)

data GroupCorrectorOpts = GroupCorrectorOpts   GroupCorrectorCmd        deriving (Show, Eq)
data GroupCorrectorCmd  = GroupCorrectorAdd    GroupCorrectorAddOpts
                        | GroupCorrectorRemove GroupCorrectorRemoveOpts
                        | GroupCorrectorList   GroupCorrectorListOpts   deriving (Show, Eq)

data GroupCorrectorAddOpts = GroupCorrectorAddOpts
 { groupCorrectorAddRepoNN   :: Maybe String
 , groupCorrectorAddTermNN   :: Maybe String
 , groupCorrectorAddCourseNN :: Maybe String
 , groupCorrectorAddGroupNN  :: Maybe String
 , groupCorrectorAddNames    :: [String] }                              deriving (Show, Eq)

data GroupCorrectorRemoveOpts = GroupCorrectorRemoveOpts
 { groupCorrectorRemoveRepoNN   :: Maybe String
 , groupCorrectorRemoveTermNN   :: Maybe String
 , groupCorrectorRemoveCourseNN :: Maybe String
 , groupCorrectorRemoveGroupNN  :: Maybe String
 , groupCorrectorRemoveNames    :: [String] }                           deriving (Show, Eq)

data GroupCorrectorListOpts = GroupCorrectorListOpts
 { groupCorrectorListRepoNN   :: Maybe String
 , groupCorrectorListTermNN   :: Maybe String
 , groupCorrectorListCourseNN :: Maybe String
 , groupCorrectorListGroupNN  :: Maybe String }                         deriving (Show, Eq)

groupInfo role =           info (myHelper <*> group role)           (progDesc groupDesc)
groupAddInfo =             info (myHelper <*> groupAdd)             (progDesc groupAddDesc)
groupRemoveInfo =          info (myHelper <*> groupRemove)          (progDesc groupRemoveDesc)
groupListInfo =            info (myHelper <*> groupList)            (progDesc groupListDesc)
groupTeacherInfo role =    info (myHelper <*> groupTeacher role)    (progDesc groupTeacherDesc)
groupTeacherAddInfo =      info (myHelper <*> groupTeacherAdd)      (progDesc groupTeacherAddDesc)
groupTeacherRemoveInfo =   info (myHelper <*> groupTeacherRemove)   (progDesc groupTeacherRemoveDesc)
groupTeacherListInfo =     info (myHelper <*> groupTeacherList)     (progDesc groupTeacherListDesc)
groupCorrectorInfo role =  info (myHelper <*> groupCorrector role)  (progDesc groupCorrectorDesc)
groupCorrectorAddInfo =    info (myHelper <*> groupCorrectorAdd)    (progDesc groupCorrectorAddDesc)
groupCorrectorRemoveInfo = info (myHelper <*> groupCorrectorRemove) (progDesc groupCorrectorRemoveDesc)
groupCorrectorListInfo =   info (myHelper <*> groupCorrectorList)   (progDesc groupCorrectorListDesc)

group role = GroupOpts <$> subparser (
 hasGroupWriteRights role (command addSub       groupAddInfo) <>
 hasGroupWriteRights role (command removeSub    groupRemoveInfo) <>
 hasGroupReadRights  role (command listSub      groupListInfo) <>
 command teacherSub   (groupTeacherInfo role) <>
 command correctorSub (groupCorrectorInfo role))

groupAdd = GroupAdd <$> (GroupAddOpts
 <$> optional (strOption $ toMod repoNodeOpt <> metavar repoNodeMeta <> help repoNodeHelp)
 <*> optional (strOption $ toMod termNodeOpt <> metavar termNodeMeta <> help termNodeHelp)
 <*> optional (strOption $ toMod courseNodeOpt <> metavar courseNodeMeta <> help courseNodeHelp)
 <*> argument str (metavar groupAddMeta <> help groupAddHelp))

groupRemove = GroupRemove <$> (GroupRemoveOpts
 <$> optional (strOption $ toMod repoNodeOpt <> metavar repoNodeMeta <> help repoNodeHelp)
 <*> optional (strOption $ toMod termNodeOpt <> metavar termNodeMeta <> help termNodeHelp)
 <*> optional (strOption $ toMod courseNodeOpt <> metavar courseNodeMeta <> help courseNodeHelp)
 <*> optional (strOption $ toMod groupNodeOpt <> metavar groupNodeMeta <> help groupNodeHelp))

groupList = GroupList <$> (GroupListOpts
 <$> optional (strOption $ toMod repoNodeOpt <> metavar repoNodeMeta <> help repoNodeHelp)
 <*> optional (strOption $ toMod termNodeOpt <> metavar termNodeMeta <> help termNodeHelp)
 <*> optional (strOption $ toMod courseNodeOpt <> metavar courseNodeMeta <> help courseNodeHelp))

groupTeacher role = GroupTeacher <$> GroupTeacherOpts <$> subparser (
 hasGroupWriteRights role (command addSub     groupTeacherAddInfo) <>
 hasGroupWriteRights role (command removeSub  groupTeacherRemoveInfo) <>
 hasGroupReadRights  role (command listSub    groupTeacherListInfo))

groupTeacherAdd = GroupTeacherAdd <$> (GroupTeacherAddOpts
 <$> optional (strOption $ toMod repoNodeOpt <> metavar repoNodeMeta <> help repoNodeHelp)
 <*> optional (strOption $ toMod termNodeOpt <> metavar termNodeMeta <> help termNodeHelp)
 <*> optional (strOption $ toMod courseNodeOpt <> metavar courseNodeMeta <> help courseNodeHelp)
 <*> optional (strOption $ toMod groupNodeOpt <> metavar groupNodeMeta <> help groupNodeHelp)
 <*> some (argument str (metavar groupTeacherAddMeta <> help groupTeacherAddHelp)))

groupTeacherRemove = GroupTeacherRemove <$> (GroupTeacherRemoveOpts
 <$> optional (strOption $ toMod repoNodeOpt <> metavar repoNodeMeta <> help repoNodeHelp)
 <*> optional (strOption $ toMod termNodeOpt <> metavar termNodeMeta <> help termNodeHelp)
 <*> optional (strOption $ toMod courseNodeOpt <> metavar courseNodeMeta <> help courseNodeHelp)
 <*> optional (strOption $ toMod groupNodeOpt <> metavar groupNodeMeta <> help groupNodeHelp)
 <*> some (argument str (metavar groupTeacherRemoveMeta <> help groupTeacherRemoveHelp)))

groupTeacherList = GroupTeacherList <$> (GroupTeacherListOpts
 <$> optional (strOption $ toMod repoNodeOpt <> metavar repoNodeMeta <> help repoNodeHelp)
 <*> optional (strOption $ toMod termNodeOpt <> metavar termNodeMeta <> help termNodeHelp)
 <*> optional (strOption $ toMod courseNodeOpt <> metavar courseNodeMeta <> help courseNodeHelp)
 <*> optional (strOption $ toMod groupNodeOpt <> metavar groupNodeMeta <> help groupNodeHelp))

groupCorrector role = GroupCorrector <$> GroupCorrectorOpts <$> subparser (
 hasGroupWriteRights role (command addSub     groupCorrectorAddInfo) <>
 hasGroupWriteRights role (command removeSub  groupCorrectorRemoveInfo) <>
 hasGroupReadRights  role (command listSub    groupCorrectorListInfo))

groupCorrectorAdd = GroupCorrectorAdd <$> (GroupCorrectorAddOpts
 <$> optional (strOption $ toMod repoNodeOpt <> metavar repoNodeMeta <> help repoNodeHelp)
 <*> optional (strOption $ toMod termNodeOpt <> metavar termNodeMeta <> help termNodeHelp)
 <*> optional (strOption $ toMod courseNodeOpt <> metavar courseNodeMeta <> help courseNodeHelp)
 <*> optional (strOption $ toMod groupNodeOpt <> metavar groupNodeMeta <> help groupNodeHelp)
 <*> some (argument str (metavar groupCorrectorAddMeta <> help groupCorrectorAddHelp)))

groupCorrectorRemove = GroupCorrectorRemove <$> (GroupCorrectorRemoveOpts
 <$> optional (strOption $ toMod repoNodeOpt <> metavar repoNodeMeta <> help repoNodeHelp)
 <*> optional (strOption $ toMod termNodeOpt <> metavar termNodeMeta <> help termNodeHelp)
 <*> optional (strOption $ toMod courseNodeOpt <> metavar courseNodeMeta <> help courseNodeHelp)
 <*> optional (strOption $ toMod groupNodeOpt <> metavar groupNodeMeta <> help groupNodeHelp)
 <*> some (argument str (metavar groupCorrectorRemoveMeta <> help groupCorrectorRemoveHelp)))

groupCorrectorList = GroupCorrectorList <$> (GroupCorrectorListOpts
 <$> optional (strOption $ toMod repoNodeOpt <> metavar repoNodeMeta <> help repoNodeHelp)
 <*> optional (strOption $ toMod termNodeOpt <> metavar termNodeMeta <> help termNodeHelp)
 <*> optional (strOption $ toMod courseNodeOpt <> metavar courseNodeMeta <> help courseNodeHelp)
 <*> optional (strOption $ toMod groupNodeOpt <> metavar groupNodeMeta <> help groupNodeHelp))



 