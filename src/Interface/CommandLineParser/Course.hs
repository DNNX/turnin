module Interface.CommandLineParser.Course where

import Options.Applicative
import Interface.Lexicon
import Interface.CommandLineParser.Utils

data CourseOpts = CourseOpts      CourseCmd                                deriving (Show, Eq)
data CourseCmd  = CourseAdd       CourseAddOpts
                | CourseRemove    CourseRemoveOpts
                | CourseList      CourseListOpts
                | CourseTeacher   CourseTeacherOpts
                | CourseCorrector CourseCorrectorOpts                      deriving (Show, Eq)

data CourseAddOpts = CourseAddOpts
 { courseAddRepoNN :: Maybe String
 , courseAddTermNN :: Maybe String
 , courseAddName   :: String }                                             deriving (Show, Eq)

data CourseRemoveOpts = CourseRemoveOpts
 { courseRemoveRepoNN   :: Maybe String
 , courseRemoveTermNN   :: Maybe String
 , courseRemoveCourseNN :: Maybe String }                                  deriving (Show, Eq)

data CourseListOpts = CourseListOpts
 { courseListRepoNN :: Maybe String
 , courseListTermNN :: Maybe String }                                      deriving (Show, Eq)

data CourseTeacherOpts = CourseTeacherOpts   CourseTeacherCmd              deriving (Show, Eq)
data CourseTeacherCmd  = CourseTeacherAdd    CourseTeacherAddOpts
                       | CourseTeacherRemove CourseTeacherRemoveOpts
                       | CourseTeacherList   CourseTeacherListOpts         deriving (Show, Eq)

data CourseTeacherAddOpts = CourseTeacherAddOpts
 { courseTeacherAddRepoNN   :: Maybe String
 , courseTeacherAddTermNN   :: Maybe String
 , courseTeacherAddCourseNN :: Maybe String
 , courseTeacherAddNames    :: [String] }                                  deriving (Show, Eq)

data CourseTeacherRemoveOpts = CourseTeacherRemoveOpts
 { courseTeacherRemoveRepoNN   :: Maybe String
 , courseTeacherRemoveTermNN   :: Maybe String
 , courseTeacherRemoveCourseNN :: Maybe String
 , courseTeacherRemoveNames    :: [String] }                               deriving (Show, Eq)

data CourseTeacherListOpts = CourseTeacherListOpts
 { courseTeacherListRepoNN   :: Maybe String
 , courseTeacherListTermNN   :: Maybe String
 , courseTeacherListCourseNN :: Maybe String }                             deriving (Show, Eq)

data CourseCorrectorOpts = CourseCorrectorOpts   CourseCorrectorCmd        deriving (Show, Eq)
data CourseCorrectorCmd  = CourseCorrectorAdd    CourseCorrectorAddOpts
                         | CourseCorrectorRemove CourseCorrectorRemoveOpts
                         | CourseCorrectorList   CourseCorrectorListOpts   deriving (Show, Eq)

data CourseCorrectorAddOpts = CourseCorrectorAddOpts
 { courseCorrectorAddRepoNN   :: Maybe String
 , courseCorrectorAddTermNN   :: Maybe String
 , courseCorrectorAddCourseNN :: Maybe String
 , courseCorrectorAddNames    :: [String] }                                deriving (Show, Eq)

data CourseCorrectorRemoveOpts = CourseCorrectorRemoveOpts
 { courseCorrectorRemoveRepoNN   :: Maybe String
 , courseCorrectorRemoveTermNN   :: Maybe String
 , courseCorrectorRemoveCourseNN :: Maybe String
 , courseCorrectorRemoveNames    :: [String] }                             deriving (Show, Eq)

data CourseCorrectorListOpts = CourseCorrectorListOpts
 { courseCorrectorListRepoNN   :: Maybe String
 , courseCorrectorListTermNN   :: Maybe String
 , courseCorrectorListCourseNN :: Maybe String }                           deriving (Show, Eq)

courseInfo =                info (myHelper <*> course)                (progDesc courseDesc)
courseAddInfo =             info (myHelper <*> courseAdd)             (progDesc courseAddDesc)
courseRemoveInfo =          info (myHelper <*> courseRemove)          (progDesc courseRemoveDesc)
courseListInfo =            info (myHelper <*> courseList)            (progDesc courseListDesc)
courseTeacherInfo =         info (myHelper <*> courseTeacher)         (progDesc courseTeacherDesc)
courseTeacherAddInfo =      info (myHelper <*> courseTeacherAdd)      (progDesc courseTeacherAddDesc)
courseTeacherRemoveInfo =   info (myHelper <*> courseTeacherRemove)   (progDesc courseTeacherRemoveDesc)
courseTeacherListInfo =     info (myHelper <*> courseTeacherList)     (progDesc courseTeacherListDesc)
courseCorrectorInfo =       info (myHelper <*> courseCorrector)       (progDesc courseCorrectorDesc)
courseCorrectorAddInfo =    info (myHelper <*> courseCorrectorAdd)    (progDesc courseCorrectorAddDesc)
courseCorrectorRemoveInfo = info (myHelper <*> courseCorrectorRemove) (progDesc courseCorrectorRemoveDesc)
courseCorrectorListInfo =   info (myHelper <*> courseCorrectorList)   (progDesc courseCorrectorListDesc)

course = CourseOpts <$> subparser (
 command addSub       courseAddInfo <>
 command removeSub    courseRemoveInfo <>
 command listSub      courseListInfo <>
 command teacherSub   courseTeacherInfo <>
 command correctorSub courseCorrectorInfo)

courseAdd = CourseAdd <$> (CourseAddOpts
 <$> optional (strOption $ toMod repoNodeOpt <> metavar repoNodeMeta <> help repoNodeHelp)
 <*> optional (strOption $ toMod termNodeOpt <> metavar termNodeMeta <> help termNodeHelp)
 <*> argument str (metavar courseAddMeta <> help courseAddHelp))

courseRemove = CourseRemove <$> (CourseRemoveOpts
 <$> optional (strOption $ toMod repoNodeOpt <> metavar repoNodeMeta <> help repoNodeHelp)
 <*> optional (strOption $ toMod termNodeOpt <> metavar termNodeMeta <> help termNodeHelp)
 <*> optional (strOption $ toMod courseNodeOpt <> metavar courseNodeMeta <> help courseNodeHelp))

courseList = CourseList <$> (CourseListOpts
 <$> optional (strOption $ toMod repoNodeOpt <> metavar repoNodeMeta <> help repoNodeHelp)
 <*> optional (strOption $ toMod termNodeOpt <> metavar termNodeMeta <> help termNodeHelp))

courseTeacher = CourseTeacher <$> CourseTeacherOpts <$> subparser (
 command addSub     courseTeacherAddInfo <>
 command removeSub  courseTeacherRemoveInfo <>
 command listSub    courseTeacherListInfo)

courseTeacherAdd = CourseTeacherAdd <$> (CourseTeacherAddOpts
 <$> optional (strOption $ toMod repoNodeOpt <> metavar repoNodeMeta <> help repoNodeHelp)
 <*> optional (strOption $ toMod termNodeOpt <> metavar termNodeMeta <> help termNodeHelp)
 <*> optional (strOption $ toMod courseNodeOpt <> metavar courseNodeMeta <> help courseNodeHelp)
 <*> some (argument str (metavar courseTeacherAddMeta <> help courseTeacherAddHelp)))

courseTeacherRemove = CourseTeacherRemove <$> (CourseTeacherRemoveOpts
 <$> optional (strOption $ toMod repoNodeOpt <> metavar repoNodeMeta <> help repoNodeHelp)
 <*> optional (strOption $ toMod termNodeOpt <> metavar termNodeMeta <> help termNodeHelp)
 <*> optional (strOption $ toMod courseNodeOpt <> metavar courseNodeMeta <> help courseNodeHelp)
 <*> some (argument str (metavar courseTeacherRemoveMeta <> help courseTeacherRemoveHelp)))

courseTeacherList = CourseTeacherList <$> (CourseTeacherListOpts
 <$> optional (strOption $ toMod repoNodeOpt <> metavar repoNodeMeta <> help repoNodeHelp)
 <*> optional (strOption $ toMod termNodeOpt <> metavar termNodeMeta <> help termNodeHelp)
 <*> optional (strOption $ toMod courseNodeOpt <> metavar courseNodeMeta <> help courseNodeHelp))

courseCorrector = CourseCorrector <$> CourseCorrectorOpts <$> subparser (
 command addSub     courseCorrectorAddInfo <>
 command removeSub  courseCorrectorRemoveInfo <>
 command listSub    courseCorrectorListInfo)

courseCorrectorAdd = CourseCorrectorAdd <$> (CourseCorrectorAddOpts
 <$> optional (strOption $ toMod repoNodeOpt <> metavar repoNodeMeta <> help repoNodeHelp)
 <*> optional (strOption $ toMod termNodeOpt <> metavar termNodeMeta <> help termNodeHelp)
 <*> optional (strOption $ toMod courseNodeOpt <> metavar courseNodeMeta <> help courseNodeHelp)
 <*> some (argument str (metavar courseCorrectorAddMeta <> help courseCorrectorAddHelp)))

courseCorrectorRemove = CourseCorrectorRemove <$> (CourseCorrectorRemoveOpts
 <$> optional (strOption $ toMod repoNodeOpt <> metavar repoNodeMeta <> help repoNodeHelp)
 <*> optional (strOption $ toMod termNodeOpt <> metavar termNodeMeta <> help termNodeHelp)
 <*> optional (strOption $ toMod courseNodeOpt <> metavar courseNodeMeta <> help courseNodeHelp)
 <*> some (argument str (metavar courseCorrectorRemoveMeta <> help courseCorrectorRemoveHelp)))

courseCorrectorList = CourseCorrectorList <$> (CourseCorrectorListOpts
 <$> optional (strOption $ toMod repoNodeOpt <> metavar repoNodeMeta <> help repoNodeHelp)
 <*> optional (strOption $ toMod termNodeOpt <> metavar termNodeMeta <> help termNodeHelp)
 <*> optional (strOption $ toMod courseNodeOpt <> metavar courseNodeMeta <> help courseNodeHelp))



 