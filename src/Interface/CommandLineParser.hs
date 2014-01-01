module Interface.CommandLineParser where

--import Options.Applicative 

data GlobalOpts = Global GlobalCmd
data GlobalCmd  = Config    ConfigOpts
                | Repo      RepoOpts
                | Term      TermOpts
                | Course    CourseOpts
--                | Group     GroupOpts
--                | Project   ProjectOpts
--                | Submit    SubmitOpts
--                | Worktrain WorktrainOpts

data ConfigOpts = ConfigOpts ConfigCmd
data ConfigCmd  = ConfigThreshold     ConfigThresholdOpts
                | ConfigTermDate      ConfigTermDateOpts
                | ConfigProjectDate   ConfigProjectDateOpts
                | ConfigAcceptExec    ConfigAcceptExecOpts
                | ConfigTimeLimit     ConfigTimeLimitOpts
                | ConfigSpaceLimit    ConfigSpaceLimitOpts
                | ConfigAdminGroups   ConfigAdminGroupsOpts
                | ConfigTeacherGroups ConfigTeacherGroupsOpts
                | ConfigCorrector     ConfigCorrectorOpts

-- Config threshold
data ConfigThresholdOpts = ConfigThresholdOpts ConfigThresholdCmd
data ConfigThresholdCmd  = ConfigThresholdSet  ConfigThresholdSetOpts
                         | ConfigThresholdList ConfigThresholdListOpts

data ConfigThresholdSetOpts = ConfigThresholdSetOpts
 { configThresholdSetCurrent :: Maybe String
 , configThresholdSetChoose  :: Maybe String }
data ConfigThresholdListOpts = ConfigThresholdListOpts

-- Config term date
data ConfigTermDateOpts = ConfigTermDateOpts  ConfigTermDateCmd
data ConfigTermDateCmd  = ConfigTermDateSet   ConfigTermDateSetOpts
                        | ConfigTermDateList  ConfigTermDateListOpts

data ConfigTermDateSetOpts = ConfigTermDateSetOpts
 { configTermDateSetTerm1 :: Maybe String
 , configTermDateSetTerm2 :: Maybe String
 , configTermDateSetTerm3 :: Maybe String }
data ConfigTermDateListOpts = ConfigTermDateListOpts

-- Config project date
data ConfigProjectDateOpts = ConfigProjectDateOpts ConfigProjectDateCmd
data ConfigProjectDateCmd  = ConfigProjectDateSet  ConfigProjectDateSetOpts
                           | ConfigProjectDateList ConfigProjectDateListOpts

data ConfigProjectDateSetOpts = ConfigProjectDateSetOpts
 { configProjectDateSetEnd  :: Maybe String
 , configProjectDateSetLate :: Maybe String }
data ConfigProjectDateListOpts = ConfigProjectDateListOpts

-- Config accept exec
data ConfigAcceptExecOpts = ConfigAcceptExecOpts ConfigAcceptExecCmd
data ConfigAcceptExecCmd  = ConfigAcceptExecSet  ConfigAcceptExecSetOpts
                          | ConfigAcceptExecList ConfigAcceptExecListOpts

data ConfigAcceptExecSetOpts = ConfigAcceptExecSetOpts
 { configAcceptExecSetFlag :: Bool }
data ConfigAcceptExecListOpts = ConfigAcceptExecListOpts

-- Config time limit
data ConfigTimeLimitOpts = ConfigTimeLimitOpts ConfigTimeLimitCmd
data ConfigTimeLimitCmd  = ConfigTimeLimitSet  ConfigTimeLimitSetOpts
                         | ConfigTimeLimitList ConfigTimeLimitListOpts

data ConfigTimeLimitSetOpts = ConfigTimeLimitSetOpts
 { configTimeLimitSetSeconds :: Double }
data ConfigTimeLimitListOpts = ConfigTimeLimitListOpts

-- Config space limit
data ConfigSpaceLimitOpts = ConfigSpaceLimitOpts ConfigSpaceLimitCmd
data ConfigSpaceLimitCmd  = ConfigSpaceLimitSet  ConfigSpaceLimitSetOpts
                          | ConfigSpaceLimitList ConfigSpaceLimitListOpts

data ConfigSpaceLimitSetOpts = ConfigSpaceLimitSetOpts
 { configSpaceLimitSetBytes :: Integer }
data ConfigSpaceLimitListOpts = ConfigSpaceLimitListOpts

-- Config admin groups
data ConfigAdminGroupsOpts = ConfigAdminGroupsOpts ConfigAdminGroupsCmd
data ConfigAdminGroupsCmd  = CongigAdminGroupsSet  ConfigAdminGroupsSetOpts
                           | CongigAdminGroupsList ConfigAdminGroupsListOpts

data ConfigAdminGroupsSetOpts = ConfigAdminGroupsSetOpts
 { configAdminGroupsSetNames :: [String] }
data ConfigAdminGroupsListOpts = ConfigAdminGroupsListOpts

-- Config teacher groups
data ConfigTeacherGroupsOpts = ConfigTeacherGroupsOpts ConfigTeacherGroupsCmd
data ConfigTeacherGroupsCmd  = CongigTeacherGroupsSet  ConfigTeacherGroupsSetOpts
                             | CongigTeacherGroupsList ConfigTeacherGroupsListOpts

data ConfigTeacherGroupsSetOpts = ConfigTeachereGroupsSetOpts
 { configTeacherGroupsSetNames :: [String] }
data ConfigTeacherGroupsListOpts = ConfigTeacherGroupsListOpts

-- Config corrector
data ConfigCorrectorOpts = ConfigCorrectorOpts ConfigCorrectorCmd
data ConfigCorrectorCmd  = ConfigCorrectorIs ConfigCorrectorIsOpts
                         | ConfigCorrectorAdd ConfigCorrectorAddOpts
                         | ConfigCorrectorRemove ConfigCorrectorRemoveOpts   
                         
data ConfigCorrectorIsOpts = ConfigCorrectorIsOpts
 { configCorrectorIsName :: String }
data ConfigCorrectorAddOpts = ConfigCorrectorAddOpts
 { configCorrectorAddName :: String }
data ConfigCorrectorRemoveOpts = ConfigCorrectorRemoveOpts
 { configCorrectorRemoveName :: String }

-- Repo
data RepoOpts = RepoOpts RepoCmd
data RepoCmd  = RepoAdd    RepoAddOpts
              | RepoRemove RepoRemoveOpts
              | RepoList   RepoListOpts
              
data RepoAddOpts = RepoAddOpts
 { repoAddRepoName :: String }
data RepoRemoveOpts = RepoRemoveOpts
 { repoRemoveRepoName :: Maybe String }
data RepoListOpts = RepoListOpts

-- Term
data TermOpts = TermOpts TermCmd
data TermCmd  = TermAdd    TermAddOpts
              | TermRemove TermRemoveOpts   
              | TermList   TermListOpts
              | TermDate   TermDateOpts
              
data TermAddOpts = TermAddOpts
 { termAddRepoName :: Maybe String 
 , termAddTermName :: String }
 
data TermRemoveOpts = TermRemoveOpts
 { termRemoveRepoName :: Maybe String
 , termRemoveTermName :: Maybe String }
 
data TermListOpts = TermListOpts
 { termListRepoName :: Maybe String }
 
data TermDateOpts = TermDateOpts TermDateCmd
data TermDateCmd  = TermDateSet  TermDateSetOpts 
                  | TermDateList TermDateListOpts
 
data TermDateSetOpts = TermDateSetOpts
 { termDateSetRepoName :: Maybe String
 , termDateSetTermName :: Maybe String
 , termDateSetStart    :: Maybe String
 , termDateSetEnd      :: Maybe String }
 
data TermDateListOpts = TermDateListOpts
 { termDateListRepoName :: Maybe String
 , termDateListTermName :: Maybe String }

-- Course
data CourseOpts = CourseOpts CourseCmd
data CourseCmd  = CourseAdd CourseAddOpts
                | CourseRemove CourseRemoveOpts
--                | CourseList CourseListOpts
--                | CourseTeacher CourseTeacherOpts
--                | CourseCorrector CourseCorrectorOpts

data CourseAddOpts = CourseAddOpts
 { courseAddRepoName   :: Maybe String
 , courseAddTermName   :: Maybe String
 , courseAddCourseName :: String }

data CourseRemoveOpts = CourseRemoveOpts
 { courseRemoveRepoName   :: Maybe String
 , courseRemoveTermName   :: Maybe String
 , courseRemoveCourseName :: Maybe String }
 
 
myReverse :: [a] -> [a]
myReverse [] = [] 
myReverse [x] = [x]
myReverse (_:xs) = myReverse xs





