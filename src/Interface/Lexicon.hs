module Interface.Lexicon where

-- Commands
configSub = "config"
repoSub = "repo"
termSub = "term"
courseSub = "course"
groupSub = "group"

thresholdSub = "threshold"
termDateSub = "termDate"
projectDateSub = "projectDate"
acceptExecSub = "acceptExec"
timeLimitSub = "timeLimit"
spaceLimitSub = "spaceLimit"
adminGroupsSub = "adminGroups"
teacherGroupsSub = "teacherGroups"

teacherSub = "teacher"
correctorSub = "corrector"

addSub = "add"
removeSub = "remove"
listSub = "list"

dateSub = "date"
setSub = "set"
isSub = "is"

-- Options
helpOpt = ('?', "help")

repoNodeOpt = ('r', "repo")
termNodeOpt = ('t', "term")
courseNodeOpt = ('c', "course")
groupNodeOpt = ('g', "group")

configThresholdSetCurrentOpt = ('u', "current")
configThresholdSetChooseOpt = ( 'h', "choose")
configTermDateSetTerm1Opt = ('1', "term1")
configTermDateSetTerm2Opt = ('2', "term2")
configTermDateSetTerm3Opt = ('3', "term3")
configProjectDateSetEndOpt = ('e', "end")
configProjectDateSetLateOpt = ('l', "late")

termDateSetStartOpt = ('s', "start")
termDateSetEndOpt = ('e', "end")

-- Descriptions
globalDesc = "Global desc"
globalHeader = "Global header"

configDesc = "Config desc"
repoDesc = "Repo desc"
termDesc = "Term desc"
courseDesc = "Course desc"
groupDesc = "Group desc"

configThresholdDesc = "Config threshold desc"
configThresholdSetDesc = "Config threshold set desc"
configThresholdListDesc = "Config threshold list desc"
configTermDateDesc = "Config term date desc"
configTermDateSetDesc = "Config term date set desc"
configTermDateListDesc = "Config term date list desc"
configProjectDateDesc = "Config project date desc"
configProjectDateSetDesc = "Config project date set desc"
configProjectDateListDesc = "Config project date list desc"
configAcceptExecDesc = "Config accept exec desc"
configAcceptExecSetDesc = "Config accept exec set desc"
configAcceptExecListDesc = "Config accept exec list desc"
configTimeLimitDesc = "Config time limit desc"
configTimeLimitSetDesc = "Config time limit set desc"
configTimeLimitListDesc = "Config time limit list desc"
configSpaceLimitDesc = "Config space limit desc"
configSpaceLimitSetDesc = "Config space limit set desc"
configSpaceLimitListDesc = "Config space limit list desc"
configAdminGroupsDesc = "Config admin groups desc"
configAdminGroupsSetDesc = "Config admin groups set desc"
configAdminGroupsListDesc = "Config admin groups list desc"
configTeacherGroupsDesc = "Config teacher groups desc"
configTeacherGroupsSetDesc = "Config teacher groups set desc"
configTeacherGroupsListDesc = "Config teacher groups list desc"
configCorrectorDesc = "Config corrector desc"
configCorrectorIsDesc = "Config corrector is desc"
configCorrectorAddDesc = "Config corrector add desc"
configCorrectorRemoveDesc = "Config corrector remove desc"

repoAddDesc = "Repo add desc"
repoRemoveDesc = "Repo remove desc"
repoListDesc = "Repo list desc"

termAddDesc = "Term add desc"
termRemoveDesc = "Term remove desc"
termListDesc = "Term list desc"
termDateDesc = "Term date desc"
termDateSetDesc = "Term date set desc"
termDateListDesc = "Term date list desc"

courseAddDesc = "Course add desc"
courseRemoveDesc = "Course remove desc"
courseListDesc = "Course list desc"
courseTeacherDesc = "Course teacher desc"
courseTeacherAddDesc = "Course teacher add desc"
courseTeacherRemoveDesc = "Course teacher remove desc"
courseTeacherListDesc = "Course teacher list desc"
courseCorrectorDesc = "Course corrector desc"
courseCorrectorAddDesc = "Course corrector add desc"
courseCorrectorRemoveDesc = "Course corrector remove desc"
courseCorrectorListDesc = "Course corrector list desc"

groupAddDesc = "Group add desc"
groupRemoveDesc = "Group remove desc"
groupListDesc = "Group list desc"
groupTeacherDesc = "Group teacher desc"
groupTeacherAddDesc = "Group teacher add desc"
groupTeacherRemoveDesc = "Group teacher remove desc"
groupTeacherListDesc = "Group teacher list desc"
groupCorrectorDesc = "Group corrector desc"
groupCorrectorAddDesc = "Group corrector add desc"
groupCorrectorRemoveDesc = "Group corrector remove desc"
groupCorrectorListDesc = "Group corrector list desc"

-- Metavars
repoNodeMeta = "REPO_NAME"
termNodeMeta = "TERM_NAME"
courseNodeMeta = "COURSE_NAME"
groupNodeMeta = "GROUP_NAME"

configThresholdSetCurrentMeta = "CURRENT"
configThresholdSetChooseMeta = "CHOOSE"
configTermDateSetTerm1Meta = "TERM1"
configTermDateSetTerm2Meta = "TERM2"
configTermDateSetTerm3Meta = "TERM3"
configProjectDateSetEndMeta = "END"
configProjectDateSetLateMeta = "LATE"
configAcceptExecSetWhetherMeta = "ACCEPT_EXEC"
configTimeLimitSetSecondsMeta = "TIME_LIMIT"
configSpaceLimitSetBytesMeta = "SPACE_LIMIT"
configAdminGroupsSetGroupsMeta = "ADMIN_GROUPS"
configTeacherGroupsSetGroupsMeta = "TEACHER_GROUPS"
configCorrectorIsNameMeta = "CORR_NAME"
configCorrectorAddNameMeta = "CORR_NAME"
configCorrectorRemoveNameMeta = "CORR_NAME"

repoAddNameMeta = "REPO_NAME"

termAddNameMeta = "TERM_NAME"
termAddStartMeta = "START"
termAddEndMeta = "END"
termDateSetStartMeta = "START_DATE"
termDateSetEndMeta = "END_META"

courseAddNameMeta = "COURSE_NAME"
courseTeacherAddNamesMeta = "TEACHER_NAMES"
courseTeacherRemoveNamesMeta = "TEACHER_NAMES"
courseCorrectorAddNamesMeta = "CORRECTOR_NAMES"
courseCorrectorRemoveNamesMeta = "CORRECTOR_NAMES"

groupAddNameMeta = "COURSE_NAME"
groupTeacherAddNamesMeta = "TEACHER_NAMES"
groupTeacherRemoveNamesMeta = "TEACHER_NAMES"
groupCorrectorAddNamesMeta = "CORRECTOR_NAMES"
groupCorrectorRemoveNamesMeta = "CORRECTOR_NAMES"

-- Help messages
helpHelp = "(Show, Eq) this help text"

repoNodeHelp = "Repo node help"
termNodeHelp = "Term node help"
courseNodeHelp = "Course node help"
groupNodeHelp = "Group node help"

configThresholdSetCurrentHelp = "Config threshold set current help"
configThresholdSetChooseHelp = "Config threshold set choose help"
configTermDateSetTerm1Help = "Config term date set term1 help"
configTermDateSetTerm2Help = "Config term date set term2 help"
configTermDateSetTerm3Help = "Config term date set term3 help"
configProjectDateSetEndHelp = "Config project date set end help"
configProjectDateSetLateHelp = "Config project date set late help"
configAcceptExecSetWhetherHelp = "Config accept exec set whether help"
configTimeLimitSetSecondsHelp = "Config time limit set seconds help"
configSpaceLimitSetBytesHelp = "Config space limit set bytes help"
configAdminGroupsSetGroupsHelp = "Config admin groups set groups help"
configTeacherGroupsSetGroupsHelp = "Config teacher groups set groups help"
configCorrectorIsNameHelp = "Config corrector is name help"
configCorrectorAddNameHelp = "Config corrector add name help"
configCorrectorRemoveNameHelp = "Config corrector remove name help"

repoAddNameHelp = "Repo add name help"

termAddNameHelp = "Term add name help"
termAddStartHelp = "Term add start help"
termAddEndHelp = "Term add end help"
termDateSetStartHelp = "Term date set start help"
termDateSetEndHelp = "Term date set end help"

courseAddNameHelp = "Course add name help"
courseTeacherAddNamesHelp = "Course teacher add names help"
courseTeacherRemoveNamesHelp = "Course teacher remove names help"
courseCorrectorAddNamesHelp = "Course teacher add names help"
courseCorrectorRemoveNamesHelp = "Course teacher remove names help"

groupAddNameHelp = "Group add name help"
groupTeacherAddNamesHelp = "Group teacher add names help"
groupTeacherRemoveNamesHelp = "Group teacher remove names help"
groupCorrectorAddNamesHelp = "Group teacher add names help"
groupCorrectorRemoveNamesHelp = "Group teacher remove names help"


