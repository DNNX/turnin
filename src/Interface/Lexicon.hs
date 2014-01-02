module Interface.Lexicon where

-- Commands
configSub = "config"
repoSub = "repo"
termSub = "term"

thresholdSub = "threshold"
termDateSub = "termDate"
projectDateSub = "projectDate"
acceptExecSub = "acceptExec"
timeLimitSub = "timeLimit"
spaceLimitSub = "spaceLimit"
adminGroupsSub = "adminGroups"
teacherGroupsSub = "teacherGroups"
correctorSub = "correctorSub"

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

-- Metavars
repoNodeMeta = "REPO_NAME"
termNodeMeta = "TERM_NAME"

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

-- Help messages
helpHelp = "Show this help text"

repoNodeHelp = "Repo node help"
termNodeHelp = "Term node help"

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

