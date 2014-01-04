module Interface.Lexicon where

-- Commands
configSub = "config"
repoSub = "repo"
termSub = "term"
courseSub = "course"
groupSub = "group"
projectSub = "project"

thresholdSub = "threshold"
termDateSub = "termDate"
projectDateSub = "projectDate"
acceptExecSub = "acceptExec"
timeLimitSub = "timeLimit"
spaceLimitSub = "spaceLimit"
adminGroupsSub = "adminGroups"
teacherGroupsSub = "teacherGroups"

validateSub = "validate"
worktrainSub = "worktrain"
submitSub = "submit"

nameSub = "name"
commandSub = "command"
scriptSub = "script"
fileSub = "file"
extractSub = "extract"

teacherSub = "teacher"
correctorSub = "corrector"

addSub = "add"
removeSub = "remove"
listSub = "list"

setSub = "set"
unsetSub = "unset"
dateSub = "date"
lateSub = "late"
inspectSub = "inspect"
isSub = "is"

-- Options
helpOpt = ('?', "help")

repoNodeOpt = ('r', "repo")
termNodeOpt = ('t', "term")
courseNodeOpt = ('c', "course")
groupNodeOpt = ('g', "group")
projectNodeOpt = ('p', "project")

configThresholdSetCurrentOpt = ('u', "current")
configThresholdSetChooseOpt = ( 'h', "choose")
configTermDateSetTerm1Opt = ('1', "term1")
configTermDateSetTerm2Opt = ('2', "term2")
configTermDateSetTerm3Opt = ('3', "term3")
configProjectDateSetEndOpt = ('e', "end")
configProjectDateSetLateOpt = ('l', "late")

termDateSetStartOpt = ('s', "start")
termDateSetEndOpt = ('e', "end")

projectAddStartOpt = ('s', "start")
projectAddEndOpt = ('e', "end")
projectAddLateOpt = ('l', "late")
projectDateSetStartOpt = ('s', "start")
projectDateSetEndOpt = ('e', "end")
projectDateSetLateOpt = ('l', "late")

-- Descriptions
globalDesc = "Global desc"
globalHeader = "Global header"

configDesc = "Config desc"
repoDesc = "Repo desc"
termDesc = "Term desc"
courseDesc = "Course desc"
groupDesc = "Group desc"
projectDesc = "Project desc"

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

projectAddDesc = "Project add desc"
projectRemoveDesc = "Project remove desc"
projectListDesc = "Project list desc"
projectDateDesc = "Project date desc"
projectDateSetDesc = "Project date set desc"
projectDateListDesc = "Project date list desc"
projectValidateDesc = "Project validate desc"
projectValidateAcceptExecDesc = "Project validate accept exec desc"
projectValidateAcceptExecSetDesc = "Project validate accept exec set desc"
projectValidateAcceptExecListDesc = "Project validate accept exec list desc"
projectValidateNameDesc = "Project validate name desc"
projectValidateNameAddDesc = "Project validate name add desc"
projectValidateNameRemoveDesc = "Project validate name remove desc"
projectValidateNameListDesc = "Project validate name list desc"
projectValidateCommandDesc = "Project validate command desc"
projectValidateCommandSetDesc = "Project validate command set desc"
projectValidateCommandUnsetDesc = "Project validate command unset desc"
projectValidateCommandListDesc = "Project validate command list desc"
projectValidateScriptDesc = "Project validate script desc"
projectValidateScriptSetDesc = "Project validate script set desc"
projectValidateScriptUnsetDesc = "Project validate script unset desc"
projectValidateScriptListDesc = "Project validate script list desc"
projectValidateScriptExtractDesc = "Project validate script extract desc"

projectWorktrainDesc = "Project worktrain desc"   
projectWorktrainScriptDesc = "Project worktrain script desc"        
projectWorktrainScriptSetDesc = "Project worktrain script set desc"     
projectWorktrainScriptUnsetDesc = "Project worktrain script unser desc"   
projectWorktrainScriptListDesc = "Project worktrain script list desc"   
projectWorktrainScriptExtractDesc = "Project worktrain script extract desc"
projectWorktrainFileDesc = "Project worktrain file desc"          
projectWorktrainFileAddDesc = "Project worktrain file add desc"       
projectWorktrainFileRemoveDesc = "Project worktrain file remove desc"    
projectWorktrainFileListDesc = "Project worktrain file list desc"      
projectWorktrainFileExtractDesc = "Project worktrain file extract desc"   
projectWorktrainTimeLimitDesc = "Project worktrain time limit desc"     
projectWorktrainTimeLimitSetDesc = "Project worktrain time limit set desc"  
projectWorktrainTimeLimitListDesc = "Project worktrain time limit list desc" 
projectWorktrainSpaceLimitDesc = "Project worktrain space limit desc"    
projectWorktrainSpaceLimitSetDesc = "Project worktrain space limit set desc" 
projectWorktrainSpaceLimitListDesc = "Project worktrain space limit list desc"

projectSubmitDesc = "Project submit desc"    
projectSubmitListDesc = "Project submit list desc"   
projectSubmitLateDesc = "Project submit late desc"   
projectSubmitInspectDesc = "Project submit inspect desc"
projectSubmitExtractDesc = "Project submit extract desc"

-- Metavars
repoNodeMeta = "REPO_NAME"
termNodeMeta = "TERM_NAME"
courseNodeMeta = "COURSE_NAME"
groupNodeMeta = "GROUP_NAME"
projectNodeMeta = "PROJECT_NAME"

configThresholdSetCurrentMeta = "CURRENT"
configThresholdSetChooseMeta = "CHOOSE"
configTermDateSetTerm1Meta = "TERM1"
configTermDateSetTerm2Meta = "TERM2"
configTermDateSetTerm3Meta = "TERM3"
configProjectDateSetEndMeta = "END"
configProjectDateSetLateMeta = "LATE"
configAcceptExecSetMeta = "ACCEPT_EXEC"
configTimeLimitSetMeta = "TIME_LIMIT"
configSpaceLimitSetMeta = "SPACE_LIMIT"
configAdminGroupsSetMeta = "ADMIN_GROUPS"
configTeacherGroupsSetMeta = "TEACHER_GROUPS"
configCorrectorIsMeta = "CORR_NAME"
configCorrectorAddMeta = "CORR_NAME"
configCorrectorRemoveMeta = "CORR_NAME"

repoAddMeta = "REPO_NAME"

termAddNameMeta = "TERM_NAME"
termAddStartMeta = "START"
termAddEndMeta = "END"
termDateSetStartMeta = "START_DATE"
termDateSetEndMeta = "END_META"

courseAddMeta = "COURSE_NAME"
courseTeacherAddMeta = "TEACHER_NAMES"
courseTeacherRemoveMeta = "TEACHER_NAMES"
courseCorrectorAddMeta = "CORRECTOR_NAMES"
courseCorrectorRemoveMeta = "CORRECTOR_NAMES"

groupAddMeta = "COURSE_NAME"
groupTeacherAddMeta = "TEACHER_NAMES"
groupTeacherRemoveMeta = "TEACHER_NAMES"
groupCorrectorAddMeta = "CORRECTOR_NAMES"
groupCorrectorRemoveMeta = "CORRECTOR_NAMES"

projectAddNameMeta = "PROJECT_NAME"
projectAddStartMeta = "START_META"
projectAddEndMeta = "END_META"
projectAddLateMeta = "LATE_META"
projectDateSetStartMeta = "START_META"
projectDateSetEndMeta = "END_META"
projectDateSetLateMeta = "LATE_META"

projectValidateAcceptExecSetMeta = "ACCEPT_EXEC"
projectValidateNameAddMeta = "NAMES"
projectValidateNameRemoveMeta = "NAMES"
projectValidateCommandSetMeta = "COMMAND"
projectValidateScriptSetMeta = "SCRIPT_NAME"
projectValidateScriptExtractMeta = "DIRECTORY"

projectWorktrainScriptSetMeta = "SCRIPT_NAME"
projectWorktrainScriptExtractMeta = "DIRECTORY"
projectWorktrainFileAddMeta = "NAMES"
projectWorktrainFileRemoveMeta = "NAMES"
projectWorktrainFileExtractDirMeta = "DIRECTORY"
projectWorktrainFileExtractNamesMeta = "NAMES"
projectWorktrainTimeLimitSetMeta = "TIME_LIMIT"
projectWorktrainSpaceLimitSetMeta = "SPACE_LIMIT"

projectSubmitInspectMeta = "KEYS"
projectSubmitExtractDirMeta = "DIRECTORY"
projectSubmitExtractKeysMeta = "KEYS"

-- Argument help messages
helpHelp = "(Show, Eq) this help text"

repoNodeHelp = "Repo node help"
termNodeHelp = "Term node help"
courseNodeHelp = "Course node help"
groupNodeHelp = "Group node help"
projectNodeHelp = "Project node help"

configThresholdSetCurrentHelp = "Config threshold set current help"
configThresholdSetChooseHelp = "Config threshold set choose help"
configTermDateSetTerm1Help = "Config term date set term1 help"
configTermDateSetTerm2Help = "Config term date set term2 help"
configTermDateSetTerm3Help = "Config term date set term3 help"
configProjectDateSetEndHelp = "Config project date set end help"
configProjectDateSetLateHelp = "Config project date set late help"
configAcceptExecSetHelp = "Config accept exec set whether help"
configTimeLimitSetHelp = "Config time limit set seconds help"
configSpaceLimitSetHelp = "Config space limit set bytes help"
configAdminGroupsSetHelp = "Config admin groups set groups help"
configTeacherGroupsSetHelp = "Config teacher groups set groups help"
configCorrectorIsHelp = "Config corrector is name help"
configCorrectorAddHelp = "Config corrector add name help"
configCorrectorRemoveHelp = "Config corrector remove name help"

repoAddHelp = "Repo add name help"

termAddNameHelp = "Term add name help"
termAddStartHelp = "Term add start help"
termAddEndHelp = "Term add end help"
termDateSetStartHelp = "Term date set start help"
termDateSetEndHelp = "Term date set end help"

courseAddHelp = "Course add name help"
courseTeacherAddHelp = "Course teacher add names help"
courseTeacherRemoveHelp = "Course teacher remove names help"
courseCorrectorAddHelp = "Course teacher add names help"
courseCorrectorRemoveHelp = "Course teacher remove names help"

groupAddHelp = "Group add name help"
groupTeacherAddHelp = "Group teacher add names help"
groupTeacherRemoveHelp = "Group teacher remove names help"
groupCorrectorAddHelp = "Group teacher add names help"
groupCorrectorRemoveHelp = "Group teacher remove names help"

projectAddNameHelp = "Project add name help"
projectAddStartHelp = "Project add start help"
projectAddEndHelp = "Project add end help"
projectAddLateHelp = "Project add late help"
projectDateSetStartHelp = "Project date set start help"
projectDateSetEndHelp = "Project date set end help"
projectDateSetLateHelp = "Project date set late help"

projectValidateAcceptExecSetHelp = "Project validate accept exec set help"
projectValidateNameAddHelp = "Project validate name add help"
projectValidateNameRemoveHelp = "Project validate name remove help"
projectValidateCommandSetHelp = "Project validate command set help"
projectValidateScriptSetHelp = "Project validate script set help"
projectValidateScriptExtractHelp = "Project validate script extract help"

projectWorktrainScriptSetHelp = "Project worktrain script set help"
projectWorktrainScriptExtractHelp = "Project worktrain script extract help"
projectWorktrainFileAddHelp = "Project worktrain file add help"
projectWorktrainFileRemoveHelp = "Project worktrain file remove help"
projectWorktrainFileExtractDirHelp = "Project worktrain file extract dir help"
projectWorktrainFileExtractNamesHelp = "Project worktrain file extract names help"
projectWorktrainTimeLimitSetHelp = "Project worktrain time limit set help"
projectWorktrainSpaceLimitSetHelp = "Project worktrain space limit set help"

projectSubmitInspectHelp = "Project submit inspect help"
projectSubmitExtractDirHelp = "Project submit extract dir help"
projectSubmitExtractKeysHelp = "Project submit extract keys help"
