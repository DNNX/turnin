{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Interface.CommandLineParserTest where
import Test.Framework
import Interface.CommandLineParserTest.Utils

import Interface.Lexicon
import Interface.CommandLineParser
import Interface.CommandLineParser.Config
import Interface.CommandLineParser.Repo
import Interface.CommandLineParser.Term
import Interface.CommandLineParser.Course
import Interface.CommandLineParser.Group
import Interface.CommandLineParser.Project
import Interface.CommandLineParser.Project.Validate

{-# ANN module "HLint: ignore Use camelCase" #-}

-- Config
prop_configThresholdSetSuccess cu ch =
 validOpts [cu, ch] ==>
  testSuccess (cu,ch) x [configSub, thresholdSub, setSub] (thresholdOpts cu ch) noArgs
   where x (Global(
            Config(ConfigOpts(
             ConfigThreshold(ConfigThresholdOpts(
              ConfigThresholdSet(ConfigThresholdSetOpts a b))))))) = (a,b)

prop_configThresholdListSuccess =
  testSuccess noArgsToGet x [configSub, thresholdSub, listSub] noOpts noArgs
   where x (Global(
            Config(ConfigOpts(
             ConfigThreshold(ConfigThresholdOpts(
              ConfigThresholdList ConfigThresholdListOpts)))))) = noArgsToGet

prop_configTermDateSetSuccess t1 t2 t3 =
 validOpts [t1, t2, t3] ==>
  testSuccess (t1,t2,t3) x [configSub, termDateSub, setSub] (configTermDateOpts t1 t2 t3) noArgs
   where x (Global(
            Config(ConfigOpts(
             ConfigTermDate(ConfigTermDateOpts(
              ConfigTermDateSet(ConfigTermDateSetOpts a b c))))))) = (a,b,c)

prop_configTermDateListSuccess =
  testSuccess noArgsToGet x [configSub, termDateSub, listSub] noOpts noArgs
   where x (Global(
            Config(ConfigOpts(
             ConfigTermDate(ConfigTermDateOpts(
              ConfigTermDateList ConfigTermDateListOpts)))))) = noArgsToGet

prop_configProjectDateSetSuccess end late =
 validOpts [end, late] ==>
  testSuccess (end, late) x [configSub, projectDateSub, setSub] (configProjectDateOpts end late) noArgs
   where x (Global(
            Config(ConfigOpts(
             ConfigProjectDate(ConfigProjectDateOpts(
              ConfigProjectDateSet(ConfigProjectDateSetOpts a b))))))) = (a,b)

prop_configProjectDateListSuccess =
  testSuccess noArgsToGet x [configSub, projectDateSub, listSub] noOpts noArgs
   where x (Global(
            Config(ConfigOpts(
             ConfigProjectDate(ConfigProjectDateOpts(
              ConfigProjectDateList ConfigProjectDateListOpts)))))) = noArgsToGet

prop_configAcceptExecSetSuccess v = let val = noLeadingHyphens v in
 testSuccess val x [configSub, acceptExecSub, setSub] noOpts [val]
  where x (Global(
           Config(ConfigOpts(
            ConfigAcceptExec(ConfigAcceptExecOpts(
             ConfigAcceptExecSet(ConfigAcceptExecSetOpts a))))))) = a

prop_configAcceptExecListSuccess =
  testSuccess noArgsToGet x [configSub, acceptExecSub, listSub] noOpts noArgs
   where x (Global(
            Config(ConfigOpts(
             ConfigAcceptExec(ConfigAcceptExecOpts(
              ConfigAcceptExecList ConfigAcceptExecListOpts)))))) = noArgsToGet

prop_configTimeLimitSetSuccess v = let val = noLeadingHyphens v in
 testSuccess val x [configSub, timeLimitSub, setSub] noOpts [val]
  where x (Global(
           Config(ConfigOpts(
            ConfigTimeLimit(ConfigTimeLimitOpts(
             ConfigTimeLimitSet(ConfigTimeLimitSetOpts a))))))) = a

prop_configTimeLimitListSuccess =
  testSuccess noArgsToGet x [configSub, timeLimitSub, listSub] noOpts noArgs
   where x (Global(
            Config(ConfigOpts(
             ConfigTimeLimit(ConfigTimeLimitOpts(
              ConfigTimeLimitList ConfigTimeLimitListOpts)))))) = noArgsToGet

prop_configSpaceLimitSetSuccess v = let val = noLeadingHyphens v in
 testSuccess val x [configSub, spaceLimitSub, setSub] noOpts [val]
  where x (Global(
           Config(ConfigOpts(
            ConfigSpaceLimit(ConfigSpaceLimitOpts(
             ConfigSpaceLimitSet(ConfigSpaceLimitSetOpts a))))))) = a

prop_configSpaceLimitListSuccess =
  testSuccess noArgsToGet x [configSub, spaceLimitSub, listSub] noOpts noArgs
   where x (Global(
            Config(ConfigOpts(
             ConfigSpaceLimit(ConfigSpaceLimitOpts(
              ConfigSpaceLimitList ConfigSpaceLimitListOpts)))))) = noArgsToGet

prop_configAdminGroupsSetSuccess gs = let groups = validArgs gs in groups /= [] ==>
  testSuccess groups x [configSub, adminGroupsSub, setSub] noOpts groups
   where x (Global(
            Config(ConfigOpts(
             ConfigAdminGroups(ConfigAdminGroupsOpts(
              ConfigAdminGroupsSet(ConfigAdminGroupsSetOpts a))))))) = a

prop_configAdminGroupsListSuccess =
  testSuccess noArgsToGet x [configSub, adminGroupsSub, listSub] noOpts noArgs
   where x (Global(
            Config(ConfigOpts(
             ConfigAdminGroups(ConfigAdminGroupsOpts(
              ConfigAdminGroupsList ConfigAdminGroupsListOpts)))))) = noArgsToGet

prop_configTeacherGroupsSetSuccess gs = let groups = validArgs gs in -- List can be empty
 testSuccess groups x [configSub, teacherGroupsSub, setSub] noOpts groups
  where x (Global(
           Config(ConfigOpts(
            ConfigTeacherGroups(ConfigTeacherGroupsOpts(
             ConfigTeacherGroupsSet(ConfigTeacherGroupsSetOpts a))))))) = a

prop_configTeacherGroupsListSuccess =
  testSuccess noArgsToGet x [configSub, teacherGroupsSub, listSub] noOpts noArgs
   where x (Global(
            Config(ConfigOpts(
             ConfigTeacherGroups(ConfigTeacherGroupsOpts(
              ConfigTeacherGroupsList ConfigTeacherGroupsListOpts)))))) = noArgsToGet

prop_configCorrectorIsSuccess n = let name = noLeadingHyphens n in
 testSuccess name x [configSub, correctorSub, isSub] noOpts [name]
  where x (Global(
           Config(ConfigOpts(
            ConfigCorrector(ConfigCorrectorOpts(
             ConfigCorrectorIs(ConfigCorrectorIsOpts a))))))) = a

prop_configCorrectorAddSuccess n = let name = noLeadingHyphens n in
 testSuccess name x [configSub, correctorSub, addSub] noOpts [name]
  where x (Global(
           Config(ConfigOpts(
            ConfigCorrector(ConfigCorrectorOpts(
             ConfigCorrectorAdd(ConfigCorrectorAddOpts a))))))) = a

prop_configCorrectorRemoveSuccess n = let name = noLeadingHyphens n in
 testSuccess name x [configSub, correctorSub, removeSub] noOpts [name]
  where x (Global(
           Config(ConfigOpts(
            ConfigCorrector(ConfigCorrectorOpts(
             ConfigCorrectorRemove(ConfigCorrectorRemoveOpts a))))))) = a

-- Repo
prop_repoAddSuccess n = let name = noLeadingHyphens n in
 testSuccess name x [repoSub, addSub] noOpts [name]
  where x (Global(
           Repo(RepoOpts(
            RepoAdd(RepoAddOpts a))))) = a

prop_repoRemoveSuccess repoNN =
 validOpts [repoNN] ==>
  testSuccess repoNN x [repoSub, removeSub] (repoOpts repoNN) noArgs
   where x (Global(
            Repo(RepoOpts(
             RepoRemove(RepoRemoveOpts a))))) = a

prop_repoListSuccess =
 testSuccess noArgsToGet x [repoSub, listSub] noOpts noArgs
  where x (Global(
           Repo(RepoOpts(
            RepoList RepoListOpts)))) = noArgsToGet

-- Term
prop_termAddSuccess repoNN n s e = let args = validArgs [n, s, e] in length args == 3 ==>
  validOpts [repoNN] ==> let [name, start, end] = args in
   testSuccess (repoNN, name, start, end) x [termSub, addSub] (repoOpts repoNN) args
    where x (Global(
             Term(TermOpts(
              TermAdd(TermAddOpts a b c d))))) = (a,b,c,d)

prop_termRemoveSuccess repoNN termNN =
 validOpts [repoNN, termNN] ==>
  testSuccess (repoNN,termNN) x [termSub, removeSub] (termOpts repoNN termNN) noArgs
   where x (Global(
            Term(TermOpts(
             TermRemove(TermRemoveOpts a b))))) = (a,b)

prop_termListSuccess repoNN =
 validOpts [repoNN] ==>
  testSuccess repoNN x [termSub, listSub] (repoOpts repoNN) noArgs
   where x (Global(
            Term(TermOpts(
             TermList(TermListOpts a))))) = a

prop_termDateSetSuccess repoNN termNN start end =
 validOpts [repoNN, termNN, start, end] ==>
  testSuccess (repoNN, termNN, start, end) x [termSub, dateSub, setSub] (termDateOpts repoNN termNN start end) noArgs
   where x (Global(
            Term(TermOpts(
             TermDate(TermDateOpts(
              TermDateSet(TermDateSetOpts a b c d))))))) = (a,b,c,d)

prop_termDateListSuccess repoNN termNN =
 validOpts [repoNN, termNN] ==>
  testSuccess (repoNN, termNN) x [termSub, dateSub, listSub] (termOpts repoNN termNN) noArgs
   where x (Global(
            Term(TermOpts(
             TermDate(TermDateOpts(
              TermDateList(TermDateListOpts a b))))))) = (a,b)

-- Course
prop_courseAddSuccess repoNN termNN n = let name = noLeadingHyphens n in
 validOpts [repoNN, termNN] ==>
  testSuccess (repoNN, termNN, name) x [courseSub, addSub] (termOpts repoNN termNN) [name]
   where x (Global(
            Course(CourseOpts(
             CourseAdd(CourseAddOpts a b c))))) = (a,b,c)

prop_courseRemoveSuccess repoNN termNN courseNN =
 validOpts [repoNN, termNN, courseNN] ==>
  testSuccess (repoNN, termNN, courseNN) x [courseSub, removeSub] (courseOpts repoNN termNN courseNN) noArgs
   where x (Global(
            Course(CourseOpts(
             CourseRemove(CourseRemoveOpts a b c))))) = (a,b,c)

prop_courseListSuccess repoNN termNN =
 validOpts [repoNN, termNN] ==>
  testSuccess (repoNN, termNN) x [courseSub, listSub] (termOpts repoNN termNN) noArgs
   where x (Global(
            Course(CourseOpts(
             CourseList(CourseListOpts a b))))) = (a,b)

prop_courseTeacherAddSuccess repoNN termNN courseNN ns = let names = validArgs ns in names /= [] ==>
 validOpts [repoNN, termNN, courseNN] ==>
  testSuccess (repoNN, termNN, courseNN, names) x [courseSub, teacherSub, addSub]
   (courseOpts repoNN termNN courseNN) names
       where x (Global(
                Course(CourseOpts(
                 CourseTeacher(CourseTeacherOpts(
                  CourseTeacherAdd(CourseTeacherAddOpts a b c d))))))) = (a,b,c,d)

prop_courseTeacherRemoveSuccess repoNN termNN courseNN ns = let names = validArgs ns in names /= [] ==>
 validOpts [repoNN, termNN, courseNN] ==>
  testSuccess (repoNN, termNN, courseNN, names) x [courseSub, teacherSub, removeSub]
   (courseOpts repoNN termNN courseNN) names
       where x (Global(
                Course(CourseOpts(
                 CourseTeacher(CourseTeacherOpts(
                  CourseTeacherRemove(CourseTeacherRemoveOpts a b c d))))))) = (a,b,c,d)

prop_courseTeacherListSuccess repoNN termNN courseNN =
 validOpts [repoNN, termNN, courseNN] ==>
  testSuccess (repoNN, termNN, courseNN) x [courseSub, teacherSub, listSub]
   (courseOpts repoNN termNN courseNN) noArgs
       where x (Global(
                Course(CourseOpts(
                 CourseTeacher(CourseTeacherOpts(
                  CourseTeacherList(CourseTeacherListOpts a b c))))))) = (a,b,c)


prop_courseCorrectorAddSuccess repoNN termNN courseNN ns = let names = validArgs ns in names /= [] ==>
 validOpts [repoNN, termNN, courseNN] ==>
  testSuccess (repoNN, termNN, courseNN, names) x [courseSub, correctorSub, addSub]
   (courseOpts repoNN termNN courseNN) names
       where x (Global(
                Course(CourseOpts(
                 CourseCorrector(CourseCorrectorOpts(
                  CourseCorrectorAdd(CourseCorrectorAddOpts a b c d))))))) = (a,b,c,d)

prop_courseCorrectorRemoveSuccess repoNN termNN courseNN ns = let names = validArgs ns in names /= [] ==>
 validOpts [repoNN, termNN, courseNN] ==>
  testSuccess (repoNN, termNN, courseNN, names) x [courseSub, correctorSub, removeSub]
   (courseOpts repoNN termNN courseNN) names
       where x (Global(
                Course(CourseOpts(
                 CourseCorrector(CourseCorrectorOpts(
                  CourseCorrectorRemove(CourseCorrectorRemoveOpts a b c d))))))) = (a,b,c,d)

prop_courseCorrectorListSuccess repoNN termNN courseNN =
 validOpts [repoNN, termNN, courseNN] ==>
  testSuccess (repoNN, termNN, courseNN) x [courseSub, correctorSub, listSub]
   (courseOpts repoNN termNN courseNN) noArgs
       where x (Global(
                Course(CourseOpts(
                 CourseCorrector(CourseCorrectorOpts(
                  CourseCorrectorList(CourseCorrectorListOpts a b c))))))) = (a,b,c)

-- Group
prop_groupAddSuccess repoNN termNN courseNN n = let name = noLeadingHyphens n in
 validOpts [repoNN, termNN, courseNN] ==>
  testSuccess (repoNN, termNN, courseNN, name) x [groupSub, addSub]
   (courseOpts repoNN termNN courseNN) [name]
    where x (Global(
             Group(GroupOpts(
              GroupAdd(GroupAddOpts a b c d))))) = (a,b,c,d)

prop_groupRemoveSuccess repoNN termNN courseNN groupNN =
 validOpts [repoNN, termNN, courseNN, groupNN] ==>
  testSuccess (repoNN, termNN, courseNN, groupNN) x [groupSub, removeSub]
   (groupOpts repoNN termNN courseNN groupNN) noArgs
    where x (Global(
             Group(GroupOpts(
              GroupRemove(GroupRemoveOpts a b c d))))) = (a,b,c,d)

prop_groupListSuccess repoNN termNN courseNN =
 validOpts [repoNN, termNN, courseNN] ==>
  testSuccess (repoNN, termNN, courseNN) x [groupSub, listSub] (courseOpts repoNN termNN courseNN) noArgs
   where x (Global(
            Group(GroupOpts(
             GroupList(GroupListOpts a b c))))) = (a,b,c)

prop_groupTeacherAddSuccess repoNN termNN courseNN groupNN ns = let names = validArgs ns in names /= [] ==>
 validOpts [repoNN, termNN, courseNN, groupNN] ==>
  testSuccess (repoNN, termNN, courseNN, groupNN, names) x
   [groupSub, teacherSub, addSub] (groupOpts repoNN termNN courseNN groupNN) names
       where x (Global(
                Group(GroupOpts(
                 GroupTeacher(GroupTeacherOpts(
                  GroupTeacherAdd(GroupTeacherAddOpts a b c d e))))))) = (a,b,c,d, e)

prop_groupTeacherRemoveSuccess repoNN termNN courseNN groupNN ns = let names = validArgs ns in names /= [] ==>
 validOpts [repoNN, termNN, courseNN, groupNN] ==>
  testSuccess (repoNN, termNN, courseNN, groupNN, names) x [groupSub, teacherSub, removeSub]
   (groupOpts repoNN termNN courseNN groupNN) names
       where x (Global(
                Group(GroupOpts(
                 GroupTeacher(GroupTeacherOpts(
                  GroupTeacherRemove(GroupTeacherRemoveOpts a b c d e))))))) = (a,b,c,d,e)

prop_groupTeacherListSuccess repoNN termNN courseNN groupNN =
 validOpts [repoNN, termNN, courseNN, groupNN] ==>
  testSuccess (repoNN, termNN, courseNN, groupNN) x [groupSub, teacherSub, listSub]
   (groupOpts repoNN termNN courseNN groupNN) noArgs
    where x (Global(
             Group(GroupOpts(
              GroupTeacher(GroupTeacherOpts(
               GroupTeacherList(GroupTeacherListOpts a b c d))))))) = (a,b,c,d)


prop_groupCorrectorAddSuccess repoNN termNN courseNN groupNN ns = let names = validArgs ns in names /= [] ==>
 validOpts [repoNN, termNN, courseNN, groupNN] ==>
  testSuccess (repoNN, termNN, courseNN, groupNN, names) x [groupSub, correctorSub, addSub]
   (groupOpts repoNN termNN courseNN groupNN) names
       where x (Global(
                Group(GroupOpts(
                 GroupCorrector(GroupCorrectorOpts(
                  GroupCorrectorAdd(GroupCorrectorAddOpts a b c d e))))))) = (a,b,c,d,e)

prop_groupCorrectorRemoveSuccess repoNN termNN courseNN groupNN ns = let names = validArgs ns in names /= [] ==>
 validOpts [repoNN, termNN, courseNN, groupNN] ==>
  testSuccess (repoNN, termNN, courseNN, groupNN, names) x [groupSub, correctorSub, removeSub]
   (groupOpts repoNN termNN courseNN groupNN) names
       where x (Global(
                Group(GroupOpts(
                 GroupCorrector(GroupCorrectorOpts(
                  GroupCorrectorRemove(GroupCorrectorRemoveOpts a b c d e))))))) = (a,b,c,d, e)

prop_groupCorrectorListSuccess repoNN termNN courseNN groupNN =
 validOpts [repoNN, termNN, courseNN, groupNN] ==>
  testSuccess (repoNN, termNN, courseNN, groupNN) x [groupSub, correctorSub, listSub]
   (groupOpts repoNN termNN courseNN groupNN) noArgs
       where x (Global(
                Group(GroupOpts(
                 GroupCorrector(GroupCorrectorOpts(
                  GroupCorrectorList(GroupCorrectorListOpts a b c d))))))) = (a,b,c,d)

-- Project
prop_projectAddSuccess repoNN termNN courseNN groupNN start end late n = let ns = validArgs [n] in ns /= [] ==>
 validOpts [repoNN, termNN, courseNN, groupNN, start, end, late] ==> let name = head ns in
  testSuccess (repoNN, termNN, courseNN, groupNN, start, end, late, name) x
   [projectSub, addSub] (projectAddOpts repoNN termNN courseNN groupNN start end late) [name]
   where x (Global(
            Project(ProjectOpts(
             ProjectAdd(ProjectAddOpts a b c d e f g h))))) = (a,b,c,d,e,f,g,h)

prop_projectRemoveSuccess repoNN termNN courseNN groupNN projectNN =
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess (repoNN, termNN, courseNN, groupNN, projectNN) x
   [projectSub, removeSub] (projectOpts repoNN termNN courseNN groupNN projectNN) noArgs
    where x (Global(
             Project(ProjectOpts(
              ProjectRemove(ProjectRemoveOpts a b c d e))))) = (a,b,c,d,e)

prop_projectListSuccess repoNN termNN courseNN groupNN =
 validOpts [repoNN, termNN, courseNN, groupNN] ==>
  testSuccess (repoNN, termNN, courseNN, groupNN) x
   [projectSub, listSub] (groupOpts repoNN termNN courseNN groupNN) noArgs
    where x (Global(
             Project(ProjectOpts(
              ProjectList(ProjectListOpts a b c d))))) = (a,b,c,d)

prop_projectDateSetSuccess repoNN termNN courseNN groupNN projectNN start end late =
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN, start, end, late] ==>
  testSuccess (repoNN, termNN, courseNN, groupNN, projectNN, start, end, late) x [projectSub, dateSub, setSub]
  (projectDateOpts repoNN termNN courseNN groupNN projectNN start end late) noArgs
    where x (Global(
             Project(ProjectOpts(
              ProjectDate(ProjectDateOpts(
               ProjectDateSet(ProjectDateSetOpts a b c d e f g h))))))) = (a,b,c,d,e,f,g,h)

prop_projectDateListSuccess repoNN termNN courseNN groupNN projectNN =
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess (repoNN, termNN, courseNN, groupNN, projectNN) x [projectSub, dateSub, listSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) noArgs
    where x (Global(
             Project(ProjectOpts(
              ProjectDate(ProjectDateOpts(
               ProjectDateList(ProjectDateListOpts a b c d e))))))) = (a,b,c,d,e)


prop_projectValidateAcceptExecSetSuccess repoNN termNN courseNN groupNN projectNN v = let val = ["yYnN" !! (v `mod` 4)] in
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess (repoNN, termNN, courseNN, groupNN, projectNN, val) x [projectSub, validateSub, acceptExecSub, setSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) [val]
    where x (Global(
             Project(ProjectOpts(
              ProjectValidate(ProjectValidateOpts(
               ProjectValidateAcceptExec(ProjectValidateAcceptExecOpts(
                ProjectValidateAcceptExecSet(ProjectValidateAcceptExecSetOpts a b c d e f))))))))) = (a,b,c,d,e,f)

prop_projectValidateAcceptExecListSuccess repoNN termNN courseNN groupNN projectNN =
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess (repoNN, termNN, courseNN, groupNN, projectNN) x [projectSub, validateSub, acceptExecSub, listSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) noArgs
    where x (Global(
             Project(ProjectOpts(
              ProjectValidate(ProjectValidateOpts(
               ProjectValidateAcceptExec(ProjectValidateAcceptExecOpts(
                ProjectValidateAcceptExecList(ProjectValidateAcceptExecListOpts a b c d e))))))))) = (a,b,c,d,e)

prop_projectValidateNameAddSuccess repoNN termNN courseNN groupNN projectNN ns = let names = validArgs ns in names /= [] ==>
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess (repoNN, termNN, courseNN, groupNN, projectNN, names) x [projectSub, validateSub, nameSub, addSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) names
    where x (Global(
             Project(ProjectOpts(
              ProjectValidate(ProjectValidateOpts(
               ProjectValidateName(ProjectValidateNameOpts(
                ProjectValidateNameAdd(ProjectValidateNameAddOpts a b c d e f))))))))) = (a,b,c,d,e,f)


prop_projectValidateNameRemoveSuccess repoNN termNN courseNN groupNN projectNN ns = let names = validArgs ns in names /= [] ==>
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess (repoNN, termNN, courseNN, groupNN, projectNN, names) x [projectSub, validateSub, nameSub, removeSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) names
    where x (Global(
             Project(ProjectOpts(
              ProjectValidate(ProjectValidateOpts(
               ProjectValidateName(ProjectValidateNameOpts(
                ProjectValidateNameRemove(ProjectValidateNameRemoveOpts a b c d e f))))))))) = (a,b,c,d,e,f)


prop_projectValidateNameListSuccess repoNN termNN courseNN groupNN projectNN =
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess (repoNN, termNN, courseNN, groupNN, projectNN) x [projectSub, validateSub, nameSub, listSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) noArgs
    where x (Global(
             Project(ProjectOpts(
              ProjectValidate(ProjectValidateOpts(
               ProjectValidateName(ProjectValidateNameOpts(
                ProjectValidateNameList(ProjectValidateNameListOpts a b c d e))))))))) = (a,b,c,d,e)


prop_projectValidateCommandSetSuccess repoNN termNN courseNN groupNN projectNN co = let cs = validArgs [co] in cs /= [] ==>
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==> let command = head cs in
  testSuccess (repoNN, termNN, courseNN, groupNN, projectNN, command) x [projectSub, validateSub, commandSub, setSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) [command]
    where x (Global(
             Project(ProjectOpts(
              ProjectValidate(ProjectValidateOpts(
               ProjectValidateCommand(ProjectValidateCommandOpts(
                ProjectValidateCommandSet(ProjectValidateCommandSetOpts a b c d e f))))))))) = (a,b,c,d,e,f)


prop_projectValidateCommandUnsetSuccess repoNN termNN courseNN groupNN projectNN =
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess (repoNN, termNN, courseNN, groupNN, projectNN) x [projectSub, validateSub, commandSub, unsetSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) noArgs
    where x (Global(
             Project(ProjectOpts(
              ProjectValidate(ProjectValidateOpts(
               ProjectValidateCommand(ProjectValidateCommandOpts(
                ProjectValidateCommandUnset(ProjectValidateCommandUnsetOpts a b c d e))))))))) = (a,b,c,d,e)


prop_projectValidateCommandListSuccess repoNN termNN courseNN groupNN projectNN =
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess (repoNN, termNN, courseNN, groupNN, projectNN) x [projectSub, validateSub, commandSub, listSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) noArgs
    where x (Global(
             Project(ProjectOpts(
              ProjectValidate(ProjectValidateOpts(
               ProjectValidateCommand(ProjectValidateCommandOpts(
                ProjectValidateCommandList(ProjectValidateCommandListOpts a b c d e))))))))) = (a,b,c,d,e)


prop_projectValidateScriptSetSuccess repoNN termNN courseNN groupNN projectNN s = let ss = validArgs [s] in ss /= [] ==>
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==> let scriptName = head ss in
  testSuccess (repoNN, termNN, courseNN, groupNN, projectNN, scriptName) x [projectSub, validateSub, scriptSub, setSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) [scriptName]
    where x (Global(
             Project(ProjectOpts(
              ProjectValidate(ProjectValidateOpts(
               ProjectValidateScript(ProjectValidateScriptOpts(
                ProjectValidateScriptSet(ProjectValidateScriptSetOpts a b c d e f))))))))) = (a,b,c,d,e,f)


prop_projectValidateScriptUnsetSuccess repoNN termNN courseNN groupNN projectNN =
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess (repoNN, termNN, courseNN, groupNN, projectNN) x [projectSub, validateSub, scriptSub, unsetSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) noArgs
    where x (Global(
             Project(ProjectOpts(
              ProjectValidate(ProjectValidateOpts(
               ProjectValidateScript(ProjectValidateScriptOpts(
                ProjectValidateScriptUnset(ProjectValidateScriptUnsetOpts a b c d e))))))))) = (a,b,c,d,e)


prop_projectValidateScriptListSuccess repoNN termNN courseNN groupNN projectNN =
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess (repoNN, termNN, courseNN, groupNN, projectNN) x [projectSub, validateSub, scriptSub, listSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) noArgs
    where x (Global(
             Project(ProjectOpts(
              ProjectValidate(ProjectValidateOpts(
               ProjectValidateScript(ProjectValidateScriptOpts(
                ProjectValidateScriptList(ProjectValidateScriptListOpts a b c d e))))))))) = (a,b,c,d,e)


prop_projectValidateScriptExtractSuccess repoNN termNN courseNN groupNN projectNN di = let ds = validArgs [di] in ds /= [] ==>
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==> let extractDir = head ds in
  testSuccess (repoNN, termNN, courseNN, groupNN, projectNN, extractDir) x [projectSub, validateSub, scriptSub, extractSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) [extractDir]
    where x (Global(
             Project(ProjectOpts(
              ProjectValidate(ProjectValidateOpts(
               ProjectValidateScript(ProjectValidateScriptOpts(
                ProjectValidateScriptExtract(ProjectValidateScriptExtractOpts a b c d e f))))))))) = (a,b,c,d,e,f)


          