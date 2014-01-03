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

{-# ANN module "HLint: ignore Use camelCase" #-}
 
-- Config  
prop_configThresholdSetSuccess cu ch =
 validOpts [cu, ch] ==>
  testSuccess (cu,ch) f [configSub, thresholdSub, setSub] (thresholdOpts cu ch) noArgs
   where f (Global(
            Config(ConfigOpts(
             ConfigThreshold(ConfigThresholdOpts(
              ConfigThresholdSet(ConfigThresholdSetOpts a b))))))) = (a,b)

prop_configThresholdListSuccess =
  testSuccess noArgsToGet f [configSub, thresholdSub, listSub] noOpts noArgs
   where f (Global(
            Config(ConfigOpts(
             ConfigThreshold(ConfigThresholdOpts(
              ConfigThresholdList ConfigThresholdListOpts)))))) = noArgsToGet

prop_configTermDateSetSuccess t1 t2 t3 =
 validOpts [t1, t2, t3] ==>
  testSuccess (t1,t2,t3) f [configSub, termDateSub, setSub] (configTermDateOpts t1 t2 t3) noArgs
   where f (Global(
            Config(ConfigOpts( 
             ConfigTermDate(ConfigTermDateOpts(
              ConfigTermDateSet(ConfigTermDateSetOpts a b c))))))) = (a,b,c)

prop_configTermDateListSuccess =
  testSuccess noArgsToGet f [configSub, termDateSub, listSub] noOpts noArgs
   where f (Global(
            Config(ConfigOpts(
             ConfigTermDate(ConfigTermDateOpts(
              ConfigTermDateList ConfigTermDateListOpts)))))) = noArgsToGet

prop_configProjectDateSetSuccess end late =
 validOpts [end, late] ==>
  testSuccess (end, late) f [configSub, projectDateSub, setSub] (configProjectDateOpts end late) noArgs
   where f (Global(
            Config(ConfigOpts(
             ConfigProjectDate(ConfigProjectDateOpts(
              ConfigProjectDateSet(ConfigProjectDateSetOpts a b))))))) = (a,b)

prop_configProjectDateListSuccess =
  testSuccess noArgsToGet f [configSub, projectDateSub, listSub] noOpts noArgs
   where f (Global(
            Config(ConfigOpts(
             ConfigProjectDate(ConfigProjectDateOpts(
              ConfigProjectDateList ConfigProjectDateListOpts)))))) = noArgsToGet

prop_configAcceptExecSetSuccess v = let val = ["yYnN" !! (v `mod` 4)] in 
 testSuccess val f [configSub, acceptExecSub, setSub] noOpts [val]
  where f (Global(
           Config(ConfigOpts(
            ConfigAcceptExec(ConfigAcceptExecOpts(
             ConfigAcceptExecSet(ConfigAcceptExecSetOpts a))))))) = a

prop_configAcceptExecListSuccess =
  testSuccess noArgsToGet f [configSub, acceptExecSub, listSub] noOpts noArgs
   where f (Global(
            Config(ConfigOpts(
             ConfigAcceptExec(ConfigAcceptExecOpts(
              ConfigAcceptExecList ConfigAcceptExecListOpts)))))) = noArgsToGet

prop_configTimeLimitSetSuccess v = let val = show $ abs (v :: Double) in 
 testSuccess val f [configSub, timeLimitSub, setSub] noOpts [val]
  where f (Global(
           Config(ConfigOpts(
            ConfigTimeLimit(ConfigTimeLimitOpts(
             ConfigTimeLimitSet(ConfigTimeLimitSetOpts a))))))) = a

prop_configTimeLimitListSuccess =
  testSuccess noArgsToGet f [configSub, timeLimitSub, listSub] noOpts noArgs
   where f (Global(
            Config(ConfigOpts(
             ConfigTimeLimit(ConfigTimeLimitOpts(
              ConfigTimeLimitList ConfigTimeLimitListOpts)))))) = noArgsToGet

prop_configSpaceLimitSetSuccess v = let val = show $ abs (v :: Integer) in
 testSuccess val f [configSub, spaceLimitSub, setSub] noOpts [val]
  where f (Global(
           Config(ConfigOpts(
            ConfigSpaceLimit(ConfigSpaceLimitOpts(
             ConfigSpaceLimitSet(ConfigSpaceLimitSetOpts a))))))) = a

prop_configSpaceLimitListSuccess =
  testSuccess noArgsToGet f [configSub, spaceLimitSub, listSub] noOpts noArgs
   where f (Global(
            Config(ConfigOpts(
             ConfigSpaceLimit(ConfigSpaceLimitOpts(
              ConfigSpaceLimitList ConfigSpaceLimitListOpts)))))) = noArgsToGet

prop_configAdminGroupsSetSuccess gs = let groups = validArgs gs in groups /= [] ==>
  testSuccess groups f [configSub, adminGroupsSub, setSub] noOpts groups
   where f (Global(
            Config(ConfigOpts( 
             ConfigAdminGroups(ConfigAdminGroupsOpts(
              ConfigAdminGroupsSet(ConfigAdminGroupsSetOpts a))))))) = a

prop_configAdminGroupsListSuccess =
  testSuccess noArgsToGet f [configSub, adminGroupsSub, listSub] noOpts noArgs
   where f (Global(
            Config(ConfigOpts(
             ConfigAdminGroups(ConfigAdminGroupsOpts(
              ConfigAdminGroupsList ConfigAdminGroupsListOpts)))))) = noArgsToGet

prop_configTeacherGroupsSetSuccess gs = let groups = validArgs gs in -- List can be empty
 testSuccess groups f [configSub, teacherGroupsSub, setSub] noOpts groups
  where f (Global(
           Config(ConfigOpts(
            ConfigTeacherGroups(ConfigTeacherGroupsOpts(
             ConfigTeacherGroupsSet(ConfigTeacherGroupsSetOpts a))))))) = a

prop_configTeacherGroupsListSuccess =
  testSuccess noArgsToGet f [configSub, teacherGroupsSub, listSub] noOpts noArgs
   where f (Global(
            Config(ConfigOpts(
             ConfigTeacherGroups(ConfigTeacherGroupsOpts(
              ConfigTeacherGroupsList ConfigTeacherGroupsListOpts)))))) = noArgsToGet

prop_configCorrectorIsSuccess n = let name = noLeadingHyphens n in
 testSuccess name f [configSub, correctorSub, isSub] noOpts [name]
  where f (Global(
           Config(ConfigOpts(
            ConfigCorrector(ConfigCorrectorOpts(
             ConfigCorrectorIs(ConfigCorrectorIsOpts a))))))) = a

prop_configCorrectorAddSuccess n = let name = noLeadingHyphens n in
 testSuccess name f [configSub, correctorSub, addSub] noOpts [name]
  where f (Global(
           Config(ConfigOpts(
            ConfigCorrector(ConfigCorrectorOpts(
             ConfigCorrectorAdd(ConfigCorrectorAddOpts a))))))) = a

prop_configCorrectorRemoveSuccess n = let name = noLeadingHyphens n in
 testSuccess name f [configSub, correctorSub, removeSub] noOpts [name]
  where f (Global(
           Config(ConfigOpts(
            ConfigCorrector(ConfigCorrectorOpts(
             ConfigCorrectorRemove(ConfigCorrectorRemoveOpts a))))))) = a
                 
-- Repo                
prop_repoAddSuccess n = let name = noLeadingHyphens n in
 testSuccess name f [repoSub, addSub] noOpts [name]
  where f (Global(
           Repo(RepoOpts(
            RepoAdd(RepoAddOpts a))))) = a
                
prop_repoRemoveSuccess repoNN =
 validOpts [repoNN] ==>
  testSuccess repoNN f [repoSub, removeSub] (repoOpts repoNN) noArgs
   where f (Global(
            Repo(RepoOpts(
             RepoRemove(RepoRemoveOpts a))))) = a
            
prop_repoListSuccess =
 testSuccess noArgsToGet f [repoSub, listSub] noOpts noArgs
  where f (Global(
           Repo(RepoOpts(
            RepoList RepoListOpts)))) = noArgsToGet
 
-- Term         
prop_termAddSuccess repoNN n s e = let args = validArgs [n, s, e] in length args == 3 ==>
 let [name, start, end] = args in
  validOpts [repoNN] ==>
   testSuccess (repoNN, name, start, end) f [termSub, addSub] (repoOpts repoNN) args
    where f (Global(
             Term(TermOpts( 
              TermAdd(TermAddOpts a b c d))))) = (a,b,c,d)
                
prop_termRemoveSuccess repoNN termNN =
 validOpts [repoNN, termNN] ==>
  testSuccess (repoNN,termNN) f [termSub, removeSub] (termOpts repoNN termNN) noArgs
   where f (Global(
            Term(TermOpts(
             TermRemove(TermRemoveOpts a b))))) = (a,b)
             
prop_termListSuccess repoNN =
 validOpts [repoNN] ==>
  testSuccess repoNN f [termSub, listSub] (repoOpts repoNN) noArgs
   where f (Global(
            Term(TermOpts(
             TermList(TermListOpts a))))) = a
             
prop_termDateSetSuccess repoNN termNN start end =
 validOpts [repoNN, termNN, start, end] ==>
  testSuccess (repoNN, termNN, start, end) f [termSub, dateSub, setSub] (termDateOpts repoNN termNN start end) noArgs
   where f (Global(
            Term(TermOpts(
             TermDate(TermDateOpts(
              TermDateSet(TermDateSetOpts a b c d))))))) = (a,b,c,d)
 
prop_termDateListSuccess repoNN termNN =
 validOpts [repoNN, termNN] ==>
  testSuccess (repoNN, termNN) f [termSub, dateSub, listSub] (termOpts repoNN termNN) noArgs
   where f (Global(
            Term(TermOpts(
             TermDate(TermDateOpts(
              TermDateList(TermDateListOpts a b))))))) = (a,b)
                      
-- Course            
prop_courseAddSuccess repoNN termNN n = let name = noLeadingHyphens n in
 validOpts [repoNN, termNN] ==>
  testSuccess (repoNN, termNN, name) f [courseSub, addSub] (termOpts repoNN termNN) [name]
   where f (Global(
            Course(CourseOpts(
             CourseAdd(CourseAddOpts a b c))))) = (a,b,c)
                
prop_courseRemoveSuccess repoNN termNN courseNN =
 validOpts [repoNN, termNN, courseNN] ==>
  testSuccess (repoNN, termNN, courseNN) f [courseSub, removeSub] (courseOpts repoNN termNN courseNN) noArgs
   where f (Global(
            Course(CourseOpts(
             CourseRemove(CourseRemoveOpts a b c))))) = (a,b,c)
            
prop_courseListSuccess repoNN termNN =
 validOpts [repoNN, termNN] ==>
  testSuccess (repoNN, termNN) f [courseSub, listSub] (termOpts repoNN termNN) noArgs
   where f (Global(
            Course(CourseOpts(
             CourseList(CourseListOpts a b))))) = (a,b)
            
prop_courseTeacherAddSuccess repoNN termNN courseNN ns = let names = validArgs ns in names /= [] ==>
 validOpts [repoNN, termNN, courseNN] ==>
  testSuccess (repoNN, termNN, courseNN, names) f [courseSub, teacherSub, addSub] 
   (courseOpts repoNN termNN courseNN) names
       where f (Global(
                Course(CourseOpts(
                 CourseTeacher(CourseTeacherOpts(
                  CourseTeacherAdd(CourseTeacherAddOpts a b c d))))))) = (a,b,c,d)
              
prop_courseTeacherRemoveSuccess repoNN termNN courseNN ns = let names = validArgs ns in names /= [] ==>
 validOpts [repoNN, termNN, courseNN] ==>
  testSuccess (repoNN, termNN, courseNN, names) f [courseSub, teacherSub, removeSub] 
   (courseOpts repoNN termNN courseNN) names
       where f (Global(
                Course(CourseOpts(
                 CourseTeacher(CourseTeacherOpts(
                  CourseTeacherRemove(CourseTeacherRemoveOpts a b c d))))))) = (a,b,c,d)
              
prop_courseTeacherListSuccess repoNN termNN courseNN =
 validOpts [repoNN, termNN, courseNN] ==>
  testSuccess (repoNN, termNN, courseNN) f [courseSub, teacherSub, listSub] 
   (courseOpts repoNN termNN courseNN) noArgs
       where f (Global(
                Course(CourseOpts(
                 CourseTeacher(CourseTeacherOpts(
                  CourseTeacherList(CourseTeacherListOpts a b c))))))) = (a,b,c)
              

prop_courseCorrectorAddSuccess repoNN termNN courseNN ns = let names = validArgs ns in names /= [] ==>
 validOpts [repoNN, termNN, courseNN] ==>
  testSuccess (repoNN, termNN, courseNN, names) f [courseSub, correctorSub, addSub] 
   (courseOpts repoNN termNN courseNN) names
       where f (Global( 
                Course(CourseOpts(
                 CourseCorrector(CourseCorrectorOpts(
                  CourseCorrectorAdd(CourseCorrectorAddOpts a b c d))))))) = (a,b,c,d)
              
prop_courseCorrectorRemoveSuccess repoNN termNN courseNN ns = let names = validArgs ns in names /= [] ==>
 validOpts [repoNN, termNN, courseNN] ==>
  testSuccess (repoNN, termNN, courseNN, names) f [courseSub, correctorSub, removeSub] 
   (courseOpts repoNN termNN courseNN) names
       where f (Global(
                Course(CourseOpts(
                 CourseCorrector(CourseCorrectorOpts(
                  CourseCorrectorRemove(CourseCorrectorRemoveOpts a b c d))))))) = (a,b,c,d)
              
prop_courseCorrectorListSuccess repoNN termNN courseNN =
 validOpts [repoNN, termNN, courseNN] ==>
  testSuccess (repoNN, termNN, courseNN) f [courseSub, correctorSub, listSub] 
   (courseOpts repoNN termNN courseNN) noArgs
       where f (Global(
                Course(CourseOpts(
                 CourseCorrector(CourseCorrectorOpts(
                  CourseCorrectorList(CourseCorrectorListOpts a b c))))))) = (a,b,c)
                             
-- Group          
prop_groupAddSuccess repoNN termNN courseNN n = let name = noLeadingHyphens n in
 validOpts [repoNN, termNN, courseNN] ==>
  testSuccess (repoNN, termNN, courseNN, name) f [groupSub, addSub] 
   (courseOpts repoNN termNN courseNN) [name]
    where f (Global(
             Group(GroupOpts(
              GroupAdd(GroupAddOpts a b c d))))) = (a,b,c,d)
                
prop_groupRemoveSuccess repoNN termNN courseNN groupNN =
 validOpts [repoNN, termNN, courseNN, groupNN] ==>
  testSuccess (repoNN, termNN, courseNN, groupNN) f [groupSub, removeSub] 
   (groupOpts repoNN termNN courseNN groupNN) noArgs
    where f (Global( 
             Group(GroupOpts(
              GroupRemove(GroupRemoveOpts a b c d))))) = (a,b,c,d)
            
prop_groupListSuccess repoNN termNN courseNN =
 validOpts [repoNN, termNN, courseNN] ==>
  testSuccess (repoNN, termNN, courseNN) f [groupSub, listSub] (courseOpts repoNN termNN courseNN) noArgs
   where f (Global(
            Group(GroupOpts(
             GroupList(GroupListOpts a b c))))) = (a,b,c)
            
prop_groupTeacherAddSuccess repoNN termNN courseNN groupNN ns = let names = validArgs ns in names /= [] ==>
 validOpts [repoNN, termNN, courseNN, groupNN] ==>
  testSuccess (repoNN, termNN, courseNN, groupNN, names) f 
   [groupSub, teacherSub, addSub] (groupOpts repoNN termNN courseNN groupNN) names
       where f (Global(
                Group(GroupOpts(
                 GroupTeacher(GroupTeacherOpts(
                  GroupTeacherAdd(GroupTeacherAddOpts a b c d e))))))) = (a,b,c,d, e)
              
prop_groupTeacherRemoveSuccess repoNN termNN courseNN groupNN ns = let names = validArgs ns in names /= [] ==>
 validOpts [repoNN, termNN, courseNN, groupNN] ==>
  testSuccess (repoNN, termNN, courseNN, groupNN, names) f [groupSub, teacherSub, removeSub] 
   (groupOpts repoNN termNN courseNN groupNN) names
       where f (Global(
                Group(GroupOpts(
                 GroupTeacher(GroupTeacherOpts(
                  GroupTeacherRemove(GroupTeacherRemoveOpts a b c d e))))))) = (a,b,c,d,e)
              
prop_groupTeacherListSuccess repoNN termNN courseNN groupNN =
 validOpts [repoNN, termNN, courseNN, groupNN] ==>
  testSuccess (repoNN, termNN, courseNN, groupNN) f [groupSub, teacherSub, listSub] 
   (groupOpts repoNN termNN courseNN groupNN) noArgs
    where f (Global(
             Group(GroupOpts(
              GroupTeacher(GroupTeacherOpts(
               GroupTeacherList(GroupTeacherListOpts a b c d))))))) = (a,b,c,d)
              
 
prop_groupCorrectorAddSuccess repoNN termNN courseNN groupNN ns = let names = validArgs ns in names /= [] ==>
 validOpts [repoNN, termNN, courseNN, groupNN] ==>
  testSuccess (repoNN, termNN, courseNN, groupNN, names) f [groupSub, correctorSub, addSub] 
   (groupOpts repoNN termNN courseNN groupNN) names
       where f (Global(
                Group(GroupOpts(
                 GroupCorrector(GroupCorrectorOpts(
                  GroupCorrectorAdd(GroupCorrectorAddOpts a b c d e))))))) = (a,b,c,d,e)
              
prop_groupCorrectorRemoveSuccess repoNN termNN courseNN groupNN ns = let names = validArgs ns in names /= [] ==>
 validOpts [repoNN, termNN, courseNN, groupNN] ==>
  testSuccess (repoNN, termNN, courseNN, groupNN, names) f [groupSub, correctorSub, removeSub] 
   (groupOpts repoNN termNN courseNN groupNN) names
       where f (Global(
                Group(GroupOpts(
                 GroupCorrector(GroupCorrectorOpts(
                  GroupCorrectorRemove(GroupCorrectorRemoveOpts a b c d e))))))) = (a,b,c,d, e)
              
prop_groupCorrectorListSuccess repoNN termNN courseNN groupNN =
 validOpts [repoNN, termNN, courseNN, groupNN] ==>
  testSuccess (repoNN, termNN, courseNN, groupNN) f [groupSub, correctorSub, listSub] 
   (groupOpts repoNN termNN courseNN groupNN) noArgs
       where f (Global(
                Group(GroupOpts(
                 GroupCorrector(GroupCorrectorOpts(
                  GroupCorrectorList(GroupCorrectorListOpts a b c d))))))) = (a,b,c,d)
                             
-- Project         
prop_projectAddSuccess repoNN termNN courseNN groupNN start end late n = let ns = validArgs [n] in ns /= [] ==>
 validOpts [repoNN, termNN, courseNN, groupNN, start, end, late] ==> let name = head ns in
  testSuccess (repoNN, termNN, courseNN, groupNN, start, end, late, name) f 
   [projectSub, addSub] (projectAddOpts repoNN termNN courseNN groupNN start end late) [name]
   where f (Global( 
            Project(ProjectOpts( 
             ProjectAdd(ProjectAddOpts a b c d e f' g h))))) = (a,b,c,d,e,f',g,h)
                
prop_projectRemoveSuccess repoNN termNN courseNN groupNN projectNN =
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess (repoNN, termNN, courseNN, groupNN, projectNN) f 
   [projectSub, removeSub] (projectOpts repoNN termNN courseNN groupNN projectNN) noArgs
    where f (Global( 
             Project(ProjectOpts(
              ProjectRemove(ProjectRemoveOpts a b c d e))))) = (a,b,c,d,e)
             
prop_projectListSuccess repoNN termNN courseNN groupNN =
 validOpts [repoNN, termNN, courseNN, groupNN] ==>
  testSuccess (repoNN, termNN, courseNN, groupNN) f 
   [projectSub, listSub] (groupOpts repoNN termNN courseNN groupNN) noArgs
    where f (Global(
             Project(ProjectOpts(
              ProjectList(ProjectListOpts a b c d))))) = (a,b,c,d)
             
prop_projectDateSetSuccess repoNN termNN courseNN groupNN projectNN start end late =
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN, start, end, late] ==>
  testSuccess (repoNN, termNN, courseNN, groupNN, projectNN, start, end, late) f [projectSub, dateSub, setSub] 
  (projectDateOpts repoNN termNN courseNN groupNN projectNN start end late) noArgs
    where f (Global(
             Project(ProjectOpts(
              ProjectDate(ProjectDateOpts(
               ProjectDateSet(ProjectDateSetOpts a b c d e f' g h))))))) = (a,b,c,d,e,f',g,h)
 
prop_projectDateListSuccess repoNN termNN courseNN groupNN projectNN =
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess (repoNN, termNN, courseNN, groupNN, projectNN) f [projectSub, dateSub, listSub] 
   (projectOpts repoNN termNN courseNN groupNN projectNN) noArgs
    where f (Global(
             Project(ProjectOpts(
              ProjectDate(ProjectDateOpts(
               ProjectDateList(ProjectDateListOpts a b c d e))))))) = (a,b,c,d,e)
                