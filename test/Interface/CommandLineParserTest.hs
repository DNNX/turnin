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

{-# ANN module "HLint: ignore Use camelCase" #-}
 
-- Config  
prop_configThresholdSetSuccess cu ch =
 validOpts [cu, ch] ==>
  testSuccess (cu,ch) h [configSub, thresholdSub, setSub] (thresholdOpts cu ch) noArgs
   where h (Global(
            Config(ConfigOpts(
             ConfigThreshold(ConfigThresholdOpts(
              ConfigThresholdSet(ConfigThresholdSetOpts a b))))))) = (a,b)

prop_configThresholdListSuccess =
  testSuccess noArgsToGet h [configSub, thresholdSub, listSub] noOpts noArgs
   where h (Global(
            Config(ConfigOpts(
             ConfigThreshold(ConfigThresholdOpts(
              ConfigThresholdList ConfigThresholdListOpts)))))) = noArgsToGet

prop_configTermDateSetSuccess t1 t2 t3 =
 validOpts [t1, t2, t3] ==>
  testSuccess (t1,t2,t3) h [configSub, termDateSub, setSub] (configTermDateOpts t1 t2 t3) noArgs
   where h (Global(
            Config(ConfigOpts( 
             ConfigTermDate(ConfigTermDateOpts(
              ConfigTermDateSet(ConfigTermDateSetOpts a b c))))))) = (a,b,c)

prop_configTermDateListSuccess =
  testSuccess noArgsToGet h [configSub, termDateSub, listSub] noOpts noArgs
   where h (Global(
            Config(ConfigOpts(
             ConfigTermDate(ConfigTermDateOpts(
              ConfigTermDateList ConfigTermDateListOpts)))))) = noArgsToGet

prop_configProjectDateSetSuccess end late =
 validOpts [end, late] ==>
  testSuccess (end, late) h [configSub, projectDateSub, setSub] (configProjectDateOpts end late) noArgs
   where h (Global(
            Config(ConfigOpts(
             ConfigProjectDate(ConfigProjectDateOpts(
              ConfigProjectDateSet(ConfigProjectDateSetOpts a b))))))) = (a,b)

prop_configProjectDateListSuccess =
  testSuccess noArgsToGet h [configSub, projectDateSub, listSub] noOpts noArgs
   where h (Global(
            Config(ConfigOpts(
             ConfigProjectDate(ConfigProjectDateOpts(
              ConfigProjectDateList ConfigProjectDateListOpts)))))) = noArgsToGet

prop_configAcceptExecSetSuccess v = let val = ["yYnN" !! (v `mod` 4)] in 
 testSuccess val h [configSub, acceptExecSub, setSub] noOpts [val]
  where h (Global(
           Config(ConfigOpts(
            ConfigAcceptExec(ConfigAcceptExecOpts(
             ConfigAcceptExecSet(ConfigAcceptExecSetOpts a))))))) = a

prop_configAcceptExecListSuccess =
  testSuccess noArgsToGet h [configSub, acceptExecSub, listSub] noOpts noArgs
   where h (Global(
            Config(ConfigOpts(
             ConfigAcceptExec(ConfigAcceptExecOpts(
              ConfigAcceptExecList ConfigAcceptExecListOpts)))))) = noArgsToGet

prop_configTimeLimitSetSuccess v = let val = show $ abs (v :: Double) in 
 testSuccess val h [configSub, timeLimitSub, setSub] noOpts [val]
  where h (Global(
           Config(ConfigOpts(
            ConfigTimeLimit(ConfigTimeLimitOpts(
             ConfigTimeLimitSet(ConfigTimeLimitSetOpts a))))))) = a

prop_configTimeLimitListSuccess =
  testSuccess noArgsToGet h [configSub, timeLimitSub, listSub] noOpts noArgs
   where h (Global(
            Config(ConfigOpts(
             ConfigTimeLimit(ConfigTimeLimitOpts(
              ConfigTimeLimitList ConfigTimeLimitListOpts)))))) = noArgsToGet

prop_configSpaceLimitSetSuccess v = let val = show $ abs (v :: Integer) in
 testSuccess val h [configSub, spaceLimitSub, setSub] noOpts [val]
  where h (Global(
           Config(ConfigOpts(
            ConfigSpaceLimit(ConfigSpaceLimitOpts(
             ConfigSpaceLimitSet(ConfigSpaceLimitSetOpts a))))))) = a

prop_configSpaceLimitListSuccess =
  testSuccess noArgsToGet h [configSub, spaceLimitSub, listSub] noOpts noArgs
   where h (Global(
            Config(ConfigOpts(
             ConfigSpaceLimit(ConfigSpaceLimitOpts(
              ConfigSpaceLimitList ConfigSpaceLimitListOpts)))))) = noArgsToGet

prop_configAdminGroupsSetSuccess gs = let groups = validArgs gs in groups /= [] ==>
  testSuccess groups h [configSub, adminGroupsSub, setSub] noOpts groups
   where h (Global(
            Config(ConfigOpts( 
             ConfigAdminGroups(ConfigAdminGroupsOpts(
              ConfigAdminGroupsSet(ConfigAdminGroupsSetOpts a))))))) = a

prop_configAdminGroupsListSuccess =
  testSuccess noArgsToGet h [configSub, adminGroupsSub, listSub] noOpts noArgs
   where h (Global(
            Config(ConfigOpts(
             ConfigAdminGroups(ConfigAdminGroupsOpts(
              ConfigAdminGroupsList ConfigAdminGroupsListOpts)))))) = noArgsToGet

prop_configTeacherGroupsSetSuccess gs = let groups = validArgs gs in -- List can be empty
 testSuccess groups h [configSub, teacherGroupsSub, setSub] noOpts groups
  where h (Global(
           Config(ConfigOpts(
            ConfigTeacherGroups(ConfigTeacherGroupsOpts(
             ConfigTeacherGroupsSet(ConfigTeacherGroupsSetOpts a))))))) = a

prop_configTeacherGroupsListSuccess =
  testSuccess noArgsToGet h [configSub, teacherGroupsSub, listSub] noOpts noArgs
   where h (Global(
            Config(ConfigOpts(
             ConfigTeacherGroups(ConfigTeacherGroupsOpts(
              ConfigTeacherGroupsList ConfigTeacherGroupsListOpts)))))) = noArgsToGet

prop_configCorrectorIsSuccess n = let name = noLeadingHyphens n in
 testSuccess name h [configSub, correctorSub, isSub] noOpts [name]
  where h (Global(
           Config(ConfigOpts(
            ConfigCorrector(ConfigCorrectorOpts(
             ConfigCorrectorIs(ConfigCorrectorIsOpts a))))))) = a

prop_configCorrectorAddSuccess n = let name = noLeadingHyphens n in
 testSuccess name h [configSub, correctorSub, addSub] noOpts [name]
  where h (Global(
           Config(ConfigOpts(
            ConfigCorrector(ConfigCorrectorOpts(
             ConfigCorrectorAdd(ConfigCorrectorAddOpts a))))))) = a

prop_configCorrectorRemoveSuccess n = let name = noLeadingHyphens n in
 testSuccess name h [configSub, correctorSub, removeSub] noOpts [name]
  where h (Global(
           Config(ConfigOpts(
            ConfigCorrector(ConfigCorrectorOpts(
             ConfigCorrectorRemove(ConfigCorrectorRemoveOpts a))))))) = a
                 
-- Repo                
prop_repoAddSuccess n = let name = noLeadingHyphens n in
 testSuccess name h [repoSub, addSub] noOpts [name]
  where h (Global(
           Repo(RepoOpts(
            RepoAdd(RepoAddOpts a))))) = a
                
prop_repoRemoveSuccess repoNN =
 validOpts [repoNN] ==>
  testSuccess repoNN h [repoSub, removeSub] (repoOpts repoNN) noArgs
   where h (Global(
            Repo(RepoOpts(
             RepoRemove(RepoRemoveOpts a))))) = a
            
prop_repoListSuccess =
 testSuccess noArgsToGet h [repoSub, listSub] noOpts noArgs
  where h (Global(
           Repo(RepoOpts(
            RepoList RepoListOpts)))) = noArgsToGet
 
-- Term         
prop_termAddSuccess repoNN n s e = let args = validArgs [n, s, e] in length args == 3 ==>
 let [name, start, end] = args in
  validOpts [repoNN] ==>
   testSuccess (repoNN, name, start, end) h [termSub, addSub] (repoOpts repoNN) args
    where h (Global(
             Term(TermOpts( 
              TermAdd(TermAddOpts a b c d))))) = (a,b,c,d)
                
prop_termRemoveSuccess repoNN termNN =
 validOpts [repoNN, termNN] ==>
  testSuccess (repoNN,termNN) h [termSub, removeSub] (termOpts repoNN termNN) noArgs
   where h (Global(
            Term(TermOpts(
             TermRemove(TermRemoveOpts a b))))) = (a,b)
             
prop_termListSuccess repoNN =
 validOpts [repoNN] ==>
  testSuccess repoNN h [termSub, listSub] (repoOpts repoNN) noArgs
   where h (Global(
            Term(TermOpts(
             TermList(TermListOpts a))))) = a
             
prop_termDateSetSuccess repoNN termNN start end =
 validOpts [repoNN, termNN, start, end] ==>
  testSuccess (repoNN, termNN, start, end) h [termSub, dateSub, setSub] (termDateOpts repoNN termNN start end) noArgs
   where h (Global(
            Term(TermOpts(
             TermDate(TermDateOpts(
              TermDateSet(TermDateSetOpts a b c d))))))) = (a,b,c,d)
 
prop_termDateListSuccess repoNN termNN =
 validOpts [repoNN, termNN] ==>
  testSuccess (repoNN, termNN) h [termSub, dateSub, listSub] (termOpts repoNN termNN) noArgs
   where h (Global(
            Term(TermOpts(
             TermDate(TermDateOpts(
              TermDateList(TermDateListOpts a b))))))) = (a,b)
                      
-- Course            
prop_courseAddSuccess repoNN termNN n = let name = noLeadingHyphens n in
 validOpts [repoNN, termNN] ==>
  testSuccess (repoNN, termNN, name) h [courseSub, addSub] (termOpts repoNN termNN) [name]
   where h (Global(
            Course(CourseOpts(
             CourseAdd(CourseAddOpts a b c))))) = (a,b,c)
                
prop_courseRemoveSuccess repoNN termNN courseNN =
 validOpts [repoNN, termNN, courseNN] ==>
  testSuccess (repoNN, termNN, courseNN) h [courseSub, removeSub] (courseOpts repoNN termNN courseNN) noArgs
   where h (Global(
            Course(CourseOpts(
             CourseRemove(CourseRemoveOpts a b c))))) = (a,b,c)
            
prop_courseListSuccess repoNN termNN =
 validOpts [repoNN, termNN] ==>
  testSuccess (repoNN, termNN) h [courseSub, listSub] (termOpts repoNN termNN) noArgs
   where h (Global(
            Course(CourseOpts(
             CourseList(CourseListOpts a b))))) = (a,b)
            
prop_courseTeacherAddSuccess repoNN termNN courseNN ns = let names = validArgs ns in names /= [] ==>
 validOpts [repoNN, termNN, courseNN] ==>
  testSuccess (repoNN, termNN, courseNN, names) h [courseSub, teacherSub, addSub] 
   (courseOpts repoNN termNN courseNN) names
       where h (Global(
                Course(CourseOpts(
                 CourseTeacher(CourseTeacherOpts(
                  CourseTeacherAdd(CourseTeacherAddOpts a b c d))))))) = (a,b,c,d)
              
prop_courseTeacherRemoveSuccess repoNN termNN courseNN ns = let names = validArgs ns in names /= [] ==>
 validOpts [repoNN, termNN, courseNN] ==>
  testSuccess (repoNN, termNN, courseNN, names) h [courseSub, teacherSub, removeSub] 
   (courseOpts repoNN termNN courseNN) names
       where h (Global(
                Course(CourseOpts(
                 CourseTeacher(CourseTeacherOpts(
                  CourseTeacherRemove(CourseTeacherRemoveOpts a b c d))))))) = (a,b,c,d)
              
prop_courseTeacherListSuccess repoNN termNN courseNN =
 validOpts [repoNN, termNN, courseNN] ==>
  testSuccess (repoNN, termNN, courseNN) h [courseSub, teacherSub, listSub] 
   (courseOpts repoNN termNN courseNN) noArgs
       where h (Global(
                Course(CourseOpts(
                 CourseTeacher(CourseTeacherOpts(
                  CourseTeacherList(CourseTeacherListOpts a b c))))))) = (a,b,c)
              

prop_courseCorrectorAddSuccess repoNN termNN courseNN ns = let names = validArgs ns in names /= [] ==>
 validOpts [repoNN, termNN, courseNN] ==>
  testSuccess (repoNN, termNN, courseNN, names) h [courseSub, correctorSub, addSub] 
   (courseOpts repoNN termNN courseNN) names
       where h (Global( 
                Course(CourseOpts(
                 CourseCorrector(CourseCorrectorOpts(
                  CourseCorrectorAdd(CourseCorrectorAddOpts a b c d))))))) = (a,b,c,d)
              
prop_courseCorrectorRemoveSuccess repoNN termNN courseNN ns = let names = validArgs ns in names /= [] ==>
 validOpts [repoNN, termNN, courseNN] ==>
  testSuccess (repoNN, termNN, courseNN, names) h [courseSub, correctorSub, removeSub] 
   (courseOpts repoNN termNN courseNN) names
       where h (Global(
                Course(CourseOpts(
                 CourseCorrector(CourseCorrectorOpts(
                  CourseCorrectorRemove(CourseCorrectorRemoveOpts a b c d))))))) = (a,b,c,d)
              
prop_courseCorrectorListSuccess repoNN termNN courseNN =
 validOpts [repoNN, termNN, courseNN] ==>
  testSuccess (repoNN, termNN, courseNN) h [courseSub, correctorSub, listSub] 
   (courseOpts repoNN termNN courseNN) noArgs
       where h (Global(
                Course(CourseOpts(
                 CourseCorrector(CourseCorrectorOpts(
                  CourseCorrectorList(CourseCorrectorListOpts a b c))))))) = (a,b,c)
                             
-- Group          
prop_groupAddSuccess repoNN termNN courseNN n = let name = noLeadingHyphens n in
 validOpts [repoNN, termNN, courseNN] ==>
  testSuccess (repoNN, termNN, courseNN, name) h [groupSub, addSub] 
   (courseOpts repoNN termNN courseNN) [name]
    where h (Global(
             Group(GroupOpts(
              GroupAdd(GroupAddOpts a b c d))))) = (a,b,c,d)
                
prop_groupRemoveSuccess repoNN termNN courseNN groupNN =
 validOpts [repoNN, termNN, courseNN, groupNN] ==>
  testSuccess (repoNN, termNN, courseNN, groupNN) h [groupSub, removeSub] 
   (groupOpts repoNN termNN courseNN groupNN) noArgs
    where h (Global( 
             Group(GroupOpts(
              GroupRemove(GroupRemoveOpts a b c d))))) = (a,b,c,d)
            
prop_groupListSuccess repoNN termNN courseNN =
 validOpts [repoNN, termNN, courseNN] ==>
  testSuccess (repoNN, termNN, courseNN) h [groupSub, listSub] (courseOpts repoNN termNN courseNN) noArgs
   where h (Global(
            Group(GroupOpts(
             GroupList(GroupListOpts a b c))))) = (a,b,c)
            
prop_groupTeacherAddSuccess repoNN termNN courseNN groupNN ns = let names = validArgs ns in names /= [] ==>
 validOpts [repoNN, termNN, courseNN, groupNN] ==>
  testSuccess (repoNN, termNN, courseNN, groupNN, names) h 
   [groupSub, teacherSub, addSub] (groupOpts repoNN termNN courseNN groupNN) names
       where h (Global(
                Group(GroupOpts(
                 GroupTeacher(GroupTeacherOpts(
                  GroupTeacherAdd(GroupTeacherAddOpts a b c d e))))))) = (a,b,c,d, e)
              
prop_groupTeacherRemoveSuccess repoNN termNN courseNN groupNN ns = let names = validArgs ns in names /= [] ==>
 validOpts [repoNN, termNN, courseNN, groupNN] ==>
  testSuccess (repoNN, termNN, courseNN, groupNN, names) h [groupSub, teacherSub, removeSub] 
   (groupOpts repoNN termNN courseNN groupNN) names
       where h (Global(
                Group(GroupOpts(
                 GroupTeacher(GroupTeacherOpts(
                  GroupTeacherRemove(GroupTeacherRemoveOpts a b c d e))))))) = (a,b,c,d,e)
              
prop_groupTeacherListSuccess repoNN termNN courseNN groupNN =
 validOpts [repoNN, termNN, courseNN, groupNN] ==>
  testSuccess (repoNN, termNN, courseNN, groupNN) h [groupSub, teacherSub, listSub] 
   (groupOpts repoNN termNN courseNN groupNN) noArgs
    where h (Global(
             Group(GroupOpts(
              GroupTeacher(GroupTeacherOpts(
               GroupTeacherList(GroupTeacherListOpts a b c d))))))) = (a,b,c,d)
              
 
prop_groupCorrectorAddSuccess repoNN termNN courseNN groupNN ns = let names = validArgs ns in names /= [] ==>
 validOpts [repoNN, termNN, courseNN, groupNN] ==>
  testSuccess (repoNN, termNN, courseNN, groupNN, names) h [groupSub, correctorSub, addSub] 
   (groupOpts repoNN termNN courseNN groupNN) names
       where h (Global(
                Group(GroupOpts(
                 GroupCorrector(GroupCorrectorOpts(
                  GroupCorrectorAdd(GroupCorrectorAddOpts a b c d e))))))) = (a,b,c,d,e)
              
prop_groupCorrectorRemoveSuccess repoNN termNN courseNN groupNN ns = let names = validArgs ns in names /= [] ==>
 validOpts [repoNN, termNN, courseNN, groupNN] ==>
  testSuccess (repoNN, termNN, courseNN, groupNN, names) h [groupSub, correctorSub, removeSub] 
   (groupOpts repoNN termNN courseNN groupNN) names
       where h (Global(
                Group(GroupOpts(
                 GroupCorrector(GroupCorrectorOpts(
                  GroupCorrectorRemove(GroupCorrectorRemoveOpts a b c d e))))))) = (a,b,c,d, e)
              
prop_groupCorrectorListSuccess repoNN termNN courseNN groupNN =
 validOpts [repoNN, termNN, courseNN, groupNN] ==>
  testSuccess (repoNN, termNN, courseNN, groupNN) h [groupSub, correctorSub, listSub] 
   (groupOpts repoNN termNN courseNN groupNN) noArgs
       where h (Global(
                Group(GroupOpts(
                 GroupCorrector(GroupCorrectorOpts(
                  GroupCorrectorList(GroupCorrectorListOpts a b c d))))))) = (a,b,c,d)
                            
                            
              