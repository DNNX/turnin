{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Interface.CommandLineParserTest where
import Test.Framework
import Interface.CommandLineParserTestUtils

import Interface.Lexicon
import Interface.CommandLineParser
import Interface.CommandLineParser.Config

{-# ANN module "HLint: ignore Use camelCase" #-}

-- Config
prop_configThresholdSetSuccess cu ch =
 validArgs [cu, ch] ==>
  testSuccess (cu,ch) h [configSub, thresholdSub, setSub] ([configThresholdSetCurrentOpt, configThresholdSetChooseOpt], [cu, ch]) noVariadics
   where h (Global(
            Config(ConfigOpts(
             ConfigThreshold(ConfigThresholdOpts(
              ConfigThresholdSet(ConfigThresholdSetOpts a b))))))) = (a, b)

prop_configThresholdListSuccess =
  testSuccess noOptsToGet h [configSub, thresholdSub, listSub] noOpts noVariadics
   where h (Global(
            Config(ConfigOpts(
             ConfigThreshold(ConfigThresholdOpts(
              ConfigThresholdList ConfigThresholdListOpts)))))) = noOptsToGet

prop_configTermDateSetSuccess t1 t2 t3 =
 validArgs [t1, t2, t3] ==>
  testSuccess (t1,t2,t3) h [configSub, termDateSub, setSub] ([configTermDateSetTerm1Opt, configTermDateSetTerm2Opt, configTermDateSetTerm3Opt],
                                                             [t1,t2,t3]) noVariadics
   where h (Global(
            Config(ConfigOpts(
             ConfigTermDate(ConfigTermDateOpts(
              ConfigTermDateSet(ConfigTermDateSetOpts a b c))))))) = (a, b, c)

prop_configTermDateListSuccess =
  testSuccess noOptsToGet h [configSub, termDateSub, listSub] noOpts noVariadics
   where h (Global(
            Config(ConfigOpts(
             ConfigTermDate(ConfigTermDateOpts(
              ConfigTermDateList ConfigTermDateListOpts)))))) = noOptsToGet

prop_configProjectDateSetSuccess end late =
 validArgs [end, late] ==>
  testSuccess (end, late) h [configSub, projectDateSub, setSub] ([configProjectDateSetEndOpt, configProjectDateSetLateOpt], [end, late]) noVariadics
   where h (Global(
            Config(ConfigOpts(
             ConfigProjectDate(ConfigProjectDateOpts(
              ConfigProjectDateSet(ConfigProjectDateSetOpts a b))))))) = (a, b)

prop_configProjectDateListSuccess =
  testSuccess noOptsToGet h [configSub, projectDateSub, listSub] noOpts noVariadics
   where h (Global(
            Config(ConfigOpts(
             ConfigProjectDate(ConfigProjectDateOpts(
              ConfigProjectDateList ConfigProjectDateListOpts)))))) = noOptsToGet

prop_configAcceptExecSetSuccess v =
 let val = ["yYnN" !! (v `mod` 4)]
 in testSuccess val h [configSub, acceptExecSub, setSub, val] noOpts noVariadics
     where h (Global(
              Config(ConfigOpts(
               ConfigAcceptExec(ConfigAcceptExecOpts(
                ConfigAcceptExecSet(ConfigAcceptExecSetOpts a))))))) = a

prop_configAcceptExecListSuccess =
  testSuccess noOptsToGet h [configSub, acceptExecSub, listSub] noOpts noVariadics
   where h (Global(
            Config(ConfigOpts(
             ConfigAcceptExec(ConfigAcceptExecOpts(
              ConfigAcceptExecList ConfigAcceptExecListOpts)))))) = noOptsToGet

prop_configTimeLimitSetSuccess v =
 let val = show $ abs (v :: Double)
 in  testSuccess val h [configSub, timeLimitSub, setSub, val] noOpts noVariadics
      where h (Global(
               Config(ConfigOpts(
                ConfigTimeLimit(ConfigTimeLimitOpts(
                 ConfigTimeLimitSet(ConfigTimeLimitSetOpts a))))))) = a

prop_configTimeLimitListSuccess =
  testSuccess noOptsToGet h [configSub, timeLimitSub, listSub] noOpts noVariadics
   where h (Global(
            Config(ConfigOpts(
             ConfigTimeLimit(ConfigTimeLimitOpts(
              ConfigTimeLimitList ConfigTimeLimitListOpts)))))) = noOptsToGet

prop_configSpaceLimitSetSuccess v =
 let val = show $ abs (v :: Integer)
 in  testSuccess val h [configSub, spaceLimitSub, setSub, val] noOpts noVariadics
      where h (Global(
               Config(ConfigOpts(
                ConfigSpaceLimit(ConfigSpaceLimitOpts(
                 ConfigSpaceLimitSet(ConfigSpaceLimitSetOpts a))))))) = a

prop_configSpaceLimitListSuccess =
  testSuccess noOptsToGet h [configSub, spaceLimitSub, listSub] noOpts noVariadics
   where h (Global(
            Config(ConfigOpts(
             ConfigSpaceLimit(ConfigSpaceLimitOpts(
              ConfigSpaceLimitList ConfigSpaceLimitListOpts)))))) = noOptsToGet

prop_configAdminGroupsSetSuccess gs =
 gs /= [] ==>
  let groups = map noLeadingHyphens gs
  in  testSuccess groups h [configSub, adminGroupsSub, setSub] noOpts groups
       where h (Global(
                Config(ConfigOpts(
                 ConfigAdminGroups(ConfigAdminGroupsOpts(
                  ConfigAdminGroupsSet(ConfigAdminGroupsSetOpts a))))))) = a

prop_configAdminGroupsListSuccess =
  testSuccess noOptsToGet h [configSub, adminGroupsSub, listSub] noOpts noVariadics
   where h (Global(
            Config(ConfigOpts(
             ConfigAdminGroups(ConfigAdminGroupsOpts(
              ConfigAdminGroupsList ConfigAdminGroupsListOpts)))))) = noOptsToGet

prop_configTeacherGroupsSetSuccess gs =
 let groups = map noLeadingHyphens gs
 in  testSuccess groups h [configSub, teacherGroupsSub, setSub] noOpts groups
      where h (Global(
               Config(ConfigOpts(
                ConfigTeacherGroups(ConfigTeacherGroupsOpts(
                 ConfigTeacherGroupsSet(ConfigTeacherGroupsSetOpts a))))))) = a

prop_configTeacherGroupsListSuccess =
  testSuccess noOptsToGet h [configSub, teacherGroupsSub, listSub] noOpts noVariadics
   where h (Global(
            Config(ConfigOpts(
             ConfigTeacherGroups(ConfigTeacherGroupsOpts(
              ConfigTeacherGroupsList ConfigTeacherGroupsListOpts)))))) = noOptsToGet

prop_configCorrectorIsSuccess n =
 let name = noLeadingHyphens n
 in  testSuccess name h [configSub, correctorSub, isSub, name] noOpts noVariadics
      where h (Global(
               Config(ConfigOpts(
                ConfigCorrector(ConfigCorrectorOpts(
                 ConfigCorrectorIs(ConfigCorrectorIsOpts a))))))) = a

prop_configCorrectorAddSuccess n =
 let name = noLeadingHyphens n
 in  testSuccess name h [configSub, correctorSub, addSub, name] noOpts noVariadics
      where h (Global(
               Config(ConfigOpts(
                ConfigCorrector(ConfigCorrectorOpts(
                 ConfigCorrectorAdd(ConfigCorrectorAddOpts a))))))) = a

prop_configCorrectorRemoveSuccess n =
 let name = noLeadingHyphens n
 in  testSuccess name h [configSub, correctorSub, removeSub, name] noOpts noVariadics
      where h (Global(
               Config(ConfigOpts(
                ConfigCorrector(ConfigCorrectorOpts(
                 ConfigCorrectorRemove(ConfigCorrectorRemoveOpts a))))))) = a



