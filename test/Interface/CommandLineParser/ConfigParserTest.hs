{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Interface.CommandLineParser.ConfigParserTest where
import Test.Framework
import Interface.CommandLineParser.ParserTestUtils

import Interface.Lexicon
import Interface.CommandLineParser
import Interface.CommandLineParser.Config

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
