{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Interface.CommandLineParser.ConfigTest where

import Test.Framework
import Interface.CommandLineParser.ParserTestUtils

import Interface.Lexicon
import Interface.CommandLineParser
import Interface.CommandLineParser.Config

{-# ANN module "HLint: ignore Use camelCase" #-}

-- Config
prop_configThresholdSetSuccess cu ch =
 validOpts [cu, ch] ==>
  testSuccess 1 (cu,ch) x [configSub, thresholdSub, setSub] (configThresholdOpts cu ch) noArgs
   where x (Global(
            Config(ConfigOpts(
             ConfigThreshold(ConfigThresholdOpts(
              ConfigThresholdSet(ConfigThresholdSetOpts a b))))))) = (a,b)

prop_configThresholdListSuccess =
  testSuccess 2 noArgsToGet x [configSub, thresholdSub, listSub] noOpts noArgs
   where x (Global(
            Config(ConfigOpts(
             ConfigThreshold(ConfigThresholdOpts(
              ConfigThresholdList ConfigThresholdListOpts)))))) = noArgsToGet

prop_configTermDateSetSuccess t1 t2 t3 =
 validOpts [t1, t2, t3] ==>
  testSuccess 1 (t1,t2,t3) x [configSub, termDateSub, setSub] (configTermDateOpts t1 t2 t3) noArgs
   where x (Global( 
            Config(ConfigOpts(
             ConfigTermDate(ConfigTermDateOpts(
              ConfigTermDateSet(ConfigTermDateSetOpts a b c))))))) = (a,b,c)

prop_configTermDateListSuccess =
  testSuccess 2 noArgsToGet x [configSub, termDateSub, listSub] noOpts noArgs
   where x (Global(
            Config(ConfigOpts(
             ConfigTermDate(ConfigTermDateOpts(
              ConfigTermDateList ConfigTermDateListOpts)))))) = noArgsToGet

prop_configProjectDateSetSuccess end late =
 validOpts [end, late] ==>
  testSuccess 1 (end, late) x [configSub, projectDateSub, setSub] (configProjectDateOpts end late) noArgs
   where x (Global(
            Config(ConfigOpts(
             ConfigProjectDate(ConfigProjectDateOpts(
              ConfigProjectDateSet(ConfigProjectDateSetOpts a b))))))) = (a,b)

prop_configProjectDateListSuccess =
  testSuccess 2 noArgsToGet x [configSub, projectDateSub, listSub] noOpts noArgs
   where x (Global(
            Config(ConfigOpts(
             ConfigProjectDate(ConfigProjectDateOpts(
              ConfigProjectDateList ConfigProjectDateListOpts)))))) = noArgsToGet

prop_configAcceptExecSetSuccess v = let val = noLeadingHyphens v in
 testSuccess 1 val x [configSub, acceptExecSub, setSub] noOpts [val]
  where x (Global(
           Config(ConfigOpts(
            ConfigAcceptExec(ConfigAcceptExecOpts(
             ConfigAcceptExecSet(ConfigAcceptExecSetOpts a))))))) = a

prop_configAcceptExecListSuccess =
  testSuccess 2 noArgsToGet x [configSub, acceptExecSub, listSub] noOpts noArgs
   where x (Global(
            Config(ConfigOpts(
             ConfigAcceptExec(ConfigAcceptExecOpts(
              ConfigAcceptExecList ConfigAcceptExecListOpts)))))) = noArgsToGet

prop_configTimeLimitSetSuccess v = let val = noLeadingHyphens v in
 testSuccess 1 val x [configSub, timeLimitSub, setSub] noOpts [val]
  where x (Global(
           Config(ConfigOpts(
            ConfigTimeLimit(ConfigTimeLimitOpts(
             ConfigTimeLimitSet(ConfigTimeLimitSetOpts a))))))) = a

prop_configTimeLimitListSuccess =
  testSuccess 2 noArgsToGet x [configSub, timeLimitSub, listSub] noOpts noArgs
   where x (Global(
            Config(ConfigOpts(
             ConfigTimeLimit(ConfigTimeLimitOpts(
              ConfigTimeLimitList ConfigTimeLimitListOpts)))))) = noArgsToGet

prop_configSpaceLimitSetSuccess v = let val = noLeadingHyphens v in
 testSuccess 1 val x [configSub, spaceLimitSub, setSub] noOpts [val]
  where x (Global(
           Config(ConfigOpts(
            ConfigSpaceLimit(ConfigSpaceLimitOpts(
             ConfigSpaceLimitSet(ConfigSpaceLimitSetOpts a))))))) = a

prop_configSpaceLimitListSuccess =
  testSuccess 2 noArgsToGet x [configSub, spaceLimitSub, listSub] noOpts noArgs
   where x (Global(
            Config(ConfigOpts(
             ConfigSpaceLimit(ConfigSpaceLimitOpts(
              ConfigSpaceLimitList ConfigSpaceLimitListOpts)))))) = noArgsToGet

prop_configAdminGroupsSetSuccess gs = let groups = validArgs gs in groups /= [] ==>
  testSuccess 1 groups x [configSub, adminGroupsSub, setSub] noOpts groups
   where x (Global(
            Config(ConfigOpts(
             ConfigAdminGroups(ConfigAdminGroupsOpts(
              ConfigAdminGroupsSet(ConfigAdminGroupsSetOpts a))))))) = a

prop_configAdminGroupsListSuccess =
  testSuccess 2 noArgsToGet x [configSub, adminGroupsSub, listSub] noOpts noArgs
   where x (Global(
            Config(ConfigOpts(
             ConfigAdminGroups(ConfigAdminGroupsOpts(
              ConfigAdminGroupsList ConfigAdminGroupsListOpts)))))) = noArgsToGet

prop_configTeacherGroupsSetSuccess gs = let groups = validArgs gs in -- List can be empty
 testSuccess 1 groups x [configSub, teacherGroupsSub, setSub] noOpts groups
  where x (Global(
           Config(ConfigOpts(
            ConfigTeacherGroups(ConfigTeacherGroupsOpts(
             ConfigTeacherGroupsSet(ConfigTeacherGroupsSetOpts a))))))) = a

prop_configTeacherGroupsListSuccess =
  testSuccess 2 noArgsToGet x [configSub, teacherGroupsSub, listSub] noOpts noArgs
   where x (Global(
            Config(ConfigOpts(
             ConfigTeacherGroups(ConfigTeacherGroupsOpts(
              ConfigTeacherGroupsList ConfigTeacherGroupsListOpts)))))) = noArgsToGet
 
prop_configCorrectorIsSuccess n = let name = noLeadingHyphens n in
 testSuccess 2 name x [configSub, correctorSub, isSub] noOpts [name]
  where x (Global(
           Config(ConfigOpts(
            ConfigCorrector(ConfigCorrectorOpts(
             ConfigCorrectorIs(ConfigCorrectorIsOpts a))))))) = a

prop_configCorrectorAddSuccess n = let name = noLeadingHyphens n in
 testSuccess 2 name x [configSub, correctorSub, addSub] noOpts [name]
  where x (Global(
           Config(ConfigOpts(
            ConfigCorrector(ConfigCorrectorOpts(
             ConfigCorrectorAdd(ConfigCorrectorAddOpts a))))))) = a

prop_configCorrectorRemoveSuccess n = let name = noLeadingHyphens n in
 testSuccess 2 name x [configSub, correctorSub, removeSub] noOpts [name]
  where x (Global(
           Config(ConfigOpts(
            ConfigCorrector(ConfigCorrectorOpts(
             ConfigCorrectorRemove(ConfigCorrectorRemoveOpts a))))))) = a
