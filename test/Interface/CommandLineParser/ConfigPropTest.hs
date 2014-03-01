{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Interface.CommandLineParser.ConfigPropTest where

import Test.Framework
import Interface.CommandLineParser.ParserTestUtils

import Interface.Lexicon
import Interface.CommandLineParser
import Interface.CommandLineParser.Config

{-# ANN module "HLint: ignore Use camelCase" #-}

prop_configThresholdSetSuccess cu ch = validOpts [cu, ch] ==> testSuccess 1 (cu,ch) configThresholdSetF [configSub, thresholdSub, setSub] (configThresholdOpts cu ch) noArgs
prop_configThresholdListSuccess = testSuccess 2 noArgsToGet configThresholdListF [configSub, thresholdSub, listSub] noOpts noArgs
prop_configTermDateSetSuccess t1 t2 t3 = validOpts [t1, t2, t3] ==> testSuccess 1 (t1,t2,t3) configTermDateSetF [configSub, termDateSub, setSub] (configTermDateOpts t1 t2 t3) noArgs
prop_configTermDateListSuccess = testSuccess 2 noArgsToGet configTermDateListF [configSub, termDateSub, listSub] noOpts noArgs
prop_configProjectDateSetSuccess end late = validOpts [end, late] ==> testSuccess 1 (end, late) configProjectDateSetF [configSub, projectDateSub, setSub] (configProjectDateOpts end late) noArgs
prop_configProjectDateListSuccess = testSuccess 2 noArgsToGet configProjectDateListF [configSub, projectDateSub, listSub] noOpts noArgs
prop_configAcceptExecSetSuccess v = let val = noLeadingHyphens v in testSuccess 1 val configAcceptExecSetF [configSub, acceptExecSub, setSub] noOpts [val]
prop_configAcceptExecListSuccess = testSuccess 2 noArgsToGet configAcceptExecListF [configSub, acceptExecSub, listSub] noOpts noArgs
prop_configTimeLimitSetSuccess v = let val = noLeadingHyphens v in testSuccess 1 val configTimeLimitSetF [configSub, timeLimitSub, setSub] noOpts [val]
prop_configTimeLimitListSuccess = testSuccess 2 noArgsToGet configTimeLimitListF [configSub, timeLimitSub, listSub] noOpts noArgs
prop_configSpaceLimitSetSuccess v = let val = noLeadingHyphens v in testSuccess 1 val configSpaceLimitSetF [configSub, spaceLimitSub, setSub] noOpts [val]
prop_configSpaceLimitListSuccess = testSuccess 2 noArgsToGet configSpaceLimitListF [configSub, spaceLimitSub, listSub] noOpts noArgs
prop_configAdminGroupsSetSuccess gs = let groups = validArgs gs in groups /= [] ==> testSuccess 1 groups configAdminGroupsSetF [configSub, adminGroupsSub, setSub] noOpts groups
prop_configAdminGroupsListSuccess = testSuccess 2 noArgsToGet configAdminGroupsListF [configSub, adminGroupsSub, listSub] noOpts noArgs
prop_configTeacherGroupsSetSuccess gs = let groups = validArgs gs in {- List can be empty -} testSuccess 1 groups configTeacherGroupsSetF [configSub, teacherGroupsSub, setSub] noOpts groups
prop_configTeacherGroupsListSuccess = testSuccess 2 noArgsToGet configTeacherGroupsListF [configSub, teacherGroupsSub, listSub] noOpts noArgs
prop_configCorrectorIsSuccess n = let name = noLeadingHyphens n in testSuccess 2 name configCorrectorIsF [configSub, correctorSub, isSub] noOpts [name]
prop_configCorrectorAddSuccess n = let name = noLeadingHyphens n in testSuccess 2 name configCorrectorAddF [configSub, correctorSub, addSub] noOpts [name]
prop_configCorrectorRemoveSuccess n = let name = noLeadingHyphens n in  testSuccess 2 name configCorrectorRemoveF [configSub, correctorSub, removeSub] noOpts [name]
             
configThresholdSetF (Global(Config(ConfigOpts(ConfigThreshold(ConfigThresholdOpts(ConfigThresholdSet(ConfigThresholdSetOpts a b))))))) = (a,b)
configThresholdListF (Global(Config(ConfigOpts(ConfigThreshold(ConfigThresholdOpts(ConfigThresholdList ConfigThresholdListOpts)))))) = noArgsToGet
configTermDateSetF (Global(Config(ConfigOpts(ConfigTermDate(ConfigTermDateOpts(ConfigTermDateSet(ConfigTermDateSetOpts a b c))))))) = (a,b,c)
configTermDateListF (Global(Config(ConfigOpts(ConfigTermDate(ConfigTermDateOpts(ConfigTermDateList ConfigTermDateListOpts)))))) = noArgsToGet
configProjectDateSetF (Global(Config(ConfigOpts(ConfigProjectDate(ConfigProjectDateOpts(ConfigProjectDateSet(ConfigProjectDateSetOpts a b))))))) = (a,b)
configProjectDateListF (Global(Config(ConfigOpts(ConfigProjectDate(ConfigProjectDateOpts(ConfigProjectDateList ConfigProjectDateListOpts)))))) = noArgsToGet
configAcceptExecSetF (Global(Config(ConfigOpts(ConfigAcceptExec(ConfigAcceptExecOpts(ConfigAcceptExecSet(ConfigAcceptExecSetOpts a))))))) = a
configAcceptExecListF (Global(Config(ConfigOpts(ConfigAcceptExec(ConfigAcceptExecOpts(ConfigAcceptExecList ConfigAcceptExecListOpts)))))) = noArgsToGet
configTimeLimitSetF (Global(Config(ConfigOpts(ConfigTimeLimit(ConfigTimeLimitOpts(ConfigTimeLimitSet(ConfigTimeLimitSetOpts a))))))) = a
configTimeLimitListF (Global(Config(ConfigOpts(ConfigTimeLimit(ConfigTimeLimitOpts(ConfigTimeLimitList ConfigTimeLimitListOpts)))))) = noArgsToGet
configSpaceLimitSetF (Global(Config(ConfigOpts(ConfigSpaceLimit(ConfigSpaceLimitOpts(ConfigSpaceLimitSet(ConfigSpaceLimitSetOpts a))))))) = a
configSpaceLimitListF (Global(Config(ConfigOpts(ConfigSpaceLimit(ConfigSpaceLimitOpts(ConfigSpaceLimitList ConfigSpaceLimitListOpts)))))) = noArgsToGet
configAdminGroupsSetF (Global(Config(ConfigOpts(ConfigAdminGroups(ConfigAdminGroupsOpts(ConfigAdminGroupsSet(ConfigAdminGroupsSetOpts a))))))) = a
configAdminGroupsListF (Global(Config(ConfigOpts(ConfigAdminGroups(ConfigAdminGroupsOpts(ConfigAdminGroupsList ConfigAdminGroupsListOpts)))))) = noArgsToGet
configTeacherGroupsSetF (Global(Config(ConfigOpts(ConfigTeacherGroups(ConfigTeacherGroupsOpts(ConfigTeacherGroupsSet(ConfigTeacherGroupsSetOpts a))))))) = a
configTeacherGroupsListF (Global(Config(ConfigOpts(ConfigTeacherGroups(ConfigTeacherGroupsOpts(ConfigTeacherGroupsList ConfigTeacherGroupsListOpts)))))) = noArgsToGet
configCorrectorIsF (Global(Config(ConfigOpts(ConfigCorrector(ConfigCorrectorOpts(ConfigCorrectorIs(ConfigCorrectorIsOpts a))))))) = a
configCorrectorAddF (Global(Config(ConfigOpts(ConfigCorrector(ConfigCorrectorOpts(ConfigCorrectorAdd(ConfigCorrectorAddOpts a))))))) = a
configCorrectorRemoveF (Global(Config(ConfigOpts(ConfigCorrector(ConfigCorrectorOpts(ConfigCorrectorRemove(ConfigCorrectorRemoveOpts a))))))) = a

