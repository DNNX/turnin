{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Interface.CommandLineParser.ConfigUnitTest where

import Test.Framework
import Interface.CommandLineParser.ParserTestUtils
import Options.Applicative
import Control.Monad
import Security.SecurityManager

import Interface.Lexicon
import Interface.CommandLineParser
import Interface.CommandLineParser.Config

{-# ANN module "HLint: ignore Use camelCase" #-}

test_empty = forM_ allRoles $ \role -> assertEqual Nothing $ execParserMaybe (globalInfo role) []
test_config = forM_ allRoles $ \role -> assertEqual Nothing $ execParserMaybe (globalInfo role) [configSub]
test_configThreshold = forM_ allRoles $ \role -> assertEqual Nothing $ execParserMaybe (globalInfo role) [configSub, thresholdSub] 

test_configThresholdSet = do
  assertEqual (Just(Global(Config(ConfigOpts(ConfigThreshold(ConfigThresholdOpts(ConfigThresholdSet(ConfigThresholdSetOpts o o)))))))) $ execParserMaybe (globalInfo adminRole) [configSub, thresholdSub, setSub]
  assertEqual Nothing $ execParserMaybe (globalInfo adminRole) [configSub, thresholdSub, setSub, "v"]
  forM_ [teacherRole, correctorRole, studentRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [configSub, thresholdSub, setSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [configSub, thresholdSub, setSub, "v"]

test_configThresholdList = do
  forM_ [adminRole, teacherRole] $ \role -> do
    assertEqual (Just(Global(Config(ConfigOpts(ConfigThreshold(ConfigThresholdOpts(ConfigThresholdList ConfigThresholdListOpts))))))) $ execParserMaybe (globalInfo role) [configSub, thresholdSub, listSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [configSub, thresholdSub, listSub, "v"]
  forM_ [correctorRole, studentRole] $ \role -> do      
    assertEqual Nothing $ execParserMaybe (globalInfo role) [configSub, thresholdSub, setSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [configSub, thresholdSub, setSub, "v"]

test_configTermDate = forM_ allRoles $ \role -> assertEqual Nothing $ execParserMaybe (globalInfo role) [configSub, termDateSub] 
test_configTermDateSet = do
  assertEqual (Just(Global(Config(ConfigOpts(ConfigTermDate(ConfigTermDateOpts(ConfigTermDateSet(ConfigTermDateSetOpts o o o)))))))) $ execParserMaybe (globalInfo adminRole) [configSub, termDateSub, setSub]
  assertEqual Nothing $ execParserMaybe (globalInfo adminRole) [configSub, termDateSub, setSub, "v"]
  forM_ [teacherRole, correctorRole, studentRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [configSub, termDateSub, setSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [configSub, termDateSub, setSub, "v"]

test_configTermDateList = do
  forM_ [adminRole, teacherRole] $ \role -> do
    assertEqual (Just(Global(Config(ConfigOpts(ConfigTermDate(ConfigTermDateOpts(ConfigTermDateList ConfigTermDateListOpts))))))) $ execParserMaybe (globalInfo role) [configSub, termDateSub, listSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [configSub, termDateSub, listSub, "v"]
  forM_ [correctorRole, studentRole] $ \role -> do    
    assertEqual Nothing $ execParserMaybe (globalInfo role) [configSub, termDateSub, setSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [configSub, termDateSub, setSub, "v"]

test_configProjectDate = forM_ allRoles $ \role -> assertEqual Nothing $ execParserMaybe (globalInfo role) [configSub, projectDateSub] 
test_configProjectDateSet = do
  assertEqual (Just(Global(Config(ConfigOpts(ConfigProjectDate(ConfigProjectDateOpts(ConfigProjectDateSet(ConfigProjectDateSetOpts o o)))))))) $ execParserMaybe (globalInfo adminRole) [configSub, projectDateSub, setSub]
  assertEqual Nothing $ execParserMaybe (globalInfo adminRole) [configSub, projectDateSub, setSub, "v"]
  forM_ [teacherRole, correctorRole, studentRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [configSub, projectDateSub, setSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [configSub, projectDateSub, setSub, "v"]

test_configProjectDateList = do
  forM_ [adminRole, teacherRole] $ \role -> do
    assertEqual (Just(Global(Config(ConfigOpts(ConfigProjectDate(ConfigProjectDateOpts(ConfigProjectDateList ConfigProjectDateListOpts))))))) $ execParserMaybe (globalInfo role) [configSub, projectDateSub, listSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [configSub, projectDateSub, listSub, "v"]
  forM_ [correctorRole, studentRole] $ \role -> do    
    assertEqual Nothing $ execParserMaybe (globalInfo role) [configSub, projectDateSub, setSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [configSub, projectDateSub, setSub, "v"]

test_configAcceptExec = forM_ allRoles $ \role -> assertEqual Nothing $ execParserMaybe (globalInfo role) [configSub, acceptExecSub]
test_configAcceptExecSet = do
  assertEqual Nothing $ execParserMaybe (globalInfo adminRole) [configSub, acceptExecSub, setSub]
  assertEqual (Just(Global(Config(ConfigOpts(ConfigAcceptExec(ConfigAcceptExecOpts(ConfigAcceptExecSet(ConfigAcceptExecSetOpts "acceptExec")))))))) $ execParserMaybe (globalInfo adminRole) [configSub, acceptExecSub, setSub, "acceptExec"]
  assertEqual Nothing $ execParserMaybe (globalInfo adminRole) [configSub, acceptExecSub, setSub, "acceptExec", "v"]
  forM_ [teacherRole, correctorRole, studentRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [configSub, acceptExecSub, setSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [configSub, acceptExecSub, setSub, "acceptExec"]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [configSub, acceptExecSub, setSub, "acceptExec", "v"]

test_configAcceptExecList = do
  forM_ [adminRole, teacherRole] $ \role -> do
    assertEqual (Just(Global(Config(ConfigOpts(ConfigAcceptExec(ConfigAcceptExecOpts(ConfigAcceptExecList ConfigAcceptExecListOpts))))))) $ execParserMaybe (globalInfo role) [configSub, acceptExecSub, listSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [configSub, acceptExecSub, listSub, "v"]
  forM_ [correctorRole, studentRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [configSub, acceptExecSub, listSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [configSub, acceptExecSub, listSub, "v"]

test_configTimeLimit = forM_ allRoles $ \role -> assertEqual Nothing $ execParserMaybe (globalInfo role) [configSub, timeLimitSub]
test_configTimeLimitSet = do
  assertEqual Nothing $ execParserMaybe (globalInfo adminRole) [configSub, timeLimitSub, setSub]
  assertEqual (Just(Global(Config(ConfigOpts(ConfigTimeLimit(ConfigTimeLimitOpts(ConfigTimeLimitSet(ConfigTimeLimitSetOpts "timeLimit")))))))) $ execParserMaybe (globalInfo adminRole) [configSub, timeLimitSub, setSub, "timeLimit"]
  assertEqual Nothing $ execParserMaybe (globalInfo adminRole) [configSub, timeLimitSub, setSub, "timeLimit", "v"]
  forM_ [teacherRole, correctorRole, studentRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [configSub, timeLimitSub, setSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [configSub, timeLimitSub, setSub, "timeLimit"]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [configSub, timeLimitSub, setSub, "timeLimit", "v"]

test_configTimeLimitList = do
  forM_ [adminRole, teacherRole] $ \role -> do
    assertEqual (Just(Global(Config(ConfigOpts(ConfigTimeLimit(ConfigTimeLimitOpts(ConfigTimeLimitList ConfigTimeLimitListOpts))))))) $ execParserMaybe (globalInfo role) [configSub, timeLimitSub, listSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [configSub, timeLimitSub, listSub, "v"]
  forM_ [correctorRole, studentRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [configSub, timeLimitSub, listSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [configSub, timeLimitSub, listSub, "v"]
    
test_configSpaceLimit = forM_ allRoles $ \role -> assertEqual Nothing $ execParserMaybe (globalInfo role) [configSub, spaceLimitSub]
test_configSpaceLimitSet = do
  assertEqual Nothing $ execParserMaybe (globalInfo adminRole) [configSub, spaceLimitSub, setSub]
  assertEqual (Just(Global(Config(ConfigOpts(ConfigSpaceLimit(ConfigSpaceLimitOpts(ConfigSpaceLimitSet(ConfigSpaceLimitSetOpts "spaceLimit")))))))) $ execParserMaybe (globalInfo adminRole) [configSub, spaceLimitSub, setSub, "spaceLimit"]
  assertEqual Nothing $ execParserMaybe (globalInfo adminRole) [configSub, spaceLimitSub, setSub, "spaceLimit", "v"]
  forM_ [teacherRole, correctorRole, studentRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [configSub, spaceLimitSub, setSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [configSub, spaceLimitSub, setSub, "spaceLimit"]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [configSub, spaceLimitSub, setSub, "spaceLimit", "v"]
 
test_configSpaceLimitList = do
  forM_ [adminRole, teacherRole] $ \role -> do
    assertEqual (Just(Global(Config(ConfigOpts(ConfigSpaceLimit(ConfigSpaceLimitOpts(ConfigSpaceLimitList ConfigSpaceLimitListOpts))))))) $ execParserMaybe (globalInfo role) [configSub, spaceLimitSub, listSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [configSub, spaceLimitSub, listSub, "v"]
  forM_ [correctorRole, studentRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [configSub, spaceLimitSub, listSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [configSub, spaceLimitSub, listSub, "v"]

test_configAdminGroups = forM_ allRoles $ \role -> assertEqual Nothing $ execParserMaybe (globalInfo role) [configSub, adminGroupsSub]
test_configAdminGroupsSet = do
  assertEqual Nothing $ execParserMaybe (globalInfo adminRole) [configSub, adminGroupsSub, setSub]
  assertEqual (Just(Global(Config(ConfigOpts(ConfigAdminGroups(ConfigAdminGroupsOpts(ConfigAdminGroupsSet(ConfigAdminGroupsSetOpts ["group.."])))))))) $ execParserMaybe (globalInfo adminRole) [configSub, adminGroupsSub, setSub, "group.."]
  forM_ [teacherRole, correctorRole, studentRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [configSub, adminGroupsSub, setSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [configSub, adminGroupsSub, setSub, "group.."]
 
test_configAdminGroupsList = do
  forM_ [adminRole, teacherRole] $ \role -> do
    assertEqual (Just(Global(Config(ConfigOpts(ConfigAdminGroups(ConfigAdminGroupsOpts(ConfigAdminGroupsList ConfigAdminGroupsListOpts))))))) $ execParserMaybe (globalInfo role) [configSub, adminGroupsSub, listSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [configSub, adminGroupsSub, listSub, "v"]
  forM_ [correctorRole, studentRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [configSub, adminGroupsSub, listSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [configSub, adminGroupsSub, listSub, "v"]

test_configTeacherGroups = forM_ allRoles $ \role -> assertEqual Nothing $ execParserMaybe (globalInfo role) [configSub, adminGroupsSub]
test_configTeacherGroupsSet = do
  assertEqual (Just(Global(Config(ConfigOpts(ConfigTeacherGroups(ConfigTeacherGroupsOpts(ConfigTeacherGroupsSet(ConfigTeacherGroupsSetOpts [])))))))) $  
                                                                                                execParserMaybe (globalInfo adminRole) [configSub, teacherGroupsSub, setSub]
  assertEqual (Just(Global(Config(ConfigOpts(ConfigTeacherGroups(ConfigTeacherGroupsOpts(ConfigTeacherGroupsSet(ConfigTeacherGroupsSetOpts ["group.."])))))))) $ 
                                                                                                execParserMaybe (globalInfo adminRole) [configSub, teacherGroupsSub, setSub, "group.."]
  forM_ [teacherRole, correctorRole, studentRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [configSub, teacherGroupsSub, setSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [configSub, teacherGroupsSub, setSub, "group.."]
 
test_configTeacherGroupsList = do
  forM_ [adminRole, teacherRole] $ \role -> do
    assertEqual (Just(Global(Config(ConfigOpts(ConfigTeacherGroups(ConfigTeacherGroupsOpts(ConfigTeacherGroupsList ConfigTeacherGroupsListOpts))))))) $ execParserMaybe (globalInfo role) [configSub, teacherGroupsSub, listSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [configSub, teacherGroupsSub, listSub, "v"]
  forM_ [correctorRole, studentRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [configSub, teacherGroupsSub, listSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [configSub, teacherGroupsSub, listSub, "v"]

test_configCorrector = forM_ allRoles $ \role -> assertEqual Nothing $ execParserMaybe (globalInfo role) [configSub, correctorSub]
test_configCorrectorIs = do
  forM_ [adminRole, teacherRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [configSub, correctorSub, isSub]
    assertEqual (Just(Global(Config(ConfigOpts(ConfigCorrector(ConfigCorrectorOpts(ConfigCorrectorIs(ConfigCorrectorIsOpts "corr")))))))) $ execParserMaybe (globalInfo role) [configSub, correctorSub, isSub, "corr"]
  forM_ [correctorRole, studentRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [configSub, correctorSub, isSub, "corr"]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [configSub, correctorSub, isSub]

test_configCorrectorAdd = do
  forM_ [adminRole, teacherRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [configSub, correctorSub, addSub]
    assertEqual (Just(Global(Config(ConfigOpts(ConfigCorrector(ConfigCorrectorOpts(ConfigCorrectorAdd(ConfigCorrectorAddOpts "corr")))))))) $ execParserMaybe (globalInfo role) [configSub, correctorSub, addSub, "corr"]
  forM_ [correctorRole, studentRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [configSub, correctorSub, addSub, "corr"]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [configSub, correctorSub, addSub]
    
test_configCorrectorRemove = do
  forM_ [adminRole, teacherRole] $ \role -> do 
    assertEqual Nothing $ execParserMaybe (globalInfo role) [configSub, correctorSub, removeSub]
    assertEqual (Just(Global(Config(ConfigOpts(ConfigCorrector(ConfigCorrectorOpts(ConfigCorrectorRemove(ConfigCorrectorRemoveOpts "corr")))))))) $ execParserMaybe (globalInfo role) [configSub, correctorSub, removeSub, "corr"]
  forM_ [correctorRole, studentRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [configSub, correctorSub, removeSub, "corr"]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [configSub, correctorSub, removeSub]
         
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

