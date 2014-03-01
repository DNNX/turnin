{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Interface.CommandLineParser.ProjectWorktrainSubmitUnitTest where

import Test.Framework
import Interface.CommandLineParser.ParserTestUtils
import Control.Monad
import Options.Applicative
import Security.SecurityManager

import Interface.Lexicon
import Interface.CommandLineParser
import Interface.CommandLineParser.Project
import Interface.CommandLineParser.Project.Worktrain
import Interface.CommandLineParser.Project.Submit

{-# ANN module "HLint: ignore Use camelCase" #-}

test_projectWorktrain = forM_ allRoles $ \role -> assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, worktrainSub]
test_projectWorktrainScript = forM_ allRoles $ \role -> assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, worktrainSub, scriptSub]
test_projectWorktrainScriptSet = do
  forM_ [adminRole, teacherRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, worktrainSub, scriptSub, setSub]
    assertEqual (Just(Global(Project(ProjectOpts(ProjectWorktrain(ProjectWorktrainOpts(ProjectWorktrainScript(ProjectWorktrainScriptOpts(ProjectWorktrainScriptSet(ProjectWorktrainScriptSetOpts o o o o o "name")))))))))) $
                                                                                                                execParserMaybe (globalInfo role) [projectSub, worktrainSub, scriptSub, setSub, "name"]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, worktrainSub, scriptSub, setSub, "name", "v"]
  forM_ [correctorRole, studentRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, worktrainSub, scriptSub, setSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, worktrainSub, scriptSub, setSub, "name"]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, worktrainSub, scriptSub, setSub, "name", "v"]

test_projectWorktrainScriptUnset = do
  forM_ [adminRole, teacherRole] $ \role -> do
    assertEqual (Just(Global(Project(ProjectOpts(ProjectWorktrain(ProjectWorktrainOpts(ProjectWorktrainScript(ProjectWorktrainScriptOpts(ProjectWorktrainScriptUnset(ProjectWorktrainScriptUnsetOpts o o o o o)))))))))) $
                                                                                                                execParserMaybe (globalInfo role) [projectSub, worktrainSub, scriptSub, unsetSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, worktrainSub, scriptSub, unsetSub, "v"]
  forM_ [correctorRole, studentRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, worktrainSub, scriptSub, unsetSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, worktrainSub, scriptSub, unsetSub, "v"]

test_projectWorktrainScriptList = do
  forM_ [adminRole, teacherRole, correctorRole] $ \role -> do
    assertEqual (Just(Global(Project(ProjectOpts(ProjectWorktrain(ProjectWorktrainOpts(ProjectWorktrainScript(ProjectWorktrainScriptOpts(ProjectWorktrainScriptList(ProjectWorktrainScriptListOpts o o o o o)))))))))) $
                                                                                                                execParserMaybe (globalInfo role) [projectSub, worktrainSub, scriptSub, listSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, worktrainSub, scriptSub, listSub, "v"]
  assertEqual Nothing $ execParserMaybe (globalInfo studentRole) [projectSub, worktrainSub, scriptSub, listSub]
  assertEqual Nothing $ execParserMaybe (globalInfo studentRole) [projectSub, worktrainSub, scriptSub, listSub, "v"]

test_projectWorktrainScriptExtract = do
  forM_ [adminRole, teacherRole, correctorRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, worktrainSub, scriptSub, extractSub]
    assertEqual (Just(Global(Project(ProjectOpts(ProjectWorktrain(ProjectWorktrainOpts(ProjectWorktrainScript(ProjectWorktrainScriptOpts(ProjectWorktrainScriptExtract(ProjectWorktrainScriptExtractOpts o o o o o "dir")))))))))) $
                                                                                                                execParserMaybe (globalInfo role) [projectSub, worktrainSub, scriptSub, extractSub, "dir"]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, worktrainSub, scriptSub, extractSub, "dir", "v"]
  assertEqual Nothing $ execParserMaybe (globalInfo studentRole) [projectSub, worktrainSub, scriptSub, extractSub]
  assertEqual Nothing $ execParserMaybe (globalInfo studentRole) [projectSub, worktrainSub, scriptSub, extractSub, "dir"]
  assertEqual Nothing $ execParserMaybe (globalInfo studentRole) [projectSub, worktrainSub, scriptSub, extractSub, "dir", "v"]

test_projectWorktrainFile = forM_ allRoles $ \role -> assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, worktrainSub, fileSub]
test_projectWorktrainFileAdd = do
  forM_ [adminRole, teacherRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, worktrainSub, fileSub, addSub]
    assertEqual (Just(Global(Project(ProjectOpts(ProjectWorktrain(ProjectWorktrainOpts(ProjectWorktrainFile(ProjectWorktrainFileOpts(ProjectWorktrainFileAdd(ProjectWorktrainFileAddOpts o o o o o ["file.."])))))))))) $
                                                                                                                execParserMaybe (globalInfo role) [projectSub, worktrainSub, fileSub, addSub, "file.."]
  forM_ [correctorRole, studentRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, worktrainSub, fileSub, addSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, worktrainSub, fileSub, addSub, "file.."]

test_projectWorktrainFileRemove = do
  forM_ [adminRole, teacherRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, worktrainSub, fileSub, removeSub]
    assertEqual (Just(Global(Project(ProjectOpts(ProjectWorktrain(ProjectWorktrainOpts(ProjectWorktrainFile(ProjectWorktrainFileOpts(ProjectWorktrainFileRemove(ProjectWorktrainFileRemoveOpts o o o o o ["file.."])))))))))) $
                                                                                                                execParserMaybe (globalInfo role) [projectSub, worktrainSub, fileSub, removeSub, "file.."]
  forM_ [correctorRole, studentRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, worktrainSub, fileSub, removeSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, worktrainSub, fileSub, removeSub, "file.."]

test_projectWorktrainFileList = do
  forM_ [adminRole, teacherRole, correctorRole] $ \role -> do
    assertEqual (Just(Global(Project(ProjectOpts(ProjectWorktrain(ProjectWorktrainOpts(ProjectWorktrainFile(ProjectWorktrainFileOpts(ProjectWorktrainFileList(ProjectWorktrainFileListOpts o o o o o)))))))))) $
                                                                                                                execParserMaybe (globalInfo role) [projectSub, worktrainSub, fileSub, listSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, worktrainSub, fileSub, listSub, "v"]
  assertEqual Nothing $ execParserMaybe (globalInfo studentRole) [projectSub, worktrainSub, fileSub, listSub]
  assertEqual Nothing $ execParserMaybe (globalInfo studentRole) [projectSub, worktrainSub, fileSub, listSub, "v"]

test_projectWorktrainFileExtract = do
  forM_ [adminRole, teacherRole, correctorRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, worktrainSub, fileSub, extractSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, worktrainSub, fileSub, extractSub, "dir"]
    assertEqual (Just(Global(Project(ProjectOpts(ProjectWorktrain(ProjectWorktrainOpts(ProjectWorktrainFile(ProjectWorktrainFileOpts(ProjectWorktrainFileExtract(ProjectWorktrainFileExtractOpts o o o o o "dir" ["file.."])))))))))) $
                                                                                                                execParserMaybe (globalInfo role) [projectSub, worktrainSub, fileSub, extractSub, "dir", "file.."]
  assertEqual Nothing $ execParserMaybe (globalInfo studentRole) [projectSub, worktrainSub, fileSub, extractSub]
  assertEqual Nothing $ execParserMaybe (globalInfo studentRole) [projectSub, worktrainSub, fileSub, extractSub, "dir"]
  assertEqual Nothing $ execParserMaybe (globalInfo studentRole) [projectSub, worktrainSub, fileSub, extractSub, "dir", "file.."]

test_projectWorktrainTimeLimit = forM_ allRoles $ \role -> assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, worktrainSub, timeLimitSub]
test_projectWorktrainTimeLimitSet = do
  forM_ [adminRole, teacherRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, worktrainSub, timeLimitSub, setSub]
    assertEqual (Just(Global(Project(ProjectOpts(ProjectWorktrain(ProjectWorktrainOpts(ProjectWorktrainTimeLimit(ProjectWorktrainTimeLimitOpts(ProjectWorktrainTimeLimitSet(ProjectWorktrainTimeLimitSetOpts o o o o o "timeLimit")))))))))) $
                                                                                                                execParserMaybe (globalInfo role) [projectSub, worktrainSub, timeLimitSub, setSub, "timeLimit"]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, worktrainSub, timeLimitSub, setSub, "timeLimit", "v"]
  forM_ [correctorRole, studentRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, worktrainSub, timeLimitSub, setSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, worktrainSub, timeLimitSub, setSub, "timeLimit"]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, worktrainSub, timeLimitSub, setSub, "timeLimit", "v"]

test_projectWorktrainTimeLimitList = do
  forM_ [adminRole, teacherRole, correctorRole] $ \role -> do
    assertEqual (Just(Global(Project(ProjectOpts(ProjectWorktrain(ProjectWorktrainOpts(ProjectWorktrainTimeLimit(ProjectWorktrainTimeLimitOpts(ProjectWorktrainTimeLimitList(ProjectWorktrainTimeLimitListOpts o o o o o)))))))))) $
                                                                                                                execParserMaybe (globalInfo role) [projectSub, worktrainSub, timeLimitSub, listSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, worktrainSub, timeLimitSub, listSub, "v"]
  assertEqual Nothing $ execParserMaybe (globalInfo studentRole) [projectSub, worktrainSub, timeLimitSub, listSub]
  assertEqual Nothing $ execParserMaybe (globalInfo studentRole) [projectSub, worktrainSub, timeLimitSub, listSub, "v"]

test_projectWorktrainSpaceLimit = forM_ allRoles $ \role -> assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, worktrainSub, spaceLimitSub]
test_projectWorktrainSpaceLimitSet = do
  forM_ [adminRole, teacherRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, worktrainSub, spaceLimitSub, setSub]
    assertEqual (Just(Global(Project(ProjectOpts(ProjectWorktrain(ProjectWorktrainOpts(ProjectWorktrainSpaceLimit(ProjectWorktrainSpaceLimitOpts(ProjectWorktrainSpaceLimitSet(ProjectWorktrainSpaceLimitSetOpts o o o o o "spaceLimit")))))))))) $
                                                                                                                execParserMaybe (globalInfo role) [projectSub, worktrainSub, spaceLimitSub, setSub, "spaceLimit"]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, worktrainSub, spaceLimitSub, setSub, "spaceLimit", "v"]
  forM_ [correctorRole, studentRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, worktrainSub, spaceLimitSub, setSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, worktrainSub, spaceLimitSub, setSub, "spaceLimit"]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, worktrainSub, spaceLimitSub, setSub, "spaceLimit", "v"]

test_projectWorktrainSpaceLimitList = do
  forM_ [adminRole, teacherRole, correctorRole] $ \role -> do
    assertEqual (Just(Global(Project(ProjectOpts(ProjectWorktrain(ProjectWorktrainOpts(ProjectWorktrainSpaceLimit(ProjectWorktrainSpaceLimitOpts(ProjectWorktrainSpaceLimitList(ProjectWorktrainSpaceLimitListOpts o o o o o)))))))))) $
                                                                                                                execParserMaybe (globalInfo role) [projectSub, worktrainSub, spaceLimitSub, listSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, worktrainSub, spaceLimitSub, listSub, "v"]
  assertEqual Nothing $ execParserMaybe (globalInfo studentRole) [projectSub, worktrainSub, spaceLimitSub, listSub]
  assertEqual Nothing $ execParserMaybe (globalInfo studentRole) [projectSub, worktrainSub, spaceLimitSub, listSub, "v"]


test_projectSubmit = forM_ allRoles $ \role -> assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, submitSub]
test_projectSubmitList = do
  forM_ [adminRole, teacherRole, correctorRole] $ \role -> do
    assertEqual (Just(Global(Project(ProjectOpts(ProjectSubmit(ProjectSubmitOpts(ProjectSubmitList(ProjectSubmitListOpts o o o o o)))))))) $ execParserMaybe (globalInfo role) [projectSub, submitSub, listSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, submitSub, listSub, "v"]
  assertEqual Nothing $ execParserMaybe (globalInfo studentRole) [projectSub, submitSub, listSub]
  assertEqual Nothing $ execParserMaybe (globalInfo studentRole) [projectSub, submitSub, listSub, "v"]

test_projectSubmitLate = do
  forM_ [adminRole, teacherRole, correctorRole] $ \role -> do
    assertEqual (Just(Global(Project(ProjectOpts(ProjectSubmit(ProjectSubmitOpts(ProjectSubmitLate(ProjectSubmitLateOpts o o o o o)))))))) $ execParserMaybe (globalInfo role) [projectSub, submitSub, lateSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, submitSub, lateSub, "v"]
  assertEqual Nothing $ execParserMaybe (globalInfo studentRole) [projectSub, submitSub, lateSub]
  assertEqual Nothing $ execParserMaybe (globalInfo studentRole) [projectSub, submitSub, lateSub, "v"]

test_projectSubmitInspect = do
  forM_ [adminRole, teacherRole, correctorRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, submitSub, inspectSub]
    assertEqual (Just(Global(Project(ProjectOpts(ProjectSubmit(ProjectSubmitOpts(ProjectSubmitInspect(ProjectSubmitInspectOpts o o o o o ["key.."])))))))) $
                                                                                                execParserMaybe (globalInfo role) [projectSub, submitSub, inspectSub, "key.."]
  assertEqual Nothing $ execParserMaybe (globalInfo studentRole) [projectSub, submitSub, lateSub]
  assertEqual Nothing $ execParserMaybe (globalInfo studentRole) [projectSub, submitSub, lateSub, "key.."]

test_projectSubmitExtract = do
  forM_ [adminRole, teacherRole, correctorRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, submitSub, extractSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, submitSub, extractSub, "dir"]
    assertEqual (Just(Global(Project(ProjectOpts(ProjectSubmit(ProjectSubmitOpts(ProjectSubmitExtract(ProjectSubmitExtractOpts o o o o o "dir" ["key.."])))))))) $
                                                                                                execParserMaybe (globalInfo role) [projectSub, submitSub, extractSub, "dir", "key.."]
  assertEqual Nothing $ execParserMaybe (globalInfo studentRole) [projectSub, submitSub, extractSub]
  assertEqual Nothing $ execParserMaybe (globalInfo studentRole) [projectSub, submitSub, extractSub, "dir"]
  assertEqual Nothing $ execParserMaybe (globalInfo studentRole) [projectSub, submitSub, extractSub, "dir", "key.."]
  