{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Interface.CommandLineParser.ProjectValidateUnitTest where
import Test.Framework
import Interface.CommandLineParser.ParserTestUtils
import Control.Monad
import Security.SecurityManager
import Options.Applicative

import Interface.Lexicon
import Interface.CommandLineParser
import Interface.CommandLineParser.Project
import Interface.CommandLineParser.Project.Validate

{-# ANN module "HLint: ignore Use camelCase" #-}

test_project = forM_ allRoles $ \role -> assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub]
test_projectAdd = do
  forM_ [adminRole, teacherRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, addSub]
    assertEqual (Just(Global(Project(ProjectOpts(ProjectAdd(ProjectAddOpts o o o o o o o "name")))))) $ execParserMaybe (globalInfo role) [projectSub, addSub, "name"]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, addSub, "name", "v"]
  forM_ [correctorRole, studentRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, addSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, addSub, "name"]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, addSub, "name", "v"]

test_projectRemove = do
  forM_ [adminRole, teacherRole] $ \role -> do
    assertEqual (Just(Global(Project(ProjectOpts(ProjectRemove(ProjectRemoveOpts o o o o o)))))) $ execParserMaybe (globalInfo role) [projectSub, removeSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, removeSub, "v"]
  forM_ [correctorRole, studentRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, removeSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, removeSub, "v"]

test_projectList = do
  forM_ [adminRole, teacherRole, correctorRole] $ \role -> do
    assertEqual (Just(Global(Project(ProjectOpts(ProjectList(ProjectListOpts o o o o)))))) $ execParserMaybe (globalInfo role) [projectSub, listSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, listSub, "v"]
  assertEqual Nothing $ execParserMaybe (globalInfo studentRole) [projectSub, listSub]
  assertEqual Nothing $ execParserMaybe (globalInfo studentRole) [projectSub, listSub, "v"]

test_projectDate = forM_ allRoles $ \role -> assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, dateSub]
test_projectDateSet = do
  forM_ [adminRole, teacherRole] $ \role -> do
    assertEqual (Just(Global(Project(ProjectOpts(ProjectDate(ProjectDateOpts(ProjectDateSet(ProjectDateSetOpts o o o o o o o o)))))))) $ execParserMaybe (globalInfo role) [projectSub, dateSub, setSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, dateSub, setSub, "v"]
  forM_ [correctorRole, studentRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, dateSub, setSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, dateSub, setSub, "v"]

test_projectDateList = do
  forM_ [adminRole, teacherRole, correctorRole] $ \role -> do
    assertEqual (Just(Global(Project(ProjectOpts(ProjectDate(ProjectDateOpts(ProjectDateList(ProjectDateListOpts o o o o o)))))))) $ execParserMaybe (globalInfo role) [projectSub, dateSub, listSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, dateSub, listSub, "v"]
  assertEqual Nothing $ execParserMaybe (globalInfo studentRole) [projectSub, dateSub, listSub]
  assertEqual Nothing $ execParserMaybe (globalInfo studentRole) [projectSub, dateSub, listSub, "v"]


test_projectValidate = forM_ allRoles $ \role -> assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, validateSub]
test_projectValidateAcceptexec = forM_ allRoles $ \role -> assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, validateSub, acceptExecSub]
test_projectValidateAcceptExecSet = do
  forM_ [adminRole, teacherRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, validateSub, acceptExecSub, setSub]
    assertEqual (Just(Global(Project(ProjectOpts(ProjectValidate(ProjectValidateOpts(ProjectValidateAcceptExec(ProjectValidateAcceptExecOpts(ProjectValidateAcceptExecSet(ProjectValidateAcceptExecSetOpts o o o o o "acceptExec")))))))))) $
                                                                                                execParserMaybe (globalInfo role) [projectSub, validateSub, acceptExecSub, setSub, "acceptExec"]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, validateSub, acceptExecSub, setSub, "acceptExec", "v"]
  forM_ [correctorRole, studentRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, validateSub, acceptExecSub, setSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, validateSub, acceptExecSub, setSub, "v"]

test_projectValidateAcceptExecList = do
  forM_ [adminRole, teacherRole, correctorRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, validateSub, acceptExecSub, setSub]
    assertEqual (Just(Global(Project(ProjectOpts(ProjectValidate(ProjectValidateOpts(ProjectValidateAcceptExec(ProjectValidateAcceptExecOpts(ProjectValidateAcceptExecList(ProjectValidateAcceptExecListOpts o o o o o)))))))))) $
                                                                                                execParserMaybe (globalInfo role) [projectSub, validateSub, acceptExecSub, listSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, validateSub, acceptExecSub, listSub, "v"]
  forM_ [correctorRole, studentRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, dateSub, setSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, dateSub, setSub, "v"]

test_projectValidateName = forM_ allRoles $ \role -> assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, validateSub, nameSub]
test_projectValidateNameAdd = do
  forM_ [adminRole, teacherRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, validateSub, nameSub, addSub]
    assertEqual (Just(Global(Project(ProjectOpts(ProjectValidate(ProjectValidateOpts(ProjectValidateName(ProjectValidateNameOpts(ProjectValidateNameAdd(ProjectValidateNameAddOpts o o o o o ["name.."])))))))))) $
                                                                                                                execParserMaybe (globalInfo role) [projectSub, validateSub, nameSub, addSub, "name.."]
  forM_ [correctorRole, studentRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, validateSub, nameSub, addSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, validateSub, nameSub, addSub, "name.."]

test_projectValidateNameRemove = do
  forM_ [adminRole, teacherRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, validateSub, nameSub, removeSub]
    assertEqual (Just(Global(Project(ProjectOpts(ProjectValidate(ProjectValidateOpts(ProjectValidateName(ProjectValidateNameOpts(ProjectValidateNameRemove(ProjectValidateNameRemoveOpts o o o o o ["name.."])))))))))) $
                                                                                                                execParserMaybe (globalInfo role) [projectSub, validateSub, nameSub, removeSub, "name.."]
  forM_ [correctorRole, studentRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, validateSub, nameSub, removeSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, validateSub, nameSub, removeSub, "name.."]

test_projectValidateNameList = do
  forM_ [adminRole, teacherRole, correctorRole] $ \role -> do
    assertEqual (Just(Global(Project(ProjectOpts(ProjectValidate(ProjectValidateOpts(ProjectValidateName(ProjectValidateNameOpts(ProjectValidateNameList(ProjectValidateNameListOpts o o o o o)))))))))) $
                                                                                                                execParserMaybe (globalInfo role) [projectSub, validateSub, nameSub, listSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, validateSub, nameSub, listSub, "v"]
  assertEqual Nothing $ execParserMaybe (globalInfo studentRole) [projectSub, validateSub, nameSub, listSub]
  assertEqual Nothing $ execParserMaybe (globalInfo studentRole) [projectSub, validateSub, nameSub, listSub, "v"]

test_projectValidateCommand = forM_ allRoles $ \role -> assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, validateSub, commandSub]
test_projectValidateCommandSet = do
  forM_ [adminRole, teacherRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, validateSub, commandSub, setSub]
    assertEqual (Just(Global(Project(ProjectOpts(ProjectValidate(ProjectValidateOpts(ProjectValidateCommand(ProjectValidateCommandOpts(ProjectValidateCommandSet(ProjectValidateCommandSetOpts o o o o o "command")))))))))) $
                                                                                                                execParserMaybe (globalInfo role) [projectSub, validateSub, commandSub, setSub, "command"]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, validateSub, commandSub, setSub, "command", "v"]
  forM_ [correctorRole, studentRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, validateSub, commandSub, setSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, validateSub, commandSub, setSub, "command"]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, validateSub, commandSub, setSub, "command", "v"]

test_projectValidateCommandUnset = do
  forM_ [adminRole, teacherRole] $ \role -> do
    assertEqual (Just(Global(Project(ProjectOpts(ProjectValidate(ProjectValidateOpts(ProjectValidateCommand(ProjectValidateCommandOpts(ProjectValidateCommandUnset(ProjectValidateCommandUnsetOpts o o o o o)))))))))) $
                                                                                                                execParserMaybe (globalInfo role) [projectSub, validateSub, commandSub, unsetSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, validateSub, commandSub, unsetSub, "v"]
  forM_ [correctorRole, studentRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, validateSub, commandSub, unsetSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, validateSub, commandSub, unsetSub, "v"]

test_projectValidateCommandList = do
  forM_ [adminRole, teacherRole, correctorRole] $ \role -> do
    assertEqual (Just(Global(Project(ProjectOpts(ProjectValidate(ProjectValidateOpts(ProjectValidateCommand(ProjectValidateCommandOpts(ProjectValidateCommandList(ProjectValidateCommandListOpts o o o o o)))))))))) $
                                                                                                                execParserMaybe (globalInfo role) [projectSub, validateSub, commandSub, listSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, validateSub, commandSub, listSub, "v"]
  assertEqual Nothing $ execParserMaybe (globalInfo studentRole) [projectSub, validateSub, commandSub, listSub]
  assertEqual Nothing $ execParserMaybe (globalInfo studentRole) [projectSub, validateSub, commandSub, listSub, "v"]

test_projectValidateScript = forM_ allRoles $ \role -> assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, validateSub, scriptSub]
test_projectValidateScriptSet = do
  forM_ [adminRole, teacherRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, validateSub, scriptSub, setSub]
    assertEqual (Just(Global(Project(ProjectOpts(ProjectValidate(ProjectValidateOpts(ProjectValidateScript(ProjectValidateScriptOpts(ProjectValidateScriptSet(ProjectValidateScriptSetOpts o o o o o "name")))))))))) $
                                                                                                                execParserMaybe (globalInfo role) [projectSub, validateSub, scriptSub, setSub, "name"]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, validateSub, scriptSub, setSub, "name", "v"]
  forM_ [correctorRole, studentRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, validateSub, scriptSub, setSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, validateSub, scriptSub, setSub, "name"]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, validateSub, scriptSub, setSub, "name", "v"]

test_projectValidateScriptUnset = do
  forM_ [adminRole, teacherRole] $ \role -> do
    assertEqual (Just(Global(Project(ProjectOpts(ProjectValidate(ProjectValidateOpts(ProjectValidateScript(ProjectValidateScriptOpts(ProjectValidateScriptUnset(ProjectValidateScriptUnsetOpts o o o o o)))))))))) $
                                                                                                                execParserMaybe (globalInfo role) [projectSub, validateSub, scriptSub, unsetSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, validateSub, scriptSub, unsetSub, "v"]
  forM_ [correctorRole, studentRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, validateSub, scriptSub, unsetSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, validateSub, scriptSub, unsetSub, "v"]

test_projectValidateScriptList = do
  forM_ [adminRole, teacherRole, correctorRole] $ \role -> do
    assertEqual (Just(Global(Project(ProjectOpts(ProjectValidate(ProjectValidateOpts(ProjectValidateScript(ProjectValidateScriptOpts(ProjectValidateScriptList(ProjectValidateScriptListOpts o o o o o)))))))))) $
                                                                                                                execParserMaybe (globalInfo role) [projectSub, validateSub, scriptSub, listSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, validateSub, scriptSub, listSub, "v"]
  assertEqual Nothing $ execParserMaybe (globalInfo studentRole) [projectSub, validateSub, scriptSub, listSub]
  assertEqual Nothing $ execParserMaybe (globalInfo studentRole) [projectSub, validateSub, scriptSub, listSub, "v"]

test_projectValidateScriptExtract = do
  forM_ [adminRole, teacherRole, correctorRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, validateSub, scriptSub, extractSub]
    assertEqual (Just(Global(Project(ProjectOpts(ProjectValidate(ProjectValidateOpts(ProjectValidateScript(ProjectValidateScriptOpts(ProjectValidateScriptExtract(ProjectValidateScriptExtractOpts o o o o o "dir")))))))))) $
                                                                                                                execParserMaybe (globalInfo role) [projectSub, validateSub, scriptSub, extractSub, "dir"]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [projectSub, validateSub, scriptSub, extractSub, "dir", "v"]
  assertEqual Nothing $ execParserMaybe (globalInfo studentRole) [projectSub, validateSub, scriptSub, extractSub]
  assertEqual Nothing $ execParserMaybe (globalInfo studentRole) [projectSub, validateSub, scriptSub, extractSub, "dir"]
  assertEqual Nothing $ execParserMaybe (globalInfo studentRole) [projectSub, validateSub, scriptSub, extractSub, "dir", "v"]
