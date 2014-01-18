{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Interface.CommandLineParser.ProjectWorktrainSubmitTest where

import Test.Framework
import Interface.CommandLineParser.ParserTestUtils

import Interface.Lexicon
import Interface.CommandLineParser
import Interface.CommandLineParser.Project
import Interface.CommandLineParser.Project.Worktrain
import Interface.CommandLineParser.Project.Submit

{-# ANN module "HLint: ignore Use camelCase" #-}
 
prop_projectWorktrainScriptSetSuccess repoNN termNN courseNN groupNN projectNN s = let ss = validArgs [s] in ss /= [] ==>
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==> let scriptName = head ss in
  testSuccess (repoNN, termNN, courseNN, groupNN, projectNN, scriptName) x [projectSub, worktrainSub, scriptSub, setSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) [scriptName]
    where x (Global(  
             Project(ProjectOpts(
              ProjectWorktrain(ProjectWorktrainOpts(
               ProjectWorktrainScript(ProjectWorktrainScriptOpts(
                ProjectWorktrainScriptSet(ProjectWorktrainScriptSetOpts a b c d e f))))))))) = (a,b,c,d,e,f)

prop_projectWorktrainScriptUnsetSuccess repoNN termNN courseNN groupNN projectNN =
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess (repoNN, termNN, courseNN, groupNN, projectNN) x [projectSub, worktrainSub, scriptSub, unsetSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) noArgs
    where x (Global(
             Project(ProjectOpts(
              ProjectWorktrain(ProjectWorktrainOpts(
               ProjectWorktrainScript(ProjectWorktrainScriptOpts(
                ProjectWorktrainScriptUnset(ProjectWorktrainScriptUnsetOpts a b c d e))))))))) = (a,b,c,d,e)

prop_projectWorktrainScriptListSuccess repoNN termNN courseNN groupNN projectNN =
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess (repoNN, termNN, courseNN, groupNN, projectNN) x [projectSub, worktrainSub, scriptSub, listSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) noArgs
    where x (Global(
             Project(ProjectOpts(
              ProjectWorktrain(ProjectWorktrainOpts(
               ProjectWorktrainScript(ProjectWorktrainScriptOpts(
                ProjectWorktrainScriptList(ProjectWorktrainScriptListOpts a b c d e))))))))) = (a,b,c,d,e)

prop_projectWorktrainScriptExtractSuccess repoNN termNN courseNN groupNN projectNN di = let ds = validArgs [di] in ds /= [] ==>
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==> let extractDir = head ds in
  testSuccess (repoNN, termNN, courseNN, groupNN, projectNN, extractDir) x [projectSub, worktrainSub, scriptSub, extractSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) [extractDir]
    where x (Global(
             Project(ProjectOpts(
              ProjectWorktrain(ProjectWorktrainOpts(
               ProjectWorktrainScript(ProjectWorktrainScriptOpts(
                ProjectWorktrainScriptExtract(ProjectWorktrainScriptExtractOpts a b c d e f))))))))) = (a,b,c,d,e,f)

prop_projectWorktrainFileAddSuccess repoNN termNN courseNN groupNN projectNN fs = let files = validArgs fs in files /= [] ==>
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess (repoNN, termNN, courseNN, groupNN, projectNN, files) x [projectSub, worktrainSub, fileSub, addSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) files
    where x (Global(
             Project(ProjectOpts(
              ProjectWorktrain(ProjectWorktrainOpts(
               ProjectWorktrainFile(ProjectWorktrainFileOpts(
                ProjectWorktrainFileAdd(ProjectWorktrainFileAddOpts a b c d e f))))))))) = (a,b,c,d,e,f)

prop_projectWorktrainFileRemoveSuccess repoNN termNN courseNN groupNN projectNN fs = let files = validArgs fs in files /= [] ==>
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess (repoNN, termNN, courseNN, groupNN, projectNN, files) x [projectSub, worktrainSub, fileSub, removeSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) files
    where x (Global(
             Project(ProjectOpts(
              ProjectWorktrain(ProjectWorktrainOpts(
               ProjectWorktrainFile(ProjectWorktrainFileOpts(
                ProjectWorktrainFileRemove(ProjectWorktrainFileRemoveOpts a b c d e f))))))))) = (a,b,c,d,e,f)

prop_projectWorktrainFileListSuccess repoNN termNN courseNN groupNN projectNN =
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess (repoNN, termNN, courseNN, groupNN, projectNN) x [projectSub, worktrainSub, fileSub, listSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) noArgs
    where x (Global(
             Project(ProjectOpts(
              ProjectWorktrain(ProjectWorktrainOpts(
               ProjectWorktrainFile(ProjectWorktrainFileOpts(
                ProjectWorktrainFileList(ProjectWorktrainFileListOpts a b c d e))))))))) = (a,b,c,d,e)

prop_projectWorktrainFileExtractSuccess repoNN termNN courseNN groupNN projectNN di fs = let args = validArgs (di:fs) in length args >= 2 ==>
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==> let (dir:files) = args in
  testSuccess (repoNN, termNN, courseNN, groupNN, projectNN, dir, files) x [projectSub, worktrainSub, fileSub, extractSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) (dir:files)
    where x (Global(
             Project(ProjectOpts(
              ProjectWorktrain(ProjectWorktrainOpts(
               ProjectWorktrainFile(ProjectWorktrainFileOpts(
                ProjectWorktrainFileExtract(ProjectWorktrainFileExtractOpts a b c d e f g))))))))) = (a,b,c,d,e,f,g)

prop_projectWorktrainTimeLimitSetSuccess repoNN termNN courseNN groupNN projectNN v = let args = validArgs [v] in args /= [] ==>
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==> let val = head args in
  testSuccess (repoNN, termNN, courseNN, groupNN, projectNN, val) x [projectSub, worktrainSub, timeLimitSub, setSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) [val]
    where x (Global(
             Project(ProjectOpts(
              ProjectWorktrain(ProjectWorktrainOpts(
               ProjectWorktrainTimeLimit(ProjectWorktrainTimeLimitOpts(
                ProjectWorktrainTimeLimitSet(ProjectWorktrainTimeLimitSetOpts a b c d e f))))))))) = (a,b,c,d,e,f)

prop_projectWorktrainTimeLimitListSuccess repoNN termNN courseNN groupNN projectNN =
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess (repoNN, termNN, courseNN, groupNN, projectNN) x [projectSub, worktrainSub, timeLimitSub, listSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) noArgs
    where x (Global(
             Project(ProjectOpts(
              ProjectWorktrain(ProjectWorktrainOpts(
               ProjectWorktrainTimeLimit(ProjectWorktrainTimeLimitOpts(
                ProjectWorktrainTimeLimitList(ProjectWorktrainTimeLimitListOpts a b c d e))))))))) = (a,b,c,d,e)

prop_projectWorktrainSpaceLimitSetSuccess repoNN termNN courseNN groupNN projectNN v = let args = validArgs [v] in args /= [] ==>
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==> let val = head args in
  testSuccess (repoNN, termNN, courseNN, groupNN, projectNN, val) x [projectSub, worktrainSub, spaceLimitSub, setSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) [val]
    where x (Global(
             Project(ProjectOpts(
              ProjectWorktrain(ProjectWorktrainOpts(
               ProjectWorktrainSpaceLimit(ProjectWorktrainSpaceLimitOpts(
                ProjectWorktrainSpaceLimitSet(ProjectWorktrainSpaceLimitSetOpts a b c d e f))))))))) = (a,b,c,d,e,f)

prop_projectWorktrainSpaceLimitListSuccess repoNN termNN courseNN groupNN projectNN =
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess (repoNN, termNN, courseNN, groupNN, projectNN) x [projectSub, worktrainSub, spaceLimitSub, listSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) noArgs
    where x (Global(
             Project(ProjectOpts(
              ProjectWorktrain(ProjectWorktrainOpts(
               ProjectWorktrainSpaceLimit(ProjectWorktrainSpaceLimitOpts(
                ProjectWorktrainSpaceLimitList(ProjectWorktrainSpaceLimitListOpts a b c d e))))))))) = (a,b,c,d,e)

prop_projectSubmitListSuccess repoNN termNN courseNN groupNN projectNN =
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess (repoNN, termNN, courseNN, groupNN, projectNN) x [projectSub, submitSub, listSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) noArgs
    where x (Global(
             Project(ProjectOpts(
              ProjectSubmit(ProjectSubmitOpts(
               ProjectSubmitList(ProjectSubmitListOpts a b c d e))))))) = (a,b,c,d,e)

prop_projectSubmitLateSuccess repoNN termNN courseNN groupNN projectNN =
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess (repoNN, termNN, courseNN, groupNN, projectNN) x [projectSub, submitSub, lateSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) noArgs
    where x (Global(
             Project(ProjectOpts(
              ProjectSubmit(ProjectSubmitOpts(
               ProjectSubmitLate(ProjectSubmitLateOpts a b c d e))))))) = (a,b,c,d,e)
               
prop_projectSubmitInspectSuccess repoNN termNN courseNN groupNN projectNN ks = let keys = validArgs ks in keys /= [] ==>
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess (repoNN, termNN, courseNN, groupNN, projectNN, keys) x [projectSub, submitSub, inspectSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) keys
    where x (Global(
             Project(ProjectOpts(
              ProjectSubmit(ProjectSubmitOpts(
               ProjectSubmitInspect(ProjectSubmitInspectOpts a b c d e f))))))) = (a,b,c,d,e,f)
               
prop_projectSubmitExtractSuccess repoNN termNN courseNN groupNN projectNN di ks = let args = validArgs (di:ks) in length args >= 2 ==>
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==> let (dir:keys) = args in
  testSuccess (repoNN, termNN, courseNN, groupNN, projectNN, dir, keys) x [projectSub, submitSub, extractSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) (dir:keys)
    where x (Global(
             Project(ProjectOpts(
              ProjectSubmit(ProjectSubmitOpts(
               ProjectSubmitExtract(ProjectSubmitExtractOpts a b c d e f g))))))) = (a,b,c,d,e,f,g)
               