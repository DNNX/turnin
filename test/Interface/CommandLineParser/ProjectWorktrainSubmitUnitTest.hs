{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Interface.CommandLineParser.ProjectWorktrainSubmitUnitTest where

import Test.Framework
import Interface.CommandLineParser.ParserTestUtils

import Interface.Lexicon
import Interface.CommandLineParser
import Interface.CommandLineParser.Project
import Interface.CommandLineParser.Project.Worktrain
import Interface.CommandLineParser.Project.Submit

{-# ANN module "HLint: ignore Use camelCase" #-}
{-
prop_projectWorktrainScriptSetSuccess repoNN termNN courseNN groupNN projectNN s = let ss = validArgs [s] in ss /= [] ==>
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==> let scriptName = head ss in
  testSuccess 2 (repoNN, termNN, courseNN, groupNN, projectNN, scriptName) worktrainScriptSetF [projectSub, worktrainSub, scriptSub, setSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) [scriptName]

prop_projectWorktrainScriptUnsetSuccess repoNN termNN courseNN groupNN projectNN =
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess 2 (repoNN, termNN, courseNN, groupNN, projectNN)worktrainScriptUnsetF [projectSub, worktrainSub, scriptSub, unsetSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) noArgs

prop_projectWorktrainScriptListSuccess repoNN termNN courseNN groupNN projectNN =
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess 3 (repoNN, termNN, courseNN, groupNN, projectNN) worktrainScriptListF [projectSub, worktrainSub, scriptSub, listSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) noArgs

prop_projectWorktrainScriptExtractSuccess repoNN termNN courseNN groupNN projectNN di = let ds = validArgs [di] in ds /= [] ==>
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==> let extractDir = head ds in
  testSuccess 3 (repoNN, termNN, courseNN, groupNN, projectNN, extractDir) worktrainScriptExtractF [projectSub, worktrainSub, scriptSub, extractSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) [extractDir]

prop_projectWorktrainFileAddSuccess repoNN termNN courseNN groupNN projectNN fs = let files = validArgs fs in files /= [] ==>
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess 2 (repoNN, termNN, courseNN, groupNN, projectNN, files) worktrainFileAddF [projectSub, worktrainSub, fileSub, addSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) files

prop_projectWorktrainFileRemoveSuccess repoNN termNN courseNN groupNN projectNN fs = let files = validArgs fs in files /= [] ==>
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess 2 (repoNN, termNN, courseNN, groupNN, projectNN, files) worktrainFileRemoveF [projectSub, worktrainSub, fileSub, removeSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) files

prop_projectWorktrainFileListSuccess repoNN termNN courseNN groupNN projectNN =
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess 3 (repoNN, termNN, courseNN, groupNN, projectNN) worktrainFileListSuccessF [projectSub, worktrainSub, fileSub, listSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) noArgs

prop_projectWorktrainFileExtractSuccess repoNN termNN courseNN groupNN projectNN di fs = let args = validArgs (di:fs) in length args >= 2 ==>
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==> let (dir:files) = args in
  testSuccess 3 (repoNN, termNN, courseNN, groupNN, projectNN, dir, files) worktrainFileExtractSuccessF [projectSub, worktrainSub, fileSub, extractSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) (dir:files)

prop_projectWorktrainTimeLimitSetSuccess repoNN termNN courseNN groupNN projectNN v = let args = validArgs [v] in args /= [] ==>
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==> let val = head args in
  testSuccess 2 (repoNN, termNN, courseNN, groupNN, projectNN, val) worktrainTimeLimitSetF [projectSub, worktrainSub, timeLimitSub, setSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) [val]

prop_projectWorktrainTimeLimitListSuccess repoNN termNN courseNN groupNN projectNN =
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess 3 (repoNN, termNN, courseNN, groupNN, projectNN) worktrainTimeLimitListF [projectSub, worktrainSub, timeLimitSub, listSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) noArgs

prop_projectWorktrainSpaceLimitSetSuccess repoNN termNN courseNN groupNN projectNN v = let args = validArgs [v] in args /= [] ==>
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==> let val = head args in
  testSuccess 2  (repoNN, termNN, courseNN, groupNN, projectNN, val) worktrainSpaceLimitSetF [projectSub, worktrainSub, spaceLimitSub, setSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) [val]

prop_projectWorktrainSpaceLimitListSuccess repoNN termNN courseNN groupNN projectNN =
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess 3 (repoNN, termNN, courseNN, groupNN, projectNN) worktrainSpaceLimitListF [projectSub, worktrainSub, spaceLimitSub, listSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) noArgs
   
prop_projectSubmitListSuccess repoNN termNN courseNN groupNN projectNN =
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess 3 (repoNN, termNN, courseNN, groupNN, projectNN) submitListF [projectSub, submitSub, listSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) noArgs

prop_projectSubmitLateSuccess repoNN termNN courseNN groupNN projectNN =
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess 3 (repoNN, termNN, courseNN, groupNN, projectNN) submitLateF [projectSub, submitSub, lateSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) noArgs
               
prop_projectSubmitInspectSuccess repoNN termNN courseNN groupNN projectNN ks = let keys = validArgs ks in keys /= [] ==>
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess 3 (repoNN, termNN, courseNN, groupNN, projectNN, keys) submitInspectF [projectSub, submitSub, inspectSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) keys
prop_projectSubmitExtractSuccess repoNN termNN courseNN groupNN projectNN di ks = let args = validArgs (di:ks) in length args >= 2 ==>
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==> let (dir:keys) = args in
  testSuccess 3 (repoNN, termNN, courseNN, groupNN, projectNN, dir, keys) submitExtractF [projectSub, submitSub, extractSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) (dir:keys)
               
               
worktrainScriptSetF (Global(Project(ProjectOpts(ProjectWorktrain(ProjectWorktrainOpts(ProjectWorktrainScript(ProjectWorktrainScriptOpts(ProjectWorktrainScriptSet(ProjectWorktrainScriptSetOpts a b c d e f))))))))) = (a,b,c,d,e,f)
worktrainScriptUnsetF (Global(Project(ProjectOpts(ProjectWorktrain(ProjectWorktrainOpts(ProjectWorktrainScript(ProjectWorktrainScriptOpts(ProjectWorktrainScriptUnset(ProjectWorktrainScriptUnsetOpts a b c d e))))))))) = (a,b,c,d,e)
worktrainScriptListF (Global(Project(ProjectOpts(ProjectWorktrain(ProjectWorktrainOpts(ProjectWorktrainScript(ProjectWorktrainScriptOpts(ProjectWorktrainScriptList(ProjectWorktrainScriptListOpts a b c d e))))))))) = (a,b,c,d,e)                
worktrainScriptExtractF (Global(Project(ProjectOpts(ProjectWorktrain(ProjectWorktrainOpts(ProjectWorktrainScript(ProjectWorktrainScriptOpts(ProjectWorktrainScriptExtract(ProjectWorktrainScriptExtractOpts a b c d e f))))))))) = (a,b,c,d,e,f)
worktrainFileAddF (Global(Project(ProjectOpts(ProjectWorktrain(ProjectWorktrainOpts(ProjectWorktrainFile(ProjectWorktrainFileOpts(ProjectWorktrainFileAdd(ProjectWorktrainFileAddOpts a b c d e f))))))))) = (a,b,c,d,e,f)
worktrainFileRemoveF (Global(Project(ProjectOpts(ProjectWorktrain(ProjectWorktrainOpts(ProjectWorktrainFile(ProjectWorktrainFileOpts(ProjectWorktrainFileRemove(ProjectWorktrainFileRemoveOpts a b c d e f))))))))) = (a,b,c,d,e,f)
worktrainFileListSuccessF (Global(Project(ProjectOpts(ProjectWorktrain(ProjectWorktrainOpts(ProjectWorktrainFile(ProjectWorktrainFileOpts(ProjectWorktrainFileList(ProjectWorktrainFileListOpts a b c d e))))))))) = (a,b,c,d,e)
worktrainFileExtractSuccessF (Global(Project(ProjectOpts(ProjectWorktrain(ProjectWorktrainOpts(ProjectWorktrainFile(ProjectWorktrainFileOpts(ProjectWorktrainFileExtract(ProjectWorktrainFileExtractOpts a b c d e f g))))))))) = (a,b,c,d,e,f,g)
worktrainTimeLimitSetF (Global(Project(ProjectOpts(ProjectWorktrain(ProjectWorktrainOpts(ProjectWorktrainTimeLimit(ProjectWorktrainTimeLimitOpts(ProjectWorktrainTimeLimitSet(ProjectWorktrainTimeLimitSetOpts a b c d e f))))))))) = (a,b,c,d,e,f)
worktrainTimeLimitListF (Global(Project(ProjectOpts(ProjectWorktrain(ProjectWorktrainOpts(ProjectWorktrainTimeLimit(ProjectWorktrainTimeLimitOpts(ProjectWorktrainTimeLimitList(ProjectWorktrainTimeLimitListOpts a b c d e))))))))) = (a,b,c,d,e)
worktrainSpaceLimitSetF (Global(Project(ProjectOpts(ProjectWorktrain(ProjectWorktrainOpts(ProjectWorktrainSpaceLimit(ProjectWorktrainSpaceLimitOpts(ProjectWorktrainSpaceLimitSet(ProjectWorktrainSpaceLimitSetOpts a b c d e f))))))))) = (a,b,c,d,e,f)
worktrainSpaceLimitListF (Global(Project(ProjectOpts(ProjectWorktrain(ProjectWorktrainOpts(ProjectWorktrainSpaceLimit(ProjectWorktrainSpaceLimitOpts(ProjectWorktrainSpaceLimitList(ProjectWorktrainSpaceLimitListOpts a b c d e))))))))) = (a,b,c,d,e)
submitListF (Global(Project(ProjectOpts(ProjectSubmit(ProjectSubmitOpts(ProjectSubmitList(ProjectSubmitListOpts a b c d e))))))) = (a,b,c,d,e)
submitLateF (Global(Project(ProjectOpts(ProjectSubmit(ProjectSubmitOpts(ProjectSubmitLate(ProjectSubmitLateOpts a b c d e))))))) = (a,b,c,d,e)
submitInspectF (Global(Project(ProjectOpts(ProjectSubmit(ProjectSubmitOpts(ProjectSubmitInspect(ProjectSubmitInspectOpts a b c d e f))))))) = (a,b,c,d,e,f)              
submitExtractF (Global(Project(ProjectOpts(ProjectSubmit(ProjectSubmitOpts(ProjectSubmitExtract(ProjectSubmitExtractOpts a b c d e f g))))))) = (a,b,c,d,e,f,g)
                
                
                -}