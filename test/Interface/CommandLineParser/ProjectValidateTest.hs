{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Interface.CommandLineParser.ProjectValidateTest where
import Test.Framework
import Interface.CommandLineParser.ParserTestUtils

import Interface.Lexicon
import Interface.CommandLineParser
import Interface.CommandLineParser.Project
import Interface.CommandLineParser.Project.Validate

{-# ANN module "HLint: ignore Use camelCase" #-}

prop_projectAddSuccess repoNN termNN courseNN groupNN start end late n = let ns = validArgs [n]; [name] = ns in ns /= [] ==>
 validOpts [repoNN, termNN, courseNN, groupNN, start, end, late] ==>
  testSuccess 2 (repoNN, termNN, courseNN, groupNN, start, end, late, name) projectAddF
   [projectSub, addSub] (projectAddOpts repoNN termNN courseNN groupNN start end late) [name]
   where projectAddF (Global(
            Project(ProjectOpts(
             ProjectAdd(ProjectAddOpts a b c d e f g h))))) = (a,b,c,d,e,f,g,h)

prop_projectRemoveSuccess repoNN termNN courseNN groupNN projectNN =
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess 2 (repoNN, termNN, courseNN, groupNN, projectNN) x
   [projectSub, removeSub] (projectOpts repoNN termNN courseNN groupNN projectNN) noArgs
    where x (Global(
             Project(ProjectOpts(
              ProjectRemove(ProjectRemoveOpts a b c d e))))) = (a,b,c,d,e)

prop_projectListSuccess repoNN termNN courseNN groupNN =
 validOpts [repoNN, termNN, courseNN, groupNN] ==>
  testSuccess 3 (repoNN, termNN, courseNN, groupNN) x
   [projectSub, listSub] (groupOpts repoNN termNN courseNN groupNN) noArgs
    where x (Global(
             Project(ProjectOpts(
              ProjectList(ProjectListOpts a b c d))))) = (a,b,c,d)

prop_projectDateSetSuccess repoNN termNN courseNN groupNN projectNN start end late =
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN, start, end, late] ==>
  testSuccess 2 (repoNN, termNN, courseNN, groupNN, projectNN, start, end, late) x [projectSub, dateSub, setSub]
  (projectDateOpts repoNN termNN courseNN groupNN projectNN start end late) noArgs
    where x (Global(
             Project(ProjectOpts(
              ProjectDate(ProjectDateOpts(
               ProjectDateSet(ProjectDateSetOpts a b c d e f g h))))))) = (a,b,c,d,e,f,g,h)

prop_projectDateListSuccess repoNN termNN courseNN groupNN projectNN =
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess 3 (repoNN, termNN, courseNN, groupNN, projectNN) x [projectSub, dateSub, listSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) noArgs
    where x (Global(
             Project(ProjectOpts(
              ProjectDate(ProjectDateOpts(
               ProjectDateList(ProjectDateListOpts a b c d e))))))) = (a,b,c,d,e)


prop_projectValidateAcceptExecSetSuccess repoNN termNN courseNN groupNN projectNN v = let val = ["yYnN" !! (v `mod` 4)] in
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess 2 (repoNN, termNN, courseNN, groupNN, projectNN, val) x [projectSub, validateSub, acceptExecSub, setSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) [val]
    where x (Global(
             Project(ProjectOpts(
              ProjectValidate(ProjectValidateOpts(
               ProjectValidateAcceptExec(ProjectValidateAcceptExecOpts(
                ProjectValidateAcceptExecSet(ProjectValidateAcceptExecSetOpts a b c d e f))))))))) = (a,b,c,d,e,f)

prop_projectValidateAcceptExecListSuccess repoNN termNN courseNN groupNN projectNN =
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess 3 (repoNN, termNN, courseNN, groupNN, projectNN) x [projectSub, validateSub, acceptExecSub, listSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) noArgs
    where x (Global(
             Project(ProjectOpts(
              ProjectValidate(ProjectValidateOpts(
               ProjectValidateAcceptExec(ProjectValidateAcceptExecOpts(
                ProjectValidateAcceptExecList(ProjectValidateAcceptExecListOpts a b c d e))))))))) = (a,b,c,d,e)

prop_projectValidateNameAddSuccess repoNN termNN courseNN groupNN projectNN ns = let names = validArgs ns in names /= [] ==>
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess 2 (repoNN, termNN, courseNN, groupNN, projectNN, names) x [projectSub, validateSub, nameSub, addSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) names
    where x (Global(
             Project(ProjectOpts(
              ProjectValidate(ProjectValidateOpts(
               ProjectValidateName(ProjectValidateNameOpts(
                ProjectValidateNameAdd(ProjectValidateNameAddOpts a b c d e f))))))))) = (a,b,c,d,e,f)


prop_projectValidateNameRemoveSuccess repoNN termNN courseNN groupNN projectNN ns = let names = validArgs ns in names /= [] ==>
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess 2 (repoNN, termNN, courseNN, groupNN, projectNN, names) x [projectSub, validateSub, nameSub, removeSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) names
    where x (Global(
             Project(ProjectOpts(
              ProjectValidate(ProjectValidateOpts(
               ProjectValidateName(ProjectValidateNameOpts(
                ProjectValidateNameRemove(ProjectValidateNameRemoveOpts a b c d e f))))))))) = (a,b,c,d,e,f)


prop_projectValidateNameListSuccess repoNN termNN courseNN groupNN projectNN =
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess 3 (repoNN, termNN, courseNN, groupNN, projectNN) x [projectSub, validateSub, nameSub, listSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) noArgs
    where x (Global(
             Project(ProjectOpts(
              ProjectValidate(ProjectValidateOpts(
               ProjectValidateName(ProjectValidateNameOpts(
                ProjectValidateNameList(ProjectValidateNameListOpts a b c d e))))))))) = (a,b,c,d,e)


prop_projectValidateCommandSetSuccess repoNN termNN courseNN groupNN projectNN co = let cs = validArgs [co]; [command] = cs in cs /= [] ==>
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess 2 (repoNN, termNN, courseNN, groupNN, projectNN, command) x [projectSub, validateSub, commandSub, setSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) [command]
    where x (Global(
             Project(ProjectOpts(
              ProjectValidate(ProjectValidateOpts(
               ProjectValidateCommand(ProjectValidateCommandOpts(
                ProjectValidateCommandSet(ProjectValidateCommandSetOpts a b c d e f))))))))) = (a,b,c,d,e,f)


prop_projectValidateCommandUnsetSuccess repoNN termNN courseNN groupNN projectNN =
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess 2 (repoNN, termNN, courseNN, groupNN, projectNN) x [projectSub, validateSub, commandSub, unsetSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) noArgs
    where x (Global(
             Project(ProjectOpts(
              ProjectValidate(ProjectValidateOpts(
               ProjectValidateCommand(ProjectValidateCommandOpts(
                ProjectValidateCommandUnset(ProjectValidateCommandUnsetOpts a b c d e))))))))) = (a,b,c,d,e)


prop_projectValidateCommandListSuccess repoNN termNN courseNN groupNN projectNN =
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess 3 (repoNN, termNN, courseNN, groupNN, projectNN) x [projectSub, validateSub, commandSub, listSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) noArgs
    where x (Global(
             Project(ProjectOpts(
              ProjectValidate(ProjectValidateOpts(
               ProjectValidateCommand(ProjectValidateCommandOpts(
                ProjectValidateCommandList(ProjectValidateCommandListOpts a b c d e))))))))) = (a,b,c,d,e)


prop_projectValidateScriptSetSuccess repoNN termNN courseNN groupNN projectNN s = let ss = validArgs [s]; [scriptName] = ss in ss /= [] ==>
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess 2 (repoNN, termNN, courseNN, groupNN, projectNN, scriptName) x [projectSub, validateSub, scriptSub, setSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) [scriptName]
    where x (Global(
             Project(ProjectOpts(
              ProjectValidate(ProjectValidateOpts(
               ProjectValidateScript(ProjectValidateScriptOpts(
                ProjectValidateScriptSet(ProjectValidateScriptSetOpts a b c d e f))))))))) = (a,b,c,d,e,f)


prop_projectValidateScriptUnsetSuccess repoNN termNN courseNN groupNN projectNN =
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess 2 (repoNN, termNN, courseNN, groupNN, projectNN) x [projectSub, validateSub, scriptSub, unsetSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) noArgs
    where x (Global(
             Project(ProjectOpts(
              ProjectValidate(ProjectValidateOpts(
               ProjectValidateScript(ProjectValidateScriptOpts(
                ProjectValidateScriptUnset(ProjectValidateScriptUnsetOpts a b c d e))))))))) = (a,b,c,d,e)


prop_projectValidateScriptListSuccess repoNN termNN courseNN groupNN projectNN =
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess 3 (repoNN, termNN, courseNN, groupNN, projectNN) x [projectSub, validateSub, scriptSub, listSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) noArgs
    where x (Global(
             Project(ProjectOpts(
              ProjectValidate(ProjectValidateOpts(
               ProjectValidateScript(ProjectValidateScriptOpts(
                ProjectValidateScriptList(ProjectValidateScriptListOpts a b c d e))))))))) = (a,b,c,d,e)


prop_projectValidateScriptExtractSuccess repoNN termNN courseNN groupNN projectNN di = let ds = validArgs [di]; [extractDir] = ds in ds /= [] ==>
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess 3 (repoNN, termNN, courseNN, groupNN, projectNN, extractDir) x [projectSub, validateSub, scriptSub, extractSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) [extractDir]
    where x (Global(
             Project(ProjectOpts(
              ProjectValidate(ProjectValidateOpts(
               ProjectValidateScript(ProjectValidateScriptOpts(
                ProjectValidateScriptExtract(ProjectValidateScriptExtractOpts a b c d e f))))))))) = (a,b,c,d,e,f)


          