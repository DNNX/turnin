{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Interface.CommandLineParser.ProjectValidatePropTest where
import Test.Framework
import Interface.CommandLineParser.ParserTestUtils

import Interface.Lexicon
import Interface.CommandLineParser
import Interface.CommandLineParser.Project
import Interface.CommandLineParser.Project.Validate

{-# ANN module "HLint: ignore Use camelCase" #-}

prop_projectAddSuccess repoNN termNN courseNN groupNN start end late n = let ns = validArgs [n]; [name] = ns in ns /= [] ==>
 validOpts [repoNN, termNN, courseNN, groupNN, start, end, late] ==>
  testSuccess 2 (repoNN, termNN, courseNN, groupNN, start, end, late, name) addF
   [projectSub, addSub] (projectAddOpts repoNN termNN courseNN groupNN start end late) [name]

prop_projectRemoveSuccess repoNN termNN courseNN groupNN projectNN =
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess 2 (repoNN, termNN, courseNN, groupNN, projectNN) removeF
   [projectSub, removeSub] (projectOpts repoNN termNN courseNN groupNN projectNN) noArgs

prop_projectListSuccess repoNN termNN courseNN groupNN =
 validOpts [repoNN, termNN, courseNN, groupNN] ==>
  testSuccess 3 (repoNN, termNN, courseNN, groupNN) listF
   [projectSub, listSub] (groupOpts repoNN termNN courseNN groupNN) noArgs

prop_projectDateSetSuccess repoNN termNN courseNN groupNN projectNN start end late =
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN, start, end, late] ==>
  testSuccess 2 (repoNN, termNN, courseNN, groupNN, projectNN, start, end, late) dateSetF [projectSub, dateSub, setSub]
  (projectDateOpts repoNN termNN courseNN groupNN projectNN start end late) noArgs

prop_projectDateListSuccess repoNN termNN courseNN groupNN projectNN =
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess 3 (repoNN, termNN, courseNN, groupNN, projectNN) dateListF [projectSub, dateSub, listSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) noArgs

prop_projectValidateAcceptExecSetSuccess repoNN termNN courseNN groupNN projectNN v = let val = ["yYnN" !! (v `mod` 4)] in
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess 2 (repoNN, termNN, courseNN, groupNN, projectNN, val) validateAcceptExecSetF [projectSub, validateSub, acceptExecSub, setSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) [val]

prop_projectValidateAcceptExecListSuccess repoNN termNN courseNN groupNN projectNN =
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess 3 (repoNN, termNN, courseNN, groupNN, projectNN) validateAcceptExecListF [projectSub, validateSub, acceptExecSub, listSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) noArgs

prop_projectValidateNameAddSuccess repoNN termNN courseNN groupNN projectNN ns = let names = validArgs ns in names /= [] ==>
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess 2 (repoNN, termNN, courseNN, groupNN, projectNN, names) validateNameAddF [projectSub, validateSub, nameSub, addSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) names

prop_projectValidateNameRemoveSuccess repoNN termNN courseNN groupNN projectNN ns = let names = validArgs ns in names /= [] ==>
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess 2 (repoNN, termNN, courseNN, groupNN, projectNN, names) validateNameRemoveF [projectSub, validateSub, nameSub, removeSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) names

prop_projectValidateNameListSuccess repoNN termNN courseNN groupNN projectNN =
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess 3 (repoNN, termNN, courseNN, groupNN, projectNN) validateNameListF [projectSub, validateSub, nameSub, listSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) noArgs

prop_projectValidateCommandSetSuccess repoNN termNN courseNN groupNN projectNN co = let cs = validArgs [co]; [command] = cs in cs /= [] ==>
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess 2 (repoNN, termNN, courseNN, groupNN, projectNN, command) validateCommandSetF [projectSub, validateSub, commandSub, setSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) [command]

prop_projectValidateCommandUnsetSuccess repoNN termNN courseNN groupNN projectNN =
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess 2 (repoNN, termNN, courseNN, groupNN, projectNN) validateCommandUnsetF [projectSub, validateSub, commandSub, unsetSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) noArgs

prop_projectValidateCommandListSuccess repoNN termNN courseNN groupNN projectNN =
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess 3 (repoNN, termNN, courseNN, groupNN, projectNN) validateCommandListF [projectSub, validateSub, commandSub, listSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) noArgs

prop_projectValidateScriptSetSuccess repoNN termNN courseNN groupNN projectNN s = let ss = validArgs [s]; [scriptName] = ss in ss /= [] ==>
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess 2 (repoNN, termNN, courseNN, groupNN, projectNN, scriptName) validateScriptSet [projectSub, validateSub, scriptSub, setSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) [scriptName]

prop_projectValidateScriptUnsetSuccess repoNN termNN courseNN groupNN projectNN =
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess 2 (repoNN, termNN, courseNN, groupNN, projectNN) validateScriptUnsetF [projectSub, validateSub, scriptSub, unsetSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) noArgs

prop_projectValidateScriptListSuccess repoNN termNN courseNN groupNN projectNN =
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess 3 (repoNN, termNN, courseNN, groupNN, projectNN) validateScriptListF [projectSub, validateSub, scriptSub, listSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) noArgs

prop_projectValidateScriptExtractSuccess repoNN termNN courseNN groupNN projectNN di = let ds = validArgs [di]; [extractDir] = ds in ds /= [] ==>
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess 3 (repoNN, termNN, courseNN, groupNN, projectNN, extractDir) validateScriptExtractF [projectSub, validateSub, scriptSub, extractSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) [extractDir]

addF (Global(Project(ProjectOpts(ProjectAdd(ProjectAddOpts a b c d e f g h))))) = (a,b,c,d,e,f,g,h)
removeF (Global(Project(ProjectOpts(ProjectRemove(ProjectRemoveOpts a b c d e))))) = (a,b,c,d,e)
listF (Global(Project(ProjectOpts(ProjectList(ProjectListOpts a b c d))))) = (a,b,c,d)
dateSetF (Global(Project(ProjectOpts(ProjectDate(ProjectDateOpts(ProjectDateSet(ProjectDateSetOpts a b c d e f g h))))))) = (a,b,c,d,e,f,g,h)
dateListF (Global(Project(ProjectOpts(ProjectDate(ProjectDateOpts(ProjectDateList(ProjectDateListOpts a b c d e))))))) = (a,b,c,d,e)
validateAcceptExecSetF (Global(Project(ProjectOpts(ProjectValidate(ProjectValidateOpts(ProjectValidateAcceptExec(ProjectValidateAcceptExecOpts(ProjectValidateAcceptExecSet(ProjectValidateAcceptExecSetOpts a b c d e f))))))))) = (a,b,c,d,e,f)
validateAcceptExecListF (Global(Project(ProjectOpts(ProjectValidate(ProjectValidateOpts(ProjectValidateAcceptExec(ProjectValidateAcceptExecOpts(ProjectValidateAcceptExecList(ProjectValidateAcceptExecListOpts a b c d e))))))))) = (a,b,c,d,e)
validateNameAddF (Global(Project(ProjectOpts(ProjectValidate(ProjectValidateOpts(ProjectValidateName(ProjectValidateNameOpts(ProjectValidateNameAdd(ProjectValidateNameAddOpts a b c d e f))))))))) = (a,b,c,d,e,f)
validateNameRemoveF (Global(Project(ProjectOpts(ProjectValidate(ProjectValidateOpts(ProjectValidateName(ProjectValidateNameOpts(ProjectValidateNameRemove(ProjectValidateNameRemoveOpts a b c d e f))))))))) = (a,b,c,d,e,f)
validateNameListF (Global(Project(ProjectOpts(ProjectValidate(ProjectValidateOpts(ProjectValidateName(ProjectValidateNameOpts(ProjectValidateNameList(ProjectValidateNameListOpts a b c d e))))))))) = (a,b,c,d,e)
validateCommandSetF (Global(Project(ProjectOpts(ProjectValidate(ProjectValidateOpts(ProjectValidateCommand(ProjectValidateCommandOpts(ProjectValidateCommandSet(ProjectValidateCommandSetOpts a b c d e f))))))))) = (a,b,c,d,e,f)
validateCommandUnsetF (Global(Project(ProjectOpts(ProjectValidate(ProjectValidateOpts(ProjectValidateCommand(ProjectValidateCommandOpts(ProjectValidateCommandUnset(ProjectValidateCommandUnsetOpts a b c d e))))))))) = (a,b,c,d,e)
validateCommandListF (Global(Project(ProjectOpts(ProjectValidate(ProjectValidateOpts(ProjectValidateCommand(ProjectValidateCommandOpts(ProjectValidateCommandList(ProjectValidateCommandListOpts a b c d e))))))))) = (a,b,c,d,e)
validateScriptSet (Global(Project(ProjectOpts(ProjectValidate(ProjectValidateOpts(ProjectValidateScript(ProjectValidateScriptOpts(ProjectValidateScriptSet(ProjectValidateScriptSetOpts a b c d e f))))))))) = (a,b,c,d,e,f)
validateScriptUnsetF (Global(Project(ProjectOpts(ProjectValidate(ProjectValidateOpts(ProjectValidateScript(ProjectValidateScriptOpts(ProjectValidateScriptUnset(ProjectValidateScriptUnsetOpts a b c d e))))))))) = (a,b,c,d,e)
validateScriptListF (Global(Project(ProjectOpts(ProjectValidate(ProjectValidateOpts(ProjectValidateScript(ProjectValidateScriptOpts(ProjectValidateScriptList(ProjectValidateScriptListOpts a b c d e))))))))) = (a,b,c,d,e)
validateScriptExtractF (Global(Project(ProjectOpts(ProjectValidate(ProjectValidateOpts(ProjectValidateScript(ProjectValidateScriptOpts(ProjectValidateScriptExtract(ProjectValidateScriptExtractOpts a b c d e f))))))))) = (a,b,c,d,e,f)

                