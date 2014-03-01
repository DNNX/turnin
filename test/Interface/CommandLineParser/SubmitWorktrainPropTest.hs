{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Interface.CommandLineParser.SubmitWorktrainPropTest where

import Test.Framework
import Interface.CommandLineParser.ParserTestUtils

import Interface.Lexicon
import Interface.CommandLineParser
import Interface.CommandLineParser.SubmitWorktrain

{-# ANN module "HLint: ignore Use camelCase" #-}

prop_submitSuccess repoNN termNN courseNN groupNN projectNN ns = let names = validArgs ns in names /= [] ==>
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess 4 (repoNN, termNN, courseNN, groupNN, projectNN, names) submitF [submitSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) names

prop_inspectSuccess repoNN termNN courseNN groupNN projectNN =
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess 4 (repoNN, termNN, courseNN, groupNN, projectNN) inspectF [inspectSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) noArgs

prop_extractSuccess repoNN termNN courseNN groupNN projectNN di = let args = validArgs [di] in args /= [] ==>
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==> let dir = head args in
  testSuccess 4 (repoNN, termNN, courseNN, groupNN, projectNN, dir) extractF [extractSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) [dir]

prop_worktrainRunSuccess repoNN termNN courseNN groupNN projectNN ks = let keys = validArgs ks in keys /= [] ==>
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess 3 (repoNN, termNN, courseNN, groupNN, projectNN, keys) worktrainRunF [worktrainSub, runSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) keys

prop_worktrainDateSuccess repoNN termNN courseNN groupNN projectNN =
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess 3 (repoNN, termNN, courseNN, groupNN, projectNN) worktrainDateF [worktrainSub, dateSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) noArgs

prop_worktrainListSuccess repoNN termNN courseNN groupNN projectNN dateNN =
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN, dateNN] ==>
  testSuccess 3 (repoNN, termNN, courseNN, groupNN, projectNN, dateNN) worktrainListF [worktrainSub, listSub]
   (trainRunOpts repoNN termNN courseNN groupNN projectNN dateNN) noArgs

prop_worktrainExtractSuccess repoNN termNN courseNN groupNN projectNN dateNN di ks = let args = validArgs (di:ks) in length args >= 2 ==>
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN, dateNN] ==> let (dir:keys) = args in
  testSuccess 3 (repoNN, termNN, courseNN, groupNN, projectNN, dateNN, dir, keys) worktrainExtractF [worktrainSub, extractSub]
   (trainRunOpts repoNN termNN courseNN groupNN projectNN dateNN) (dir:keys)

prop_worktrainOutputSuccess repoNN termNN courseNN groupNN projectNN dateNN di ks m = let args = validArgs (di:ks) in length args >= 2 ==>
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN, dateNN] ==> let (dir:keys) = args in
  testSuccess 3 (repoNN, termNN, courseNN, groupNN, projectNN, dateNN, dir, keys, m) worktrainOutputF [worktrainSub, outputSub]
   (trainRunOutputOpts repoNN termNN courseNN groupNN projectNN dateNN m mergeFlag) (dir:keys)

submitF (Global(Submit(SubmitOpts a b c d e f))) = (a,b,c,d,e,f)
inspectF (Global(Inspect(InspectOpts a b c d e))) = (a,b,c,d,e)
extractF (Global(Extract(ExtractOpts a b c d e f))) = (a,b,c,d,e,f)
worktrainRunF (Global(Worktrain(WorktrainOpts(WorktrainRun(WorktrainRunOpts a b c d e f))))) = (a,b,c,d,e,f)
worktrainDateF (Global(Worktrain(WorktrainOpts(WorktrainDate(WorktrainDateOpts a b c d e))))) = (a,b,c,d,e)
worktrainListF (Global(Worktrain(WorktrainOpts(WorktrainList(WorktrainListOpts a b c d e f))))) = (a,b,c,d,e,f)
worktrainExtractF (Global(Worktrain(WorktrainOpts(WorktrainExtract(WorktrainExtractOpts a b c d e f g h))))) = (a,b,c,d,e,f,g,h)
worktrainOutputF (Global(Worktrain(WorktrainOpts(WorktrainOutput(WorktrainOutputOpts a b c d e f g h i))))) = (a,b,c,d,e,f,g,h,i)
          