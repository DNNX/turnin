{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Interface.CommandLineParser.SubmitWorktrainTest where

import Test.Framework
import Interface.CommandLineParser.ParserTestUtils
 
import Interface.Lexicon 
import Interface.CommandLineParser
import Interface.CommandLineParser.SubmitWorktrain

{-# ANN module "HLint: ignore Use camelCase" #-}

prop_submitSuccess repoNN termNN courseNN groupNN projectNN ns = let names = validArgs ns in names /= [] ==>
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess 4 (repoNN, termNN, courseNN, groupNN, projectNN, names) x [submitSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) names
    where x (Global(
             Submit(SubmitOpts a b c d e f))) = (a,b,c,d,e,f)
              
prop_inspectSuccess repoNN termNN courseNN groupNN projectNN =
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess 4 (repoNN, termNN, courseNN, groupNN, projectNN) x [inspectSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) noArgs
    where x (Global(
             Inspect(InspectOpts a b c d e))) = (a,b,c,d,e)
               
prop_extractSuccess repoNN termNN courseNN groupNN projectNN di = let args = validArgs [di] in args /= [] ==>
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==> let dir = head args in
  testSuccess 4 (repoNN, termNN, courseNN, groupNN, projectNN, dir) x [extractSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) [dir]
    where x (Global(
             Extract(ExtractOpts a b c d e f))) = (a,b,c,d,e,f)
                
prop_worktrainRunSuccess repoNN termNN courseNN groupNN projectNN ks = let keys = validArgs ks in keys /= [] ==>
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess 3 (repoNN, termNN, courseNN, groupNN, projectNN, keys) x [worktrainSub, runSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) keys
    where x (Global(
             Worktrain(WorktrainOpts( 
              WorktrainRun(WorktrainRunOpts a b c d e f))))) = (a,b,c,d,e,f)
          
prop_worktrainDateSuccess repoNN termNN courseNN groupNN projectNN =
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess 3 (repoNN, termNN, courseNN, groupNN, projectNN) x [worktrainSub, dateSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) noArgs
    where x (Global(
             Worktrain(WorktrainOpts(
              WorktrainDate(WorktrainDateOpts a b c d e))))) = (a,b,c,d,e)
               
prop_worktrainListSuccess repoNN termNN courseNN groupNN projectNN dateNN =
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN, dateNN] ==>
  testSuccess 3 (repoNN, termNN, courseNN, groupNN, projectNN, dateNN) x [worktrainSub, listSub]
   (trainRunOpts repoNN termNN courseNN groupNN projectNN dateNN) noArgs
    where x (Global(
             Worktrain(WorktrainOpts( 
              WorktrainList(WorktrainListOpts a b c d e f))))) = (a,b,c,d,e,f)           
          
prop_worktrainExtractSuccess repoNN termNN courseNN groupNN projectNN dateNN di ks = let args = validArgs (di:ks) in length args >= 2 ==>
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN, dateNN] ==> let (dir:keys) = args in
  testSuccess 3 (repoNN, termNN, courseNN, groupNN, projectNN, dateNN, dir, keys) x [worktrainSub, extractSub]
   (trainRunOpts repoNN termNN courseNN groupNN projectNN dateNN) (dir:keys)
    where x (Global(
             Worktrain(WorktrainOpts(
              WorktrainExtract(WorktrainExtractOpts a b c d e f g h))))) = (a,b,c,d,e,f,g,h)           
           
prop_worktrainOutputSuccess repoNN termNN courseNN groupNN projectNN dateNN di ks m = let args = validArgs (di:ks) in length args >= 2 ==>
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN, dateNN] ==> let (dir:keys) = args in
  testSuccess 3 (repoNN, termNN, courseNN, groupNN, projectNN, dateNN, m, dir, keys) x [worktrainSub, outputSub]
   (trainRunOutputOpts repoNN termNN courseNN groupNN projectNN dateNN m mergeFlag) (dir:keys)
    where x (Global(  
             Worktrain(WorktrainOpts(
              WorktrainOutput(WorktrainOutputOpts a b c d e f g h i))))) = (a,b,c,d,e,f,g,h,i)           
          