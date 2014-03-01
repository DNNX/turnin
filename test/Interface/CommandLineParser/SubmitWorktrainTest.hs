{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Interface.CommandLineParser.SubmitWorktrainTest where

import Test.Framework
import Interface.CommandLineParser.ParserTestUtils
import Options.Applicative
import Control.Monad (forM_)
import Security.SecurityManager (adminRole, teacherRole, correctorRole, studentRole)
 
import Interface.Lexicon 
import Interface.CommandLineParser
import Interface.CommandLineParser.SubmitWorktrain

{-# ANN module "HLint: ignore Use camelCase" #-}

test_submit = forM_ allRoles  $ \role -> do 
   assertEqual Nothing $ execParserMaybe (globalInfo role) ["submit"]
   assertEqual (Just(Global(Submit(SubmitOpts o o o o o ["key.."])))) $ execParserMaybe (globalInfo role) ["submit", "key.."]

prop_submitSuccess repoNN termNN courseNN groupNN projectNN ns = let names = validArgs ns in names /= [] ==>
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess 4 (repoNN, termNN, courseNN, groupNN, projectNN, names) submitF [submitSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) names
   
test_inspec = forM_ allRoles  $ \role -> do 
   assertEqual (Just(Global(Inspect(InspectOpts o o o o o)))) $ execParserMaybe (globalInfo role) ["inspect"]
   assertEqual Nothing $ execParserMaybe (globalInfo role) ["inspect", "v"]
 
prop_inspectSuccess repoNN termNN courseNN groupNN projectNN =
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess 4 (repoNN, termNN, courseNN, groupNN, projectNN) inspectF [inspectSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) noArgs
      
test_extract = forM_ allRoles  $ \role -> do 
   assertEqual Nothing $ execParserMaybe (globalInfo role) ["extract"]
   assertEqual (Just(Global(Extract(ExtractOpts o o o o o "dir")))) $ execParserMaybe (globalInfo role) ["extract", "dir"]
   assertEqual Nothing $ execParserMaybe (globalInfo role) ["extract", "dir", "v"]    
               
prop_extractSuccess repoNN termNN courseNN groupNN projectNN di = let args = validArgs [di] in args /= [] ==>
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==> let dir = head args in
  testSuccess 4 (repoNN, termNN, courseNN, groupNN, projectNN, dir) extractF [extractSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) [dir]
      
test_worktrain = forM_ allRoles $ \role -> assertEqual Nothing $ execParserMaybe (globalInfo role) ["worktrain"]      
      
test_worktrainRun = do
 forM_ [adminRole, teacherRole, correctorRole] $ \role -> do
   assertEqual Nothing $ execParserMaybe (globalInfo role) ["worktrain", "run"]
   assertEqual (Just(Global(Worktrain(WorktrainOpts(WorktrainRun(WorktrainRunOpts o o o o o ["key.."])))))) $ execParserMaybe (globalInfo role) ["worktrain", "run", "key.."]
 assertEqual Nothing $ execParserMaybe (globalInfo studentRole) ["worktrain", "run"]
 assertEqual Nothing $ execParserMaybe (globalInfo studentRole) ["worktrain", "run", "v"]
                
prop_worktrainRunSuccess repoNN termNN courseNN groupNN projectNN ks = let keys = validArgs ks in keys /= [] ==>
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess 3 (repoNN, termNN, courseNN, groupNN, projectNN, keys) worktrainRunF [worktrainSub, runSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) keys

test_worktrainDate = do
 forM_ [adminRole, teacherRole, correctorRole] $ \role -> do
   assertEqual (Just(Global(Worktrain(WorktrainOpts(WorktrainDate(WorktrainDateOpts o o o o o)))))) $ execParserMaybe (globalInfo role) ["worktrain", "date"]
   assertEqual Nothing $ execParserMaybe (globalInfo role) ["worktrain", "date", "v"]
 assertEqual Nothing $ execParserMaybe (globalInfo studentRole) ["worktrain", "date"]             
              
prop_worktrainDateSuccess repoNN termNN courseNN groupNN projectNN =
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN] ==>
  testSuccess 3 (repoNN, termNN, courseNN, groupNN, projectNN) worktrainDateF [worktrainSub, dateSub]
   (projectOpts repoNN termNN courseNN groupNN projectNN) noArgs

test_worktrainList = do
 forM_ [adminRole, teacherRole, correctorRole] $ \role -> do
   assertEqual (Just(Global(Worktrain(WorktrainOpts(WorktrainList(WorktrainListOpts o o o o o o)))))) $ execParserMaybe (globalInfo role) ["worktrain", "list"]
   assertEqual Nothing $ execParserMaybe (globalInfo role) ["worktrain", "list", "v"]
 assertEqual Nothing $ execParserMaybe (globalInfo studentRole) ["worktrain", "list"] 
 assertEqual Nothing $ execParserMaybe (globalInfo studentRole) ["worktrain", "list", "v"]            
               
prop_worktrainListSuccess repoNN termNN courseNN groupNN projectNN dateNN =
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN, dateNN] ==>
  testSuccess 3 (repoNN, termNN, courseNN, groupNN, projectNN, dateNN) worktrainListF [worktrainSub, listSub]
   (trainRunOpts repoNN termNN courseNN groupNN projectNN dateNN) noArgs

test_worktrainExtract = do
 forM_ [adminRole, teacherRole, correctorRole] $ \role -> do
   assertEqual Nothing $ execParserMaybe (globalInfo role) ["worktrain", "extract"]
   assertEqual Nothing $ execParserMaybe (globalInfo role) ["worktrain", "extract", "dir"]
   assertEqual (Just(Global(Worktrain(WorktrainOpts(WorktrainExtract(WorktrainExtractOpts o o o o o o "dir" ["key.."])))))) $ execParserMaybe (globalInfo role) ["worktrain", "extract", "dir", "key.."]
   assertEqual Nothing $ execParserMaybe (globalInfo studentRole) ["worktrain", "extract"]
   assertEqual Nothing $ execParserMaybe (globalInfo studentRole) ["worktrain", "extract", "dir"]
   assertEqual Nothing $ execParserMaybe (globalInfo studentRole) ["worktrain", "extract", "dir", "key.."]
          
prop_worktrainExtractSuccess repoNN termNN courseNN groupNN projectNN dateNN di ks = let args = validArgs (di:ks) in length args >= 2 ==>
 validOpts [repoNN, termNN, courseNN, groupNN, projectNN, dateNN] ==> let (dir:keys) = args in
  testSuccess 3 (repoNN, termNN, courseNN, groupNN, projectNN, dateNN, dir, keys) worktrainExtractF [worktrainSub, extractSub]
   (trainRunOpts repoNN termNN courseNN groupNN projectNN dateNN) (dir:keys)

test_worktrainOutput = do
 forM_ [adminRole, teacherRole, correctorRole] $ \role -> do
   assertEqual Nothing $ execParserMaybe (globalInfo role) ["worktrain", "output"]
   assertEqual Nothing $ execParserMaybe (globalInfo role) ["worktrain", "output", "dir"]
   assertEqual Nothing $ execParserMaybe (globalInfo role) ["worktrain", "output", "dir", "--merge"]
   assertEqual (Just(Global(Worktrain(WorktrainOpts(WorktrainOutput(WorktrainOutputOpts o o o o o o "dir" ["key.."] False)))))) $ execParserMaybe (globalInfo role) ["worktrain", "output", "dir", "key.."]
   assertEqual (Just(Global(Worktrain(WorktrainOpts(WorktrainOutput(WorktrainOutputOpts o o o o o o "dir" ["key.."] True)))))) $ execParserMaybe (globalInfo role) ["worktrain", "output", "dir", "key..", "--merge"]
 assertEqual Nothing $ execParserMaybe (globalInfo studentRole) ["worktrain", "output"]
 assertEqual Nothing $ execParserMaybe (globalInfo studentRole) ["worktrain", "output", "dir"]
 assertEqual Nothing $ execParserMaybe (globalInfo studentRole) ["worktrain", "output", "dir", "--merge"]
 assertEqual Nothing $ execParserMaybe (globalInfo studentRole) ["worktrain", "output", "dir", "key.."]
 assertEqual Nothing $ execParserMaybe (globalInfo studentRole) ["worktrain", "output", "dir", "key..", "--merge"] 
           
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
          