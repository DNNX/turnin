{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Interface.CommandLineParser.SubmitWorktrainUnitTest where

import Test.Framework
import Interface.CommandLineParser.ParserTestUtils
import Options.Applicative
import Control.Monad
import Security.SecurityManager
 
import Interface.Lexicon 
import Interface.CommandLineParser
import Interface.CommandLineParser.SubmitWorktrain

{-# ANN module "HLint: ignore Use camelCase" #-}

test_submit = forM_ allRoles  $ \role -> do 
   assertEqual Nothing $ execParserMaybe (globalInfo role) [submitSub]
   assertEqual (Just(Global(Submit(SubmitOpts o o o o o ["key.."])))) $ execParserMaybe (globalInfo role) [submitSub, "key.."]

test_inspect = forM_ allRoles  $ \role -> do 
   assertEqual (Just(Global(Inspect(InspectOpts o o o o o)))) $ execParserMaybe (globalInfo role) [inspectSub]
   assertEqual Nothing $ execParserMaybe (globalInfo role) [inspectSub, "v"]
 
test_extract = forM_ allRoles  $ \role -> do 
   assertEqual Nothing $ execParserMaybe (globalInfo role) [extractSub]
   assertEqual (Just(Global(Extract(ExtractOpts o o o o o "dir")))) $ execParserMaybe (globalInfo role) [extractSub, "dir"]
   assertEqual Nothing $ execParserMaybe (globalInfo role) [extractSub, "dir", "v"]    
      
test_worktrain = forM_ allRoles $ \role -> assertEqual Nothing $ execParserMaybe (globalInfo role) [worktrainSub]      
      
test_worktrainRun = do
 forM_ [adminRole, teacherRole, correctorRole] $ \role -> do
   assertEqual Nothing $ execParserMaybe (globalInfo role) [worktrainSub, runSub]
   assertEqual (Just(Global(Worktrain(WorktrainOpts(WorktrainRun(WorktrainRunOpts o o o o o ["key.."])))))) $ execParserMaybe (globalInfo role) [worktrainSub, runSub, "key.."]
 assertEqual Nothing $ execParserMaybe (globalInfo studentRole) [worktrainSub, runSub]
 assertEqual Nothing $ execParserMaybe (globalInfo studentRole) [worktrainSub, runSub, "v"]
     
test_worktrainDate = do
 forM_ [adminRole, teacherRole, correctorRole] $ \role -> do
   assertEqual (Just(Global(Worktrain(WorktrainOpts(WorktrainDate(WorktrainDateOpts o o o o o)))))) $ execParserMaybe (globalInfo role) [worktrainSub, dateSub]
   assertEqual Nothing $ execParserMaybe (globalInfo role) [worktrainSub, dateSub, "v"]
 assertEqual Nothing $ execParserMaybe (globalInfo studentRole) [worktrainSub, dateSub]             
     
test_worktrainList = do
 forM_ [adminRole, teacherRole, correctorRole] $ \role -> do
   assertEqual (Just(Global(Worktrain(WorktrainOpts(WorktrainList(WorktrainListOpts o o o o o o)))))) $ execParserMaybe (globalInfo role) [worktrainSub, listSub]
   assertEqual Nothing $ execParserMaybe (globalInfo role) [worktrainSub, listSub, "v"]
 assertEqual Nothing $ execParserMaybe (globalInfo studentRole) [worktrainSub, listSub] 
 assertEqual Nothing $ execParserMaybe (globalInfo studentRole) [worktrainSub, listSub, "v"]            
    
test_worktrainExtract = forM_ [adminRole, teacherRole, correctorRole] $ \role -> do
   assertEqual Nothing $ execParserMaybe (globalInfo role) [worktrainSub, extractSub]
   assertEqual Nothing $ execParserMaybe (globalInfo role) [worktrainSub, extractSub, "dir"]
   assertEqual (Just(Global(Worktrain(WorktrainOpts(WorktrainExtract(WorktrainExtractOpts o o o o o o "dir" ["key.."])))))) $ execParserMaybe (globalInfo role) [worktrainSub, extractSub, "dir", "key.."]
   assertEqual Nothing $ execParserMaybe (globalInfo studentRole) [worktrainSub, extractSub]
   assertEqual Nothing $ execParserMaybe (globalInfo studentRole) [worktrainSub, extractSub, "dir"]
   assertEqual Nothing $ execParserMaybe (globalInfo studentRole) [worktrainSub, extractSub, "dir", "key.."]
      
test_worktrainOutput = do
 forM_ [adminRole, teacherRole, correctorRole] $ \role -> do
   assertEqual Nothing $ execParserMaybe (globalInfo role) [worktrainSub, outputSub]
   assertEqual Nothing $ execParserMaybe (globalInfo role) [worktrainSub, outputSub, "dir"]
   assertEqual Nothing $ execParserMaybe (globalInfo role) [worktrainSub, outputSub, "dir", "--merge"]
   assertEqual (Just(Global(Worktrain(WorktrainOpts(WorktrainOutput(WorktrainOutputOpts o o o o o o "dir" ["key.."] False)))))) $ execParserMaybe (globalInfo role) [worktrainSub, outputSub, "dir", "key.."]
   assertEqual (Just(Global(Worktrain(WorktrainOpts(WorktrainOutput(WorktrainOutputOpts o o o o o o "dir" ["key.."] True)))))) $ execParserMaybe (globalInfo role) [worktrainSub, outputSub, "dir", "key..", "--merge"]
 assertEqual Nothing $ execParserMaybe (globalInfo studentRole) [worktrainSub, outputSub]
 assertEqual Nothing $ execParserMaybe (globalInfo studentRole) [worktrainSub, outputSub, "dir"]
 assertEqual Nothing $ execParserMaybe (globalInfo studentRole) [worktrainSub, outputSub, "dir", "--merge"]
 assertEqual Nothing $ execParserMaybe (globalInfo studentRole) [worktrainSub, outputSub, "dir", "key.."]
 assertEqual Nothing $ execParserMaybe (globalInfo studentRole) [worktrainSub, outputSub, "dir", "key..", "--merge"] 
       
submitF (Global(Submit(SubmitOpts a b c d e f))) = (a,b,c,d,e,f)
inspectF (Global(Inspect(InspectOpts a b c d e))) = (a,b,c,d,e)
extractF (Global(Extract(ExtractOpts a b c d e f))) = (a,b,c,d,e,f)
worktrainRunF (Global(Worktrain(WorktrainOpts(WorktrainRun(WorktrainRunOpts a b c d e f))))) = (a,b,c,d,e,f)
worktrainDateF (Global(Worktrain(WorktrainOpts(WorktrainDate(WorktrainDateOpts a b c d e))))) = (a,b,c,d,e)
worktrainListF (Global(Worktrain(WorktrainOpts(WorktrainList(WorktrainListOpts a b c d e f))))) = (a,b,c,d,e,f)           
worktrainExtractF (Global(Worktrain(WorktrainOpts(WorktrainExtract(WorktrainExtractOpts a b c d e f g h))))) = (a,b,c,d,e,f,g,h)           
worktrainOutputF (Global(Worktrain(WorktrainOpts(WorktrainOutput(WorktrainOutputOpts a b c d e f g h i))))) = (a,b,c,d,e,f,g,h,i)           
          