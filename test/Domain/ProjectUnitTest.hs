{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Domain.ProjectUnitTest where

import Test.Framework

import Domain.Project

{-# ANN module "HLint: ignore Use camelCase" #-}

test_emptyProject = let n = makeProject "" in do
 assertEqual "" $ getStartDate n
 assertEqual "" $ getEndDate n
 assertEqual "" $ getLateDate n
 assertEqual "" $ getAcceptExecutables n
 assertEqual "" $ getValidationCommand n
 assertEqual "" $ getTrainTimeLimit n
 assertEqual "" $ getTrainSpaceLimit n
 assertEqual ("","") $ getValidationScript n
 assertEqual ("","") $ getTrainScript n
 assertEqual [] $ getNamesToValidate n

test_getSet =  let n = makeProject "" in do
 assertEqual "start" $ getStartDate $ setStartDate n "start"
 assertEqual "end" $ getEndDate $ setEndDate n "end"
 assertEqual "late" $ getLateDate $ setLateDate n "late"
 assertEqual "acceptExec" $ getAcceptExecutables $ setAcceptExecutables n "acceptExec"
 assertEqual "validationCommand" $ getValidationCommand $ setValidationCommand n "validationCommand"
 assertEqual ("scriptName", "scriptContent") $ getValidationScript $ setValidationScript n "scriptName" "scriptContent"
 assertEqual ("scriptName", "scriptContent") $ getTrainScript $ setTrainScript n "scriptName" "scriptContent"
 assertEqual "timeLimit" $ getTrainTimeLimit $ setTrainTimeLimit n "timeLimit"
 assertEqual "spaceLimit" $ getTrainTimeLimit $ setTrainTimeLimit n "spaceLimit"
     
test_addRemoveNamesToValidate =
 let p = makeProject ""
     absentAdd = addNamesToValidate p ["n1", "n2"]
     presentAdd = addNamesToValidate absentAdd ["n1", "n2"]
     presentRemove = removeNamesToValidate absentAdd ["n1", "n2"]
     absentRemove = removeNamesToValidate p ["n1", "n2"] in do
 assertEqual p presentRemove
 assertEqual p absentRemove
 assertEqual absentAdd presentAdd
 assertEqual [] $ getNamesToValidate p
 assertEqual ["n1", "n2"] $ getNamesToValidate absentAdd 
 
 
 
 