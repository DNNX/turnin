{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Domain.RootUnitTest where

import Test.Framework

import Domain.Root
 
{-# ANN module "HLint: ignore Use camelCase" #-}

test_emptyRoot = let n = makeRoot "" in do
 assertEqual "" $ getCurrentThreshold n
 assertEqual "" $ getChooseThreshold n
 assertEqual "" $ getTerm1Date n
 assertEqual "" $ getTerm2Date n
 assertEqual "" $ getTerm3Date n
 assertEqual "" $ getProjectEndDate n 
 assertEqual "" $ getProjectLateDate n 
 assertEqual "" $ getAcceptExecutables n 
 assertEqual "" $ getTrainTimeLimit n
 assertEqual "" $ getTrainSpaceLimit n
 assertEqual [] $ getAdminGroups n
 assertEqual [] $ getTeacherGroups n
 assertEqual [] $ getCorrectors n

test_getSetAddRemove =  let n = makeRoot "" in do
 assertEqual "current" $ getCurrentThreshold $ setCurrentThreshold n "current"
 assertEqual "choose" $ getChooseThreshold $ setChooseThreshold n "choose"
 assertEqual "term1" $ getTerm1Date $ setTerm1Date n "term1"
 assertEqual "term2" $ getTerm2Date $ setTerm2Date n "term2"
 assertEqual "term3" $ getTerm3Date $ setTerm3Date n "term3"
 assertEqual "end" $ getProjectEndDate $ setProjectEndDate n "end"
 assertEqual "late" $ getProjectLateDate $ setProjectLateDate n "late"  
 assertEqual "acceptExec" $ getAcceptExecutables $ setAcceptExecutables n "acceptExec"
 assertEqual "timeLimit" $ getTrainTimeLimit $ setTrainTimeLimit n "timeLimit"
 assertEqual "spaceLimit" $ getTrainSpaceLimit $ setTrainSpaceLimit n "spaceLimit"
 assertEqual ["adminGroup1", "adminGroup2"] $ getAdminGroups $ setAdminGroups n ["adminGroup1", "adminGroup2"]
 assertEqual ["teacherGroup1", "teacherGroup2"] $ getTeacherGroups $ setTeacherGroups n ["teacherGroup1", "teacherGroup2"]
 assertEqual ["corrector"] $ getCorrectors $ removeCorrector (addCorrector (addCorrector n "corr") "corrector") "corr"
 