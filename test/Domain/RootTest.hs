{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Domain.RootTest where

import Test.Framework
import TestUtils

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

prop_emptyRoot name = let r = makeRoot name in
 [""] == applyGets r [getCurrentThreshold, getChooseThreshold, getTerm1Date, getTerm2Date, getTerm3Date, getProjectEndDate, 
                          getProjectLateDate, getAcceptExecutables, getTrainTimeLimit, getTrainSpaceLimit] &&
 [[]] == applyGets r [getAdminGroups, getTeacherGroups]

test_thresholds =  let n = makeRoot "" in do
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
 
prop_thresholds name currentThresh chooseThresh =
 let [r] = applySets (makeRoot name) [(setCurrentThreshold, currentThresh), (setChooseThreshold, chooseThresh)]
 in  currentThresh == getCurrentThreshold r &&
     chooseThresh == getChooseThreshold r  
 
prop_termDates name term1 term2 term3 =
 let [r] = applySets (makeRoot name) [(setTerm1Date, term1), (setTerm2Date, term2), (setTerm3Date, term3)]
 in  term1 == getTerm1Date r &&
     term2 == getTerm2Date r &&
     term3 == getTerm3Date r 
                    
prop_projectDates name end late =
 let [r] = applySets (makeRoot name) [(setProjectEndDate, end), (setProjectLateDate, late)]
 in  end == getProjectEndDate r &&
     late == getProjectLateDate r 
          
prop_acceptExecutables name acceptExec =
 acceptExec == getAcceptExecutables (setAcceptExecutables (makeRoot name) acceptExec)
      
prop_trainTimeLimit name timeLimit =
 timeLimit == getTrainTimeLimit (setTrainTimeLimit (makeRoot name) timeLimit)     
                 
prop_trainSpaceLimit name spaceLimit =
 spaceLimit == getTrainSpaceLimit (setTrainSpaceLimit (makeRoot name) spaceLimit)
  
prop_adminGroups name gs = let groups = uniqueNonEmptyNoComma gs in groups /= [] ==>
 sameElements groups $ getAdminGroups $ setAdminGroups (makeRoot name) groups

prop_teacherGroups name gs = let groups = uniqueNonEmptyNoComma gs in  
 sameElements groups $ getTeacherGroups $ setTeacherGroups (makeRoot name) groups   

prop_correctors name cs = let correctors = uniqueNonEmptyNoComma cs 
                          in correctors /= [] ==> f correctors
 where f (corrector:rest) = 
        let r = foldl addCorrector (makeRoot name) rest
            absentAdd = addCorrector r corrector
            presentAdd = addCorrector absentAdd corrector
            presentRemove = removeCorrector absentAdd corrector
            absentRemove = removeCorrector r corrector
        in  areEqual [presentRemove, absentRemove, r] &&
            absentAdd == presentAdd &&
            sameElements rest (getCorrectors r) &&
            sameElements (corrector:rest) (getCorrectors absentAdd)
      
                        


