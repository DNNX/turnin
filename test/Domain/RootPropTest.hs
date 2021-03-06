{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Domain.RootPropTest where

import Test.Framework
import TestUtils

import Infrastructure.Node
import Domain.Root

{-# ANN module "HLint: ignore Use camelCase" #-}

prop_emptyRoot name = let r = make name in
 [""] == applyGets r [getCurrentThreshold, getChooseThreshold, getTerm1Date, getTerm2Date, getTerm3Date, getProjectEndDate,
                          getProjectLateDate, getAcceptExecutables, getTrainTimeLimit, getTrainSpaceLimit] &&
 [[]] == applyGets r [getAdminGroups, getTeacherGroups]

prop_thresholds name currentThresh chooseThresh =
 let [r] = applySets (make name) [(setCurrentThreshold, currentThresh), (setChooseThreshold, chooseThresh)]
 in  currentThresh == getCurrentThreshold r &&
     chooseThresh == getChooseThreshold r

prop_termDates name term1 term2 term3 =
 let [r] = applySets (make name) [(setTerm1Date, term1), (setTerm2Date, term2), (setTerm3Date, term3)]
 in  term1 == getTerm1Date r &&
     term2 == getTerm2Date r &&
     term3 == getTerm3Date r

prop_projectDates name end late =
 let [r] = applySets (make name) [(setProjectEndDate, end), (setProjectLateDate, late)]
 in  end == getProjectEndDate r &&
     late == getProjectLateDate r

prop_acceptExecutables name acceptExec =
 acceptExec == getAcceptExecutables (setAcceptExecutables (make name) acceptExec)

prop_trainTimeLimit name timeLimit =
 timeLimit == getTrainTimeLimit (setTrainTimeLimit (make name) timeLimit)

prop_trainSpaceLimit name spaceLimit =
 spaceLimit == getTrainSpaceLimit (setTrainSpaceLimit (make name) spaceLimit)

prop_adminGroups name gs = let groups = uniqueNonEmptyNoComma gs in groups /= [] ==>
 sameElements groups $ getAdminGroups $ setAdminGroups (make name) groups

prop_teacherGroups name gs = let groups = uniqueNonEmptyNoComma gs in
 sameElements groups $ getTeacherGroups $ setTeacherGroups (make name) groups

prop_correctors name cs = let correctors = uniqueNonEmptyNoComma cs
                          in correctors /= [] ==> f correctors
 where f (corrector:rest) =
        let r = foldl addCorrector (make name) rest
            absentAdd = addCorrector r corrector
            presentAdd = addCorrector absentAdd corrector
            presentRemove = removeCorrector absentAdd corrector
            absentRemove = removeCorrector r corrector
        in  areEqual [presentRemove, absentRemove, r] &&
            absentAdd == presentAdd &&
            sameElements rest (getCorrectors r) &&
            sameElements (corrector:rest) (getCorrectors absentAdd)




