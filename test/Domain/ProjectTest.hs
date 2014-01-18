{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Domain.ProjectTest where
import Test.Framework
 
import TestUtils

import Domain.Project

{-# ANN module "HLint: ignore Use camelCase" #-}

prop_emptyProject name = let p = makeProject name in
 [""] == applyGets p [getStartDate, getEndDate, getLateDate, getValidationCommand, getAcceptExecutables, 
                      getTrainTimeLimit, getTrainSpaceLimit] &&
 [("", "")] == applyGets p [getValidationScript, getTrainScript] &&
 [] == getNamesToValidate p   
  
prop_setGetStartEndLateDates name start end late = 
 let [p] = applySets (makeProject name) [(setStartDate, start), (setEndDate, end), (setLateDate, late)]
 in  start == getStartDate p &&
     end == getEndDate p && 
     late == getLateDate p
       
prop_setGetAcceptExecutables name acceptsExec = 
 acceptsExec == getAcceptExecutables (setAcceptExecutables (makeProject name) acceptsExec)
 
prop_addRemoveNamesToValidate name ns = let ns' = uniqueNonEmptyNoComma ns in ns' /= [] ==> 
 let (xs,names) = splitAt (length ns' `div` 2) ns'
     p = addNamesToValidate (makeProject name) xs
     absentAdd = addNamesToValidate p names
     presentAdd = addNamesToValidate absentAdd names
     presentRemove = removeNamesToValidate absentAdd names
     absentRemove = removeNamesToValidate p names
 in  areEqual [presentRemove,absentRemove, p] &&
     absentAdd == presentAdd &&
     sameElements ns' (getNamesToValidate absentAdd)
 
prop_setGetValidationCommand name command = 
 command == getValidationCommand (setValidationCommand (makeProject name) command)
 
prop_setGetValidationScript name scriptName scriptContent = 
 (scriptName, scriptContent) == getValidationScript (setValidationScript (makeProject name) scriptName scriptContent)
 
prop_setGetTrainScript name scriptName scriptContent = 
 (scriptName, scriptContent) == getTrainScript (setTrainScript (makeProject name) scriptName scriptContent)
 
prop_setGetTrainTimeLimit name timeLimit = 
 timeLimit == getTrainTimeLimit (setTrainTimeLimit (makeProject name) timeLimit)
 
prop_setGetTrainSpaceLimit name spaceLimit = 
 spaceLimit == getTrainSpaceLimit (setTrainSpaceLimit (makeProject name) spaceLimit) 
 
 
 
 
 
 