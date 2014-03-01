{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Domain.ProjectPropTest where

import Test.Framework
import TestUtils

import Infrastructure.Node
import Domain.Project

{-# ANN module "HLint: ignore Use camelCase" #-}

prop_emptyProject name = let p = make name in
 [""] == applyGets p [getStartDate, getEndDate, getLateDate, getValidationCommand, getAcceptExecutables,
                      getTrainTimeLimit, getTrainSpaceLimit] &&
 [("", "")] == applyGets p [getValidationScript, getTrainScript] &&
 [] == getNamesToValidate p

prop_setGetStartEndLateDates name start end late =
 let [p] = applySets (make name) [(setStartDate, start), (setEndDate, end), (setLateDate, late)]
 in  start == getStartDate p &&
     end == getEndDate p &&
     late == getLateDate p

prop_setGetAcceptExecutables name acceptsExec =
 acceptsExec == getAcceptExecutables (setAcceptExecutables (make name) acceptsExec)

prop_addRemoveNamesToValidate name ns = let ns' = uniqueNonEmptyNoComma ns in ns' /= [] ==>
 let (xs,names) = splitAt (length ns' `div` 2) ns'
     p = addNamesToValidate (make name) xs
     absentAdd = addNamesToValidate p names
     presentAdd = addNamesToValidate absentAdd names
     presentRemove = removeNamesToValidate absentAdd names
     absentRemove = removeNamesToValidate p names
 in  areEqual [presentRemove,absentRemove, p] &&
     absentAdd == presentAdd &&
     sameElements ns' (getNamesToValidate absentAdd)

prop_setGetValidationCommand name command =
 command == getValidationCommand (setValidationCommand (make name) command)

prop_setGetValidationScript name scriptName scriptContent =
 (scriptName, scriptContent) == getValidationScript (setValidationScript (make name) scriptName scriptContent)

prop_setGetTrainScript name scriptName scriptContent =
 (scriptName, scriptContent) == getTrainScript (setTrainScript (make name) scriptName scriptContent)

prop_setGetTrainTimeLimit name timeLimit =
 timeLimit == getTrainTimeLimit (setTrainTimeLimit (make name) timeLimit)

prop_setGetTrainSpaceLimit name spaceLimit =
 spaceLimit == getTrainSpaceLimit (setTrainSpaceLimit (make name) spaceLimit)





 