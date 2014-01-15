{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Domain.ProjectTest where
import Test.Framework
 
import Data.List

import Domain.Project

{-# ANN module "HLint: ignore Use camelCase" #-}


prop_emptyProject name = let p = makeProject name in
 [""] == nub (map ($ p) [getStartDate, getEndDate, getLateDate, getValidationCommand, getAcceptExecutables, 
                          getTrainTimeLimit, getTrainSpaceLimit]) &&
 [("", "")] == nub (map ($ p) [getValidationScript, getTrainScript]) &&
 [] == getNamesToValidate p   
  
prop_settingAndGettingStartEndLateDates name start end late = 
 let p = makeProject name
     ps = map (foldl apply p) $ permutations [(setStartDate, start), (setEndDate, end), (setLateDate, late)]
     apply x (g,d) = g x d
     [p'] = nub ps 
 in  start == getStartDate p' &&
     end == getEndDate p' && 
     late == getLateDate p'
       
prop_settingAndGettingAcceptExecutables name acceptsExec = 
 acceptsExec == (getAcceptExecutables $ setAcceptExecutables (makeProject name) acceptsExec)
 
prop_addingAndRemovingNamesToValidate name ns = let names = nub $ filter (not.null) $ map (filter (/=',')) ns in names /= [] ==> 
 let p = makeProject name
     absentAdd = addNamesToValidate p names
     presentAdd = addNamesToValidate absentAdd names
     presentRemove = removeNamesToValidate absentAdd names
     absentRemove = removeNamesToValidate p names
 in  (presentRemove,absentRemove) == (p,p) &&
     absentAdd == presentAdd &&
     (null $ names \\ getNamesToValidate absentAdd)
 
prop_settingAndGettingValidationCommand name command = 
 command == (getValidationCommand $ setValidationCommand (makeProject name) command)
 
prop_settingAndGettingValidationScript name scriptName scriptContent = 
 (scriptName, scriptContent) == (getValidationScript $ setValidationScript (makeProject name) scriptName scriptContent)
 
prop_settingAndGettingTrainScript name scriptName scriptContent = 
 (scriptName, scriptContent) == (getTrainScript $ setTrainScript (makeProject name) scriptName scriptContent)
 
prop_settingAndGettingTrainTimeLimit name timeLimit = 
 timeLimit == (getTrainTimeLimit $ setTrainTimeLimit (makeProject name) timeLimit)
 
prop_settingAndGettingTrainSpaceLimit name spaceLimit = 
 spaceLimit == (getTrainSpaceLimit $ setTrainSpaceLimit (makeProject name) spaceLimit) 
 
 
 
 
 
 