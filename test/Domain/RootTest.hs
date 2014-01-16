{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Domain.RootTest where
import Test.Framework

import Data.List

import Domain.Root
 
{-# ANN module "HLint: ignore Use camelCase" #-}

prop_emptyRoot name = let r = makeRoot name in
 [""] == nub (map ($ r) [getCurrentThreshold, getChooseThreshold, getTerm1Date, getTerm2Date, getTerm3Date, getProjectEndDate, 
                          getProjectLateDate, getAcceptExecutables, getTrainTimeLimit, getTrainSpaceLimit]) &&
 [[]] == nub (map ($ r) [getAdminGroups, getTeacherGroups])

prop_thresholds name currentThresh chooseThresh =
 let r = makeRoot name
     rs = map (foldl apply r) $ permutations [(setCurrentThreshold, currentThresh), (setChooseThreshold, chooseThresh)]
     apply x (g,d) = g x d
     [r'] = nub rs 
 in  currentThresh == getCurrentThreshold r' &&
     chooseThresh == getChooseThreshold r'  
     
prop_termDates name term1 term2 term3 =
 let r = makeRoot name
     rs = map (foldl apply r) $ permutations [(setTerm1Date, term1), (setTerm2Date, term2), (setTerm3Date, term3)]
     apply x (g,d) = g x d
     [r'] = nub rs 
 in  term1 == getTerm1Date r' &&
     term2 == getTerm2Date r' &&
     term3 == getTerm3Date r' 
          
prop_projectDates name end late =
 let r = makeRoot name
     rs = map (foldl apply r) $ permutations [(setProjectEndDate, end), (setProjectLateDate, late)]
     apply x (g,d) = g x d
     [r'] = nub rs 
 in  end == getProjectEndDate r' &&
     late == getProjectLateDate r' 
     
prop_acceptExecutables name acceptExec =
 acceptExec == getAcceptExecutables (setAcceptExecutables (makeRoot name) acceptExec)
 
prop_trainTimeLimit name timeLimit =
 timeLimit == getTrainTimeLimit (setTrainTimeLimit (makeRoot name) timeLimit)                
  
prop_trainSpaceLimit name spaceLimit =
 spaceLimit == getTrainSpaceLimit (setTrainSpaceLimit (makeRoot name) spaceLimit)
 
prop_adminGroups name gs = let groups = nub $ filter (not.null) $ map (filter (/=',')) gs in groups /= [] ==>
 let r = setAdminGroups (makeRoot name) groups 
 in null (groups \\ getAdminGroups r) && null (getAdminGroups r \\ groups) 

prop_teacherGroups name gs = 
 let groups = nub $ filter (not.null) $ map (filter (/=',')) gs
     r = setAdminGroups (makeRoot name) groups  
 in  null (groups \\ getTeacherGroups r) && null (getTeacherGroups r \\ groups) 

prop_correctors name cs = let correctors = nub $ filter (not.null) $ map (filter (/=',')) cs 
                          in correctors /= [] ==> f correctors
 where f (corrector:rest) = 
        let r = foldl addCorrector (makeRoot name) rest
            absentAdd = addCorrector r corrector
            presentAdd = addCorrector absentAdd corrector
            presentRemove = removeCorrector absentAdd corrector
            absentRemove = removeCorrector r corrector
        in  (presentRemove, absentRemove) == (r,r) &&
            absentAdd == presentAdd &&
            not(isCorrector r corrector) &&
            isCorrector absentAdd corrector
      
                       


