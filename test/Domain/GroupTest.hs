{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Domain.GroupTest where

import Test.Framework
import TestUtils

import Domain.Group
 
{-# ANN module "HLint: ignore Use camelCase" #-}

test_emptyGroup = let n = makeGroup "" in do
 assertEqual [] $ getTeachers n
 assertEqual [] $ getCorrectors n
 
prop_emptyGroup name = let g = makeGroup name in
 [[]] == applyGets g [getTeachers, getCorrectors]
 
test_teachers = 
  let g = makeGroup ""
      absentAdd = addTeachers g ["t1", "t2"]
      presentAdd = addTeachers absentAdd ["t1", "t2"]
      presentRemove = removeTeachers absentAdd ["t1", "t2"]
      absentRemove = removeTeachers g ["t1", "t2"] in do
 _ <- assertEqual g presentRemove
 assertEqual g absentRemove
 assertEqual absentAdd presentAdd
 assertEqual [] $ getTeachers g
 assertEqual ["t1", "t2"] $ getTeachers absentAdd
 
prop_teachers name ts = let ts' = uniqueNonEmptyNoComma ts in ts' /= [] ==>
 let  (xs, teachers) = splitAt (length ts' `div` 2) ts'
      g = addTeachers (makeGroup name) xs
      absentAdd = addTeachers g teachers
      presentAdd = addTeachers absentAdd teachers
      presentRemove = removeTeachers absentAdd teachers
      absentRemove = removeTeachers g teachers
 in areEqual [presentRemove, absentRemove, g] &&
    absentAdd == presentAdd && 
    sameElements (teachers ++ xs) (getTeachers absentAdd) &&
    sameElements xs (getTeachers g)
    
test_correctors = 
  let g = makeGroup ""
      absentAdd = addCorrectors g ["t1", "t2"]
      presentAdd = addCorrectors absentAdd ["t1", "t2"]
      presentRemove = removeCorrectors absentAdd ["t1", "t2"]
      absentRemove = removeCorrectors g ["t1", "t2"] in do
 assertEqual g presentRemove
 assertEqual g absentRemove
 assertEqual absentAdd presentAdd
 assertEqual [] $ getCorrectors g
 assertEqual ["t1", "t2"] $ getCorrectors absentAdd
 
prop_correctors name cs = let cs' = uniqueNonEmptyNoComma cs in cs' /= [] ==>
 let  (xs, correctors) = splitAt (length cs' `div` 2) cs'
      g = addCorrectors (makeGroup name) xs
      absentAdd = addCorrectors g correctors
      presentAdd = addCorrectors absentAdd correctors
      presentRemove = removeCorrectors absentAdd correctors
      absentRemove = removeCorrectors g correctors
 in areEqual [presentRemove, absentRemove, g] &&
    absentAdd == presentAdd &&
    sameElements (correctors ++ xs) (getCorrectors absentAdd) &&
    sameElements xs (getCorrectors g)
