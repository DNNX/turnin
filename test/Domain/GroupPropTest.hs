{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Domain.GroupPropTest where

import Test.Framework
import TestUtils

import Domain.Group

{-# ANN module "HLint: ignore Use camelCase" #-}

prop_emptyGroup name = let g = makeGroup name in [[]] == applyGets g [getTeachers, getCorrectors]

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
