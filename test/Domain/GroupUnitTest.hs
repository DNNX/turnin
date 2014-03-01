{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Domain.GroupUnitTest where

import Test.Framework

import Domain.Group

{-# ANN module "HLint: ignore Use camelCase" #-}

test_emptyGroup = let n = makeGroup "" in do
 assertEqual [] $ getTeachers n
 assertEqual [] $ getCorrectors n

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

