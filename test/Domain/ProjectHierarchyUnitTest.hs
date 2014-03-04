{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Domain.ProjectHierarchyUnitTest where

import Test.Framework

import Infrastructure.Node
import Domain.Project
import Domain.SubmitRepo
import Domain.TrainFileRepo
import Domain.TrainRun

{-# ANN module "HLint: ignore Use camelCase" #-}

test_projectChildren =
 let p = make "project"
     s = addSubmit emptySubmitRepo "submitKey" "submitValue"
     tf = addTrainFile emptyTrainFileRepo "trainFileKey" "trainFileValue"
     tr = addChild emptyTrainRunRepo $ make "trainRunName" in do
 assertEqual emptySubmitRepo $ getSubmitRepo p
 assertEqual emptyTrainFileRepo $ getTrainFileRepo p
 assertEqual emptyTrainRunRepo $ getTrainRunRepo p
 assertEqual s $ getSubmitRepo $ setSubmitRepo p s
 assertEqual tf $ getTrainFileRepo $ setTrainFileRepo p tf
 assertEqual tr $ getTrainRunRepo $ setTrainRunRepo p tr

test_trainRunRepoChildren =
 let trr = emptyTrainRunRepo
     tr = make "trainRunDate"
     absentAdd = addChild trr tr
     presentAdd = addChild absentAdd tr
     presentRemove = removeChild absentAdd "trainRunDate"
     absentRemove = removeChild trr "trainRunDate" in do
 assertEqual trr presentRemove
 assertEqual trr absentRemove
 assertEqual absentAdd presentAdd
 assertEqual [] $ getChildren trr
 assertEqual [tr] $ getChildren absentAdd
 assertEqual Nothing $ getChild trr "trainRunDate"
 assertEqual (Just tr) $ getChild absentAdd "trainRunDate"

test_emptyRepos = do
 assertEqual [] $ getSubmits emptySubmitRepo
 assertEqual [] $ getLateSubmits emptySubmitRepo
 assertEqual [] $ getTrainFiles emptyTrainFileRepo
 assertEqual [] $ getChildren emptyTrainRunRepo

test_addRemoveGetSubmits =
 let sr = emptySubmitRepo
     absentAdd = addSubmit sr "key" "content"
     presentAdd = addSubmit absentAdd "key" ("content" ++ "suffix")
     presentRemove = removeSubmit absentAdd "key"
     absentRemove = removeSubmit sr "key" in do
 assertEqual sr presentRemove
 assertEqual sr absentRemove
 assertEqual(getSubmit absentAdd "key" ++ "suffix") $ getSubmit presentAdd "key"
 assertEqual [] $ getSubmits sr
 assertEqual ["key"] $ getSubmits absentAdd
 assertEqual "" $ getSubmit sr "key"
 assertEqual "content" $ getSubmit absentAdd "key"

test_addRemoveGetLateSubmits =
 let sr = emptySubmitRepo
     absentAdd = addLateSubmit sr "key"
     presentAdd = addLateSubmit absentAdd "key"
     presentRemove = removeLateSubmit absentAdd "key"
     absentRemove = removeLateSubmit sr "key" in do
 assertEqual sr presentRemove
 assertEqual sr absentRemove
 assertEqual absentAdd presentAdd
 assertEqual [] $ getLateSubmits sr
 assertEqual ["key"] $ getLateSubmits absentAdd

test_addRemoveGetTrainFiles =
 let fr = emptyTrainFileRepo
     absentAdd = addTrainFile fr "name" "content"
     presentAdd = addTrainFile absentAdd "name" ("content" ++ "suffix")
     presentRemove = removeTrainFile absentAdd "name"
     absentRemove = removeTrainFile fr "name" in do
 assertEqual fr presentRemove
 assertEqual fr absentRemove
 assertEqual (getTrainFile absentAdd "name" ++ "suffix") $ getTrainFile presentAdd "name"
 assertEqual [] $ getTrainFiles fr
 assertEqual ["name"] $ getTrainFiles absentAdd
 assertEqual "" $ getTrainFile fr "name"
 assertEqual "content" $ getTrainFile absentAdd "name"

test_addRemoveGetTrainRuns =
 let fr = make ""
     absentAdd = addResult fr "key" "content"
     presentAdd = addResult absentAdd "key" ("content" ++ "suffix")
     presentRemove = removeResult absentAdd "key"
     absentRemove = removeResult fr "key" in do
 assertEqual fr presentRemove
 assertEqual fr absentRemove
 assertEqual (getResult absentAdd "key" ++ "suffix") $ getResult presentAdd "key"
 assertEqual [] $ getResults fr
 assertEqual ["key"] $ getResults absentAdd
 assertEqual "content" $ getResult absentAdd "key"
        