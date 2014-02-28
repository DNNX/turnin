{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Domain.ProjectHierarchyTest where

import Test.Framework
import TestUtils
import Data.Maybe

import Domain.Project
import Domain.SubmitRepo
import Domain.TrainFileRepo
import Domain.TrainRunRepo 
import Domain.TrainRun
 
{-# ANN module "HLint: ignore Use camelCase" #-}

test_projectChildren = 
 let p = makeProject "project"
     s = addSubmit emptySubmitRepo "submitKey" "submitValue"
     tf = addTrainFile emptyTrainFileRepo "trainFileKey" "trainFileValue"
     tr = addTrainRun emptyTrainRunRepo $ makeTrainRun "trainRunName" in do
 assertEqual emptySubmitRepo $ getSubmitRepo p
 assertEqual emptyTrainFileRepo $ getTrainFileRepo p
 assertEqual emptyTrainRunRepo $ getTrainRunRepo p
 assertEqual s $ getSubmitRepo $ setSubmitRepo p s
 assertEqual tf $ getTrainFileRepo $ setTrainFileRepo p tf
 assertEqual tr $ getTrainRunRepo $ setTrainRunRepo p tr

prop_projectChildren projectName submit trainFile trainRun = 
 length (filter (not.null) [submit,trainFile,trainRun]) == 3 ==>
 let p = makeProject projectName
     s = addSubmit emptySubmitRepo submit submit
     tf = addTrainFile emptyTrainFileRepo trainFile trainFile
     tr = addTrainRun emptyTrainRunRepo $ makeTrainRun trainRun
 in  emptySubmitRepo == getSubmitRepo p &&
     emptyTrainFileRepo == getTrainFileRepo p &&
     emptyTrainRunRepo == getTrainRunRepo p &&
     s == getSubmitRepo (setSubmitRepo p s) &&
     tf == getTrainFileRepo (setTrainFileRepo p tf) &&
     tr == getTrainRunRepo (setTrainRunRepo p tr)

test_trainRunRepoChildren = 
 let trr = emptyTrainRunRepo
     tr = makeTrainRun "trainRunDate"
     absentAdd = addTrainRun trr tr
     presentAdd = addTrainRun absentAdd tr
     presentRemove = removeTrainRun absentAdd "trainRunDate"
     absentRemove = removeTrainRun trr "trainRunDate" in do 
 assertEqual trr presentRemove
 assertEqual trr absentRemove
 assertEqual absentAdd presentAdd
 assertEqual [] $ getTrainRuns trr
 assertEqual ["trainRunDate"] $ getTrainRuns absentAdd
 assertEqual True $ isNothing $ getTrainRun trr "trainRunDate"
 assertEqual (Just tr) $ getTrainRun absentAdd "trainRunDate"       

prop_trainRunRepoChildren rs = let trainRunDates = uniqueNonEmpty rs
                               in  trainRunDates /= [] ==> f trainRunDates
 where f dates@(trainRunDate:rest) = 
        let trr = foldl (\x d -> addTrainRun x (makeTrainRun d)) emptyTrainRunRepo rest
            tr = makeTrainRun trainRunDate
            absentAdd = addTrainRun trr tr
            presentAdd = addTrainRun absentAdd tr
            presentRemove = removeTrainRun absentAdd trainRunDate
            absentRemove = removeTrainRun trr trainRunDate 
        in  areEqual [presentRemove, absentRemove, trr] &&
            absentAdd == presentAdd &&
            sameElements rest (getTrainRuns trr) &&
            sameElements dates (getTrainRuns absentAdd) &&
            isNothing (getTrainRun trr trainRunDate) &&
            Just tr == getTrainRun absentAdd trainRunDate                                                   


test_emptyRepos = do 
 assertEqual [] $ getSubmits emptySubmitRepo
 assertEqual [] $ getLateSubmits emptySubmitRepo
 assertEqual [] $ getTrainFiles emptyTrainFileRepo
 assertEqual [] $ getTrainRuns emptyTrainRunRepo
 
prop_emptySubmitRepo = [[]] == applyGets emptySubmitRepo [getSubmits, getLateSubmits]
prop_emptyTrainFileRepo = [] == getTrainFiles emptyTrainFileRepo
prop_emptyTrainRunRepo = [] == getTrainRuns emptyTrainRunRepo

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

prop_addRemoveGetSubmits ks suffix = let ks' = uniqueNonEmpty ks in ks' /= [] ==> f $ zip ks' $ map (++suffix) ks'
 where f ((key, content):rest) =
          let sr = foldl (\r (k,c) -> addSubmit r k c) emptySubmitRepo rest
              absentAdd = addSubmit sr key content
              presentAdd = addSubmit absentAdd key (content ++ suffix)
              presentRemove = removeSubmit absentAdd key
              absentRemove = removeSubmit sr key
          in  areEqual [presentRemove, absentRemove, sr] &&
              getSubmit absentAdd key ++ suffix == getSubmit presentAdd key &&
              sameElements (map fst rest) (getSubmits sr) &&
              sameElements (key:map fst rest) (getSubmits absentAdd) &&
              "" == getSubmit sr key &&
              content == getSubmit absentAdd key

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
           
prop_addRemoveGetLateSubmits ks = let ks' = uniqueNonEmptyNoComma ks in ks' /= [] ==> f ks'
 where f (key:rest) = 
        let sr = foldl addLateSubmit emptySubmitRepo rest
            absentAdd = addLateSubmit sr key
            presentAdd = addLateSubmit absentAdd key
            presentRemove = removeLateSubmit absentAdd key
            absentRemove = removeLateSubmit sr key
        in  areEqual [presentRemove, absentRemove, sr] && 
            absentAdd == presentAdd &&
            sameElements rest (getLateSubmits sr) &&
            sameElements (key:rest) (getLateSubmits absentAdd)

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

prop_addRemoveGetTrainFiles fs suffix = let fs' = uniqueNonEmpty fs in fs' /= [] ==> f $ zip fs' $ map (++suffix) fs'
 where f ((name, content):rest) = 
        let fr = foldl (\r (n,c) -> addTrainFile r n c) emptyTrainFileRepo rest
            absentAdd = addTrainFile fr name content
            presentAdd = addTrainFile absentAdd name (content ++ suffix)
            presentRemove = removeTrainFile absentAdd name
            absentRemove = removeTrainFile fr name
        in  areEqual [presentRemove, absentRemove, fr] &&  
            getTrainFile absentAdd name ++ suffix == getTrainFile presentAdd name &&
            sameElements (map fst rest) (getTrainFiles fr) &&
            sameElements (name: map fst rest) (getTrainFiles absentAdd) &&
            "" == getTrainFile fr name &&
            content == getTrainFile absentAdd name
        
test_addRemoveGetTrainRuns =
 let fr = makeTrainRun ""
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
        
prop_addRemoveGetTrainRuns name rs suffix = let rs' = uniqueNonEmpty rs in rs' /= [] ==> f $ zip rs' $ map (++suffix) rs'
 where f ((key, content):rest) = 
        let fr = foldl (\r (k,c) -> addResult r k c) (makeTrainRun name) rest
            absentAdd = addResult fr key content
            presentAdd = addResult absentAdd key (content ++ suffix)
            presentRemove = removeResult absentAdd key
            absentRemove = removeResult fr key
        in  areEqual [presentRemove, absentRemove, fr] &&
            getResult absentAdd key ++ suffix == getResult presentAdd key &&
            sameElements (map fst rest) (getResults fr) &&
            sameElements (key: map fst rest) (getResults absentAdd) &&
            content == getResult absentAdd key      
            
        