{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Domain.ProjectHeirarchyTest where

import Test.Framework
import TestUtils
import Data.Maybe

import Domain.SubmitRepo
import Domain.TrainFileRepo
import Domain.TrainRunRepo 
import Domain.TrainRun
 
{-# ANN module "HLint: ignore Use camelCase" #-}

prop_trainRunRepoChildren repoName rs = let trainRunDates = uniqueNonEmpty rs
                                        in  trainRunDates /= [] ==> f trainRunDates
 where f dates@(trainRunDate:rest) = 
        let trr = foldl (\x d -> addTrainRun x (makeTrainRun d)) (makeTrainRunRepo repoName) rest
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

prop_emptySubmitRepo name = [[]] == applyGets (makeSubmitRepo name) [getSubmits, getLateSubmits]
 
prop_emptyTrainFileRepo name = [] == getTrainFiles (makeTrainFileRepo name)
  
prop_emptyTrainRunRepo name = [] == getTrainRuns (makeTrainRunRepo name)
 
prop_addRemoveGetSubmits name ks suffix = let ks' = uniqueNonEmpty ks in ks' /= [] ==> f $ zip ks' $ map (++suffix) ks'
 where f ((key, content):rest) =
          let sr = foldl (\r (k,c) -> addSubmit r k c) (makeSubmitRepo name) rest
              absentAdd = addSubmit sr key content
              presentAdd = addSubmit absentAdd key (content ++ suffix)
              presentRemove = removeSubmit absentAdd key
              absentRemove = removeSubmit sr key
          in  areEqual [presentRemove, absentRemove, sr] &&
              absentAdd == presentAdd &&
              sameElements (map fst rest) (getSubmits sr) &&
              sameElements (key:map fst rest) (getSubmits absentAdd) &&
              "" == getSubmit sr key &&
              (key ++ suffix) == getSubmit absentAdd key
              
prop_addRemoveGetLateSubmits name ks = let ks' = uniqueNonEmpty ks in ks' /= [] ==> f ks'
 where f (key:rest) = 
        let sr = foldl addLateSubmit (makeSubmitRepo name) rest
            absentAdd = addLateSubmit sr key
            presentAdd = addLateSubmit absentAdd key
            presentRemove = removeLateSubmit absentAdd key
            absentRemove = removeLateSubmit sr key
        in  areEqual [presentRemove, absentRemove, sr] && 
            absentAdd == presentAdd &&
            sameElements rest (getLateSubmits sr) &&
            sameElements (key:rest) (getLateSubmits absentAdd)

prop_addRemoveGetTrainFiles repoName fs suffix = let fs' = uniqueNonEmpty fs in fs' /= [] ==> f $ zip fs' $ map (++suffix) fs'
 where f ((name, content):rest) = 
        let fr = foldl (\r (n,c) -> addTrainFile r n c) (makeTrainFileRepo repoName) rest
            absentAdd = addTrainFile fr name content
            presentAdd = addTrainFile absentAdd name (content ++ suffix)
            presentRemove = removeTrainFile absentAdd name
            absentRemove = removeTrainFile fr name
        in  areEqual [presentRemove, absentRemove, fr] &&  
            absentAdd == presentAdd &&
            sameElements (map fst rest) (getTrainFiles fr) &&
            sameElements (name: map fst rest) (getTrainFiles absentAdd) &&
            "" == getTrainFile fr name &&
            (name ++ suffix) == getTrainFile absentAdd name
        
prop_addRemoveGetTrainRuns name rs suffix = let rs' = uniqueNonEmpty rs in rs' /= [] ==> f $ zip rs' $ map (++suffix) rs'
 where f ((key, content):rest) = 
        let fr = foldl (\r (k,c) -> addResult r k c) (makeTrainRun name) rest
            absentAdd = addResult fr key content
            presentAdd = addResult absentAdd name (content ++ suffix)
            presentRemove = removeResult absentAdd name
            absentRemove = removeResult fr name
        in  areEqual [presentRemove, absentRemove, fr] &&
            absentAdd == presentAdd &&
            sameElements (map fst rest) (getResults fr) &&
            sameElements (key: map fst rest) (getResults absentAdd) &&
            (name ++ suffix) == getResult absentAdd name      
        