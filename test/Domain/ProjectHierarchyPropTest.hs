{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Domain.ProjectHierarchyPropTest where

import Test.Framework
import TestUtils
import Data.Maybe

import Infrastructure.Node
import Domain.Project
import Domain.ProjectRepo
import Domain.SubmitRepo
import Domain.TrainFileRepo
import Domain.TrainRun

{-# ANN module "HLint: ignore Use camelCase" #-}

prop_projectChildren projectName submitRepo submit trainFileRepo trainFile trainRunRepo trainRun =
 length (filter (not.null) [submitRepo,submit,trainFileRepo,trainFile,trainRunRepo,trainRun]) == 6 ==>
 let p = make projectName
     s = addSubmit emptySubmitRepo submit submit
     tf = addTrainFile emptyTrainFileRepo trainFile trainFile
     tr = addChild emptyTrainRunRepo $ make trainRun
 in  emptySubmitRepo == getSubmitRepo p &&
     emptyTrainFileRepo == getTrainFileRepo p &&
     emptyTrainRunRepo == getTrainRunRepo p &&
     s == getSubmitRepo (setSubmitRepo p s) &&
     tf == getTrainFileRepo (setTrainFileRepo p tf) &&
     tr == getTrainRunRepo (setTrainRunRepo p tr) &&
    [makeProjectSubmitRepo emptySubmitRepo,  makeProjectTrainFileRepo emptyTrainFileRepo, makeProjectTrainRunRepo emptyTrainRunRepo] == getChildren p &&
    [makeProjectSubmitRepo s,                makeProjectTrainFileRepo tf,                 makeProjectTrainRunRepo tr               ] == getChildren (setSubmitRepo (setTrainFileRepo (setTrainRunRepo p tr) tf) s) &&
                                                                                                                                        
    [makeProjectSubmitRepo s,                makeProjectTrainFileRepo emptyTrainFileRepo, makeProjectTrainRunRepo emptyTrainRunRepo] == getChildren (setSubmitRepo p s) &&
    [makeProjectSubmitRepo emptySubmitRepo,  makeProjectTrainFileRepo tf,                 makeProjectTrainRunRepo emptyTrainRunRepo] == getChildren (setTrainFileRepo p tf) &&
    [makeProjectSubmitRepo emptySubmitRepo,  makeProjectTrainFileRepo emptyTrainFileRepo, makeProjectTrainRunRepo tr               ] == getChildren (setTrainRunRepo p tr)        
     

prop_trainRunRepoChildren rs = let trainRunDates = uniqueNonEmpty rs
                               in  trainRunDates /= [] ==> f trainRunDates
 where f dates@(trainRunDate:rest) =
        let trr = foldl (\x d -> addChild x (make d)) emptyTrainRunRepo rest
            tr = make trainRunDate
            absentAdd = addChild trr tr
            presentAdd = addChild absentAdd tr 
            presentRemove = removeChild absentAdd trainRunDate
            absentRemove = removeChild trr trainRunDate
        in  areEqual [presentRemove, absentRemove, trr] &&
            absentAdd == presentAdd &&
            sameElements rest (map getName $ getChildren trr) &&
            sameElements dates (map getName $ getChildren absentAdd) &&
            isNothing (getChild trr trainRunDate) &&
            Just tr == getChild absentAdd trainRunDate

prop_emptySubmitRepo = [[]] == applyGets emptySubmitRepo [getSubmits, getLateSubmits]
prop_emptyTrainFileRepo = [] == getTrainFiles emptyTrainFileRepo
prop_emptyTrainRunRepo = [] == getChildren emptyTrainRunRepo

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

prop_addRemoveGetTrainRuns name rs suffix = let rs' = uniqueNonEmpty rs in rs' /= [] ==> f $ zip rs' $ map (++suffix) rs'
 where f ((key, content):rest) =
        let fr = foldl (\r (k,c) -> addResult r k c) (make name) rest
            absentAdd = addResult fr key content
            presentAdd = addResult absentAdd key (content ++ suffix)
            presentRemove = removeResult absentAdd key
            absentRemove = removeResult fr key
        in  areEqual [presentRemove, absentRemove, fr] &&
            getResult absentAdd key ++ suffix == getResult presentAdd key &&
            sameElements (map fst rest) (getResults fr) &&
            sameElements (key: map fst rest) (getResults absentAdd) &&
            content == getResult absentAdd key

        