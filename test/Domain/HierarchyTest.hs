{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Domain.HierarchyTest where

import Test.Framework
import Data.Maybe
import TestUtils

import Domain.Root
import Domain.Repo
import Domain.Term
import Domain.Course 
import Domain.Group
import Domain.Project
 
{-# ANN module "HLint: ignore Use camelCase" #-}

test_rootChildren =
 let root = makeRoot ""
     r = makeRepo "repo" 
     absentAdd = addRepo root r
     presentAdd = addRepo absentAdd r
     presentRemove = removeRepo absentAdd "repo"
     absentRemove = removeRepo root "repo" in do
 assertEqual root presentRemove
 assertEqual root absentRemove
 assertEqual absentAdd presentAdd
 assertEqual [] $ getRepos root
 assertEqual ["repo"] $ getRepos absentAdd
 assertEqual True $ isNothing $ getRepo root "repo"
 assertEqual (Just r) $ getRepo absentAdd "repo" 

prop_rootChildren name rs = let repoNames = uniqueNonEmpty rs
                             in  repoNames /= [] ==> f repoNames
 where f names@(repoName:rest) =  
        let root = foldl (\x n -> addRepo x (makeRepo n)) (makeRoot name) rest
            r = makeRepo repoName 
            absentAdd = addRepo root r
            presentAdd = addRepo absentAdd r
            presentRemove = removeRepo absentAdd repoName
            absentRemove = removeRepo root repoName
        in  areEqual [presentRemove, absentRemove, root] &&
            absentAdd == presentAdd &&
            sameElements rest (getRepos root) && 
            sameElements names (getRepos absentAdd) &&
            isNothing (getRepo root repoName) &&
            Just r == getRepo absentAdd repoName
            
test_repoChildren =
 let repo = makeRepo ""
     t = makeTerm "term" 
     absentAdd = addTerm repo t
     presentAdd = addTerm absentAdd t
     presentRemove = removeTerm absentAdd "term"
     absentRemove = removeTerm repo "term" in do
 assertEqual repo presentRemove
 assertEqual repo absentRemove
 assertEqual absentAdd presentAdd
 assertEqual [] $ getTerms repo
 assertEqual ["term"] $ getTerms absentAdd
 assertEqual True $ isNothing $ getTerm repo "term"
 assertEqual (Just t) $ getTerm absentAdd "term"             
             
prop_repoChildren repoName ts = let termNames = uniqueNonEmpty ts
                                in  termNames /= [] ==> f termNames
 where f names@(termName:rest) =  
        let r = foldl (\x n -> addTerm x (makeTerm n)) (makeRepo repoName) rest
            t = makeTerm termName
            absentAdd = addTerm r t
            presentAdd = addTerm absentAdd t
            presentRemove = removeTerm absentAdd termName
            absentRemove = removeTerm r termName
        in  areEqual [presentRemove, absentRemove, r] &&
            absentAdd == presentAdd &&
            sameElements rest (getTerms r) && 
            sameElements names (getTerms absentAdd) &&
            isNothing (getTerm r termName) &&
            Just t == getTerm absentAdd termName
                
test_termChildren =
 let term = makeTerm ""
     c = makeCourse "course" 
     absentAdd = addCourse term c
     presentAdd = addCourse absentAdd c
     presentRemove = removeCourse absentAdd "course"
     absentRemove = removeCourse term "course" in do
 assertEqual term presentRemove
 assertEqual term absentRemove
 assertEqual absentAdd presentAdd
 assertEqual [] $ getCourses term
 assertEqual ["course"] $ getCourses absentAdd
 assertEqual True $ isNothing $ getCourse term "course"
 assertEqual (Just c) $ getCourse absentAdd "course"        
        
prop_termChildren termName cs = let courseNames = uniqueNonEmpty cs
                                in  courseNames /= [] ==> f courseNames
 where f names@(courseName:rest) =   
        let t = foldl (\x n -> addCourse x (makeCourse n)) (makeTerm termName) rest
            c = makeCourse courseName
            absentAdd = addCourse t c
            presentAdd = addCourse absentAdd c 
            presentRemove = removeCourse absentAdd courseName
            absentRemove = removeCourse t courseName
        in  areEqual [presentRemove, absentRemove, t] &&
            absentAdd == presentAdd &&
            sameElements rest (getCourses t) && 
            sameElements names (getCourses absentAdd) &&
            isNothing (getCourse t courseName) &&
            Just c == getCourse absentAdd courseName
                      
test_courseChildren =
 let course = makeCourse ""
     g = makeGroup "group" 
     absentAdd = addGroup course g
     presentAdd = addGroup absentAdd g
     presentRemove = removeGroup absentAdd "group"
     absentRemove = removeGroup course "group" in do
 assertEqual course presentRemove
 assertEqual course absentRemove
 assertEqual absentAdd presentAdd
 assertEqual [] $ getGroups course
 assertEqual ["group"] $ getGroups absentAdd
 assertEqual True $ isNothing $ getGroup course "group"
 assertEqual (Just g) $ getGroup absentAdd "group"        
 
prop_courseChildren courseName gs = let groupNames = uniqueNonEmpty gs
                                    in  groupNames /= [] ==> f groupNames
 where f names@(groupName:rest) =  
        let c = foldl (\x n -> addGroup x (makeGroup n)) (makeCourse courseName) rest
            g = makeGroup groupName
            absentAdd = addGroup c g
            presentAdd = addGroup absentAdd g
            presentRemove = removeGroup absentAdd groupName
            absentRemove = removeGroup c groupName
        in  areEqual [presentRemove, absentRemove, c] &&
            absentAdd == presentAdd &&
            sameElements rest (getGroups c) && 
            sameElements names (getGroups absentAdd) &&
            isNothing (getGroup c groupName) &&
            Just g == getGroup absentAdd groupName     
                      
test_groupChildren =
 let grou = makeGroup ""
     p = makeProject "project" 
     absentAdd = addProject grou p
     presentAdd = addProject absentAdd p
     presentRemove = removeProject absentAdd "project"
     absentRemove = removeProject grou "project" in do
 assertEqual grou presentRemove
 assertEqual grou absentRemove
 assertEqual absentAdd presentAdd
 assertEqual [] $ getProjects grou
 assertEqual ["project"] $ getProjects absentAdd
 assertEqual True $ isNothing $ getProject grou "project"
 assertEqual (Just p) $ getProject absentAdd "project"        
 
prop_groupChildren groupName ps = let projectNames = uniqueNonEmpty ps
                                  in  projectNames /= [] ==> f projectNames
 where f names@(projectName:rest) =  
        let g = foldl (\x n -> addProject x (makeProject n)) (makeGroup groupName) rest
            p = makeProject projectName
            absentAdd = addProject g p
            presentAdd = addProject absentAdd p
            presentRemove = removeProject absentAdd projectName
            absentRemove = removeProject g projectName
        in  areEqual [presentRemove, absentRemove, g] &&
            absentAdd == presentAdd &&
            sameElements rest (getProjects g) && 
            sameElements names (getProjects absentAdd) &&
            isNothing (getProject g projectName) && 
            Just p == getProject absentAdd projectName       
               