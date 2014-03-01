{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Domain.HierarchyUnitTest where

import Test.Framework
import Data.Maybe

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
 