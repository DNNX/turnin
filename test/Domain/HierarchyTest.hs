{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Domain.HierarchyTest where
import Test.Framework

import Data.List
import Data.Maybe

import Domain.Root
import Domain.Repo
import Domain.Term
import Domain.Course
import Domain.Group
import Domain.Project
 
{-# ANN module "HLint: ignore Use camelCase" #-}

prop_rootChildrenr rs = let repoNames = filter (not.null) $ nub rs
                        in  repoNames /= [] ==> f repoNames
 where f names@(repoName:rest) =  
        let root = foldl (\x n -> addRepo x (makeRepo n)) makeRoot rest
            r = makeRepo repoName 
            addAbsent = addRepo root r
            addPresent = addRepo addAbsent r
            removePresent = removeRepo addAbsent repoName
            removeAbsent = removeRepo root repoName
        in  (removePresent, removeAbsent) == (root,root) &&
            addAbsent == addPresent &&
            null (rest \\ getRepos root) && null (getRepos root \\ rest) &&  
            null (names \\ getRepos addAbsent) && null (getRepos addAbsent \\ names) &&  
            isNothing (getRepo root repoName) &&
            Just r == getRepo addAbsent repoName 
             
prop_repoChildren repoName ts = let termNames = filter (not.null) $ nub ts
                                in  termNames /= [] ==> f termNames
 where f names@(termName:rest) =  
        let r = foldl (\ x n -> addTerm x (makeTerm n)) (makeRepo repoName) rest
            t = makeTerm termName
            addAbsent = addTerm r t
            addPresent = addTerm addAbsent t
            removePresent = removeTerm addAbsent termName
            removeAbsent = removeTerm r termName
        in  (removePresent, removeAbsent) == (r,r) &&
            addAbsent == addPresent &&
            null (rest \\ getTerms r) && null (getTerms r \\ rest) &&  
            null (names \\ getTerms addAbsent) && null (getTerms addAbsent \\ names) &&  
            isNothing (getTerm r termName) &&
            Just t == getTerm addAbsent termName
            
prop_termChildren termName cs = let courseNames = filter (not.null) $ nub cs
                                in  courseNames /= [] ==> f courseNames
 where f names@(courseName:rest) =   
        let t = foldl (\ x n -> addCourse x (makeCourse n)) (makeTerm termName) rest
            c = makeCourse courseName
            addAbsent = addCourse t c
            addPresent = addCourse addAbsent c 
            removePresent = removeCourse addAbsent courseName
            removeAbsent = removeCourse t courseName
        in  (removePresent, removeAbsent) == (t,t) &&
            addAbsent == addPresent &&
            null (rest \\ getCourses t) && null (getCourses t \\ rest) &&  
            null (names \\ getCourses addAbsent) && null (getCourses addAbsent \\ names) &&  
            isNothing (getCourse t courseName) &&
            Just c == getCourse addAbsent courseName
            
prop_courseChildren courseName gs = let groupNames = filter (not.null) $ nub gs
                                    in  groupNames /= [] ==> f groupNames
 where f names@(groupName:rest) =  
        let c = foldl (\ x n -> addGroup x (makeGroup n)) (makeCourse courseName) rest
            g = makeGroup groupName
            addAbsent = addGroup c g
            addPresent = addGroup addAbsent g
            removePresent = removeGroup addAbsent groupName
            removeAbsent = removeGroup c groupName
        in  (removePresent, removeAbsent) == (c,c) &&
            addAbsent == addPresent &&
            null (rest \\ getGroups c) && null (getGroups c \\ rest) &&  
            null (names \\ getGroups addAbsent) && null (getGroups addAbsent \\ names) &&  
            isNothing (getGroup c groupName) &&
            Just g == getGroup addAbsent groupName     

prop_groupChildren groupName ps = let projectNames = filter (not.null) $ nub ps
                                  in  projectNames /= [] ==> f projectNames
 where f names@(projectName:rest) =  
        let g = foldl (\ x n -> addProject x (makeProject n)) (makeGroup groupName) rest
            p = makeProject projectName
            addAbsent = addProject g p
            addPresent = addProject addAbsent p
            removePresent = removeProject addAbsent projectName
            removeAbsent = removeProject g projectName
        in  (removePresent, removeAbsent) == (g,g) &&
            addAbsent == addPresent &&
            null (rest \\ getProjects g) && null (getProjects g \\ rest) &&  
            null (names \\ getProjects addAbsent) && null (getProjects addAbsent \\ names) &&  
            isNothing (getProject g projectName) && 
            Just p == getProject addAbsent projectName       
               
