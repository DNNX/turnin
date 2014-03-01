{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Domain.HierarchyPropTest where

import Test.Framework
import Data.Maybe
import TestUtils

import Infrastructure.Node
import Domain.Root
import Domain.Repo
import Domain.Term
import Domain.Course
import Domain.Group

{-# ANN module "HLint: ignore Use camelCase" #-}

prop_rootChildren name rs = let repoNames = uniqueNonEmpty rs
                             in  repoNames /= [] ==> f repoNames
 where f names@(repoName:rest) =
        let root = foldl (\x n -> addRepo x (make n)) (make name) rest
            r = make repoName
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

prop_repoChildren repoName ts = let termNames = uniqueNonEmpty ts
                                in  termNames /= [] ==> f termNames
 where f names@(termName:rest) =
        let r = foldl (\x n -> addTerm x (make n)) (make repoName) rest
            t = make termName
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

prop_termChildren termName cs = let courseNames = uniqueNonEmpty cs
                                in  courseNames /= [] ==> f courseNames
 where f names@(courseName:rest) =
        let t = foldl (\x n -> addCourse x (make n)) (make termName) rest
            c = make courseName
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

prop_courseChildren courseName gs = let groupNames = uniqueNonEmpty gs
                                    in  groupNames /= [] ==> f groupNames
 where f names@(groupName:rest) =
        let c = foldl (\x n -> addGroup x (make n)) (make courseName) rest
            g = make groupName
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

prop_groupChildren groupName ps = let projectNames = uniqueNonEmpty ps
                                  in  projectNames /= [] ==> f projectNames
 where f names@(projectName:rest) =
        let g = foldl (\x n -> addProject x (make n)) (make groupName) rest
            p = make projectName
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
               