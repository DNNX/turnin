{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Domain.HierarchyUnitTest where

import Test.Framework
import Data.Maybe

import Infrastructure.Node
import Domain.Root
import Domain.Repo
import Domain.Term
import Domain.Course
import Domain.Group

{-# ANN module "HLint: ignore Use camelCase" #-}

test_rootChildren =   childrenCase (make "" :: Root) "repo"
test_repoChildren =   childrenCase (make "" :: Repo) "term"
test_termChildren =   childrenCase (make "" :: Term) "course"
test_courseChildren = childrenCase (make "" :: Course) "group"
test_groupChildren =  childrenCase (make "" :: Group) "project"

childrenCase parent childName =  
 let child = make childName
     absentAdd = addChild parent child
     presentAdd = addChild absentAdd child
     presentRemove = removeChild absentAdd childName
     absentRemove = removeChild parent childName in do
 assertEqual parent presentRemove
 assertEqual parent absentRemove
 assertEqual absentAdd presentAdd
 assertEqual [] $ getChildrenNames parent
 assertEqual [childName] $ getChildrenNames absentAdd
 assertEqual [] $ getChildren parent
 assertEqual [child] $ getChildren absentAdd
 assertEqual True $ isNothing $ getChild parent childName
 assertEqual (Just child) $ getChild absentAdd childName
 