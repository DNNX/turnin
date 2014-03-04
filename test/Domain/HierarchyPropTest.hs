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

prop_rootChildren   name = childrenCase (make name :: Root)
prop_repoChildren   name = childrenCase (make name :: Repo)
prop_termChildren   name = childrenCase (make name :: Term)
prop_courseChildren name = childrenCase (make name :: Course)
prop_groupChildren  name = childrenCase (make name :: Group)
            
childrenCase p ns = let ns' = uniqueNonEmpty ns in  ns' /= [] ==> f ns'
 where f names@(childName:rest) =
        let (parent, children) = buildFamily p rest
            child = make childName
            absentAdd = addChild parent child
            presentAdd = addChild absentAdd child
            presentRemove = removeChild absentAdd childName
            absentRemove = removeChild parent childName
        in  areEqual [presentRemove, absentRemove, parent] &&
            absentAdd == presentAdd &&
            sameElements rest (map getName $ getChildren parent) &&
            sameElements names (map getName $ getChildren absentAdd) &&
            sameElements children (getChildren parent) &&
            sameElements (child:children) (getChildren absentAdd) &&
            isNothing (getChild parent childName) &&
            Just child == getChild absentAdd childName
            
buildFamily parent = f (parent,[]) 
 where f acc    []           = acc
       f (p,cs) (name:names) = let child = make name
                               in  f (addChild p child, child:cs) names