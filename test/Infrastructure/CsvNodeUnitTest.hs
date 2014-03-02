{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Infrastructure.CsvNodeUnitTest where

import Test.Framework

import Infrastructure.Node
import Infrastructure.CsvNode

{-# ANN module "HLint: ignore Use camelCase" #-}

test_emptyCsvNode = assertEqual [] $ getCsv (make "") "key"

test_addRemoveGetSetValues =
 let n = make ""
     absentAdd = addCsv n "k" ["v1", "v2"]
     presentAdd = addCsv absentAdd "k" ["v1", "v2"]
     presentRemove = removeCsv absentAdd "k" ["v1", "v2"]
     absentRemove = removeCsv n "k" ["v2", "v1"]

     absentSet = setCsv n "k" ["v1", "v2"]
     presentSet = setCsv absentSet "k" ["v1", "v2"]
     presentUnset = setCsv absentSet "k" []
     absentUnset = setCsv n "k" [] in do
 assertEqual [] $ getCsv n "k"
 assertEqual ["v1", "v2"] $ getCsv absentAdd "k"
 assertEqual n absentRemove
 assertEqual n presentRemove
 assertEqual n absentUnset
 assertEqual n presentUnset
 assertEqual absentAdd presentAdd
 assertEqual absentAdd absentSet
 assertEqual absentAdd presentSet





       