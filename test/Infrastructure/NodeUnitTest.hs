{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Infrastructure.NodeUnitTest where

import Test.Framework
import Data.Maybe

import Infrastructure.Node
{-# ANN module "HLint: ignore Use camelCase" #-}

test_emptyNode = let n = make "node" :: Node in do
 assertEqual "node" $ getName n
 assertEqual [] $ getConfigPairs n
 assertEqual "" $ getConfig n "configKey"
 assertEqual [] $ getCachePairs n
 assertEqual "" $ getCache n "cacheKey"
 assertEqual True $ isNothing $ getChild n "childKey"

test_getSetUnsetConfig =
 let n             = make "" :: Node
     absentAdd     = setConfig n "key" "v1"
     presentAdd    = setConfig absentAdd "key" "v2" 
     presentRemove = unsetConfig absentAdd "key"
     absentRemove  = unsetConfig n "key" in  do
 assertEqual n presentRemove
 assertEqual n absentRemove
 assertEqual n $ setConfig n "" "value"
 assertEqual [] $ getConfigPairs n
 assertEqual [("key","v1")] $ getConfigPairs absentAdd
 assertEqual [("key","v2")] $ getConfigPairs presentAdd
 assertEqual "" $ getConfig n "key"
 assertEqual "v1" $ getConfig absentAdd "key"
 assertEqual "v2" $ getConfig presentAdd "key"

test_getSetUnsetCache =
 let n             = make "" :: Node
     absentAdd     = setCache n "key" "v1"
     presentAdd    = setCache absentAdd "key" "v2"
     presentRemove = unsetCache absentAdd "key"
     absentRemove  = unsetCache n "key" in do
 assertEqual n presentRemove
 assertEqual n absentRemove
 assertEqual n $ setCache n "" "value"
 assertEqual [] $ getCachePairs n
 assertEqual [("key","v1")] $ getCachePairs absentAdd 
 assertEqual [("key","v2")] $ getCachePairs presentAdd
 assertEqual "" $ getCache n "key"
 assertEqual "v1" $ getCache absentAdd "key"
 assertEqual "v2" $ getCache presentAdd "key"

test_getSetUnsetChildren =
 let n             = make "" :: Node
     c1            = make "child"
     c2            = setConfig c1 "key" "value"
     absentAdd     = addChild n c1 
     presentAdd    = addChild absentAdd c2
     presentRemove = removeChild absentAdd "child"
     absentRemove  = removeChild n "child" in do
 assertEqual n presentRemove
 assertEqual n absentRemove
 assertEqual n $ addChild n $ make ""
 assertEqual absentAdd presentAdd
 assertEqual Nothing $ getChild n "child"
 assertEqual (Just c1) $ getChild absentAdd "child"
 assertEqual [] $ getChildren n
 assertEqual [c1] $ getChildren absentAdd

test_cacheAndChildrenShareKeys =
 let n0              = make "" :: Node
     c              = make "key"
     n1             = addChild n0 c
     n1'            = setCache n1 "key" "value"
     n2             = setCache n0 "key" "value"
     n2'            = addChild n2 c in do
 assertEqual n1 n1'
 assertEqual n2 n2'
 assertEqual "" $ getCache n1 "key"
 assertEqual [] $ getCachePairs n1
 assertEqual (Just c) $ getChild n1 "key"
 assertEqual [c] $ getChildren n1
 assertEqual "value" $ getCache n2 "key"
 assertEqual [("key","value")] $ getCachePairs n2
 assertEqual Nothing $ getChild n2 "key"
 assertEqual [] $ getChildren n2
     
