{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Infrastructure.NodeUnitTest where

import Test.Framework
import Data.Maybe
import Data.List

import Infrastructure.Node
{-# ANN module "HLint: ignore Use camelCase" #-}

test_emptyNode = let n = makeNode "node" in do
 assertEqual "node" $ getName n
 assertEqual "" $ getConfig n "configKey"
 assertEqual "" $ getCache n "cacheKey"
 assertEqual [] $ getCacheKeys n
 assertEqual True $ isNothing $ getChild n "childKey"
 assertEqual [(["node"], n)] $ getKeys n

test_getSetUnsetConfig =
 let n             = makeNode ""
     absentAdd     = setConfig n "key" "v1"
     presentAdd    = setConfig absentAdd "key" "v2"
     presentRemove = unsetConfig absentAdd "key"
     absentRemove  = unsetConfig n "key" in  do
 assertEqual n presentRemove
 assertEqual n absentRemove
 assertEqual "" $ getConfig n "key"
 assertEqual "v1" $ getConfig absentAdd "key"
 assertEqual "v2" $ getConfig presentAdd "key"

test_getSetUnsetCache =
 let n             = makeNode ""
     absentAdd     = setCache n "key" "v1"
     presentAdd    = setCache absentAdd "key" "v2"
     presentRemove = unsetCache absentAdd "key"
     absentRemove  = unsetCache n "key" in do
 assertEqual n presentRemove
 assertEqual n absentRemove
 assertEqual [] $ getCacheKeys n
 assertEqual ["key"] $ getCacheKeys absentAdd
 assertEqual ["key"] $ getCacheKeys presentAdd
 assertEqual "" $ getCache n "key"
 assertEqual "v1" $ getCache absentAdd "key"
 assertEqual "v2" $ getCache presentAdd "key"

test_getSetUnsetChildren =
 let n             = makeNode ""
     c1            = makeNode "child"
     c2'            = setConfig c1 "key" "value"
     absentAdd     = setChild n c1
     presentAdd    = setChild absentAdd c2'
     presentRemove = unsetChild absentAdd "child"
     absentRemove  = unsetChild n "child" in do
 assertEqual n presentRemove
 assertEqual n absentRemove
 assertEqual absentAdd presentAdd
 assertEqual True $ isNothing $ getChild n "child"
 assertEqual (Just c1) $ getChild absentAdd "child"
 assertEqual [] $ getChildren n
 assertEqual [c1] $ getChildren absentAdd
 assertEqual [([""],absentAdd),(["","child"],c1)] $ getKeys absentAdd

buildNodeConfig name ts = let node = makeNode name in f node ts
 where f n [] = n
       f n ((k,v1,_):xs) = f (setConfig n k v1) xs

buildNodeCache name ts = let node = makeNode name in f node ts
 where f n [] = n
       f n ((k,v1,_):xs) = f (setCache n k v1) xs

buildNodeChildren parentName ns = let node = makeNode parentName
                                      f = foldl (\m name -> setChild m (setConfig (makeNode name) name name))
                                  in  f node ns

getKeysModel = f []
 where f parentKey c = let key = parentKey ++ [getName c]
                           rest = concatMap (f key.fromJust.getChild c) $ map getName $ getChildren c
                       in  (key,c):rest

moo f ts = let ts' = nubBy (\(a,_,_)(b,_,_)->a==b) ts
               tss = tails ts'
               tss' = filter (not.null) tss
           in  tss' /= [] ==> all f tss'

first (a,_,_) = a





