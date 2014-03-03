{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Infrastructure.NodeUnitTest where

import Test.Framework
import Data.Maybe
import Data.List

import Infrastructure.Node
{-# ANN module "HLint: ignore Use camelCase" #-}

test_emptyNode = let n = make "node" :: Node in do
 assertEqual "node" $ getName n
 assertEqual "" $ getConfig n "configKey"
 assertEqual "" $ getCache n "cacheKey"
 assertEqual [] $ getCacheKeys n
 assertEqual True $ isNothing $ getChild n "childKey"

test_getSetUnsetConfig =
 let n             = make "" :: Node
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
 let n             = make "" :: Node
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
 let n             = make "" :: Node
     c1            = make "child"
     c2'            = setConfig c1 "key" "value"
     absentAdd     = addChild n c1 
     presentAdd    = addChild absentAdd c2'
     presentRemove = removeChild absentAdd "child"
     absentRemove  = removeChild n "child" in do
 assertEqual n presentRemove
 assertEqual n absentRemove
 assertEqual absentAdd presentAdd
 assertEqual True $ isNothing $ getChild n "child"
 assertEqual (Just c1) $ getChild absentAdd "child"
 assertEqual [] $ getChildren n
 assertEqual [c1] $ getChildren absentAdd

buildNodeConfig name ts = let node = make name in f node ts
 where f n [] = n
       f n ((k,v1,_):xs) = f (setConfig n k v1) xs

buildNodeCache name ts = let node = make name in f node ts
 where f n [] = n
       f n ((k,v1,_):xs) = f (setCache n k v1) xs

buildNodeChildren parentName ns = let node = make parentName
                                      f = foldl (\m name -> addChild m (setConfig (make name) name name))
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





