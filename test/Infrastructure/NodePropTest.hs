{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Infrastructure.NodePropTest where

import Test.Framework
import Data.Maybe
import Data.List
import TestUtils

import Infrastructure.Node
{-# ANN module "HLint: ignore Use camelCase" #-}

prop_emptyNode name k =
 let n = make name
 in  "" == getConfig n k &&
     [] == getConfigPairs n &&
     "" == getCache n k &&
     [] == getCachePairs n &&
    isNothing (getChild n k) &&
    [] == getChildren n &&
     name == getName n

prop_getSetUnsetConfig name = moo f . filter (\(a,b,c) -> "" `notElem` [a,b,c])
 where f ((key, v1, v2):rest) = 
        let n             = buildConfig (make name) rest
            absentAdd     = setConfig n key v1
            presentAdd    = setConfig absentAdd key v2
            presentRemove = unsetConfig absentAdd key
            absentRemove  = unsetConfig n key
        in  areEqual [presentRemove, absentRemove, n, setConfig n "" v1] &&
            sameElements (map toPair rest) (getConfigPairs n) &&
            sameElements ((key,v1):getConfigPairs n) (getConfigPairs absentAdd) &&
            sameElements ((key,v2):getConfigPairs n) (getConfigPairs presentAdd) &&
            "" == getConfig n key && 
            v1 == getConfig absentAdd key &&
            v2 == getConfig presentAdd key

prop_getSetUnsetCache name = moo f . filter (\(a,b,c) -> "" `notElem` [a,b,c])
 where f ((key, v1, v2):rest) =
        let n             = buildCache (make name) rest
            absentAdd     = setCache n key v1
            presentAdd    = setCache absentAdd key v2
            presentRemove = unsetCache absentAdd key
            absentRemove  = unsetCache n key
        in  areEqual [presentRemove, absentRemove, n,  setCache n "" v1] &&
            sameElements (map toPair rest) (getCachePairs n) &&
            sameElements ((key,v1):getCachePairs n) (getCachePairs absentAdd) &&
            sameElements ((key,v2):getCachePairs n) (getCachePairs presentAdd) &&
            "" == getCache n key &&
            v1 == getCache absentAdd key &&
            v2 == getCache presentAdd key 

prop_getSetUnsetChildren parentName = runOnUniqueTails f
 where f names@(name:rest) =
        let n             = buildChildren (make parentName) rest :: Node
            c1            = make name
            c2            = setConfig c1 "key" "value"
            absentAdd     = addChild n c1
            presentAdd    = addChild absentAdd c2
            presentRemove = removeChild absentAdd name
            absentRemove  = removeChild n name
        in areEqual [presentRemove, absentRemove, n, addChild n (buildChildren (make "") rest)] &&
           absentAdd == presentAdd &&
           isNothing (getChild n name) &&
           Just c1 == getChild absentAdd name &&
           sameElements rest (map getName $ getChildren n) &&
           sameElements names (map getName $ getChildren absentAdd)

prop_cacheAndChildrenShareKeys parentName suffix = runOnUniqueTails f
 where f (name:rest) =
        let (cacheKeys, childrenNames) = splitAt (length rest `div` 2) rest
            cachePairs     = zip cacheKeys $ map (++suffix) cacheKeys
            n0             = buildCache (buildChildren (make parentName) childrenNames) $ map (\(a,b) -> (a,b,b)) cachePairs
            c              = make name
            n1             = addChild n0 c
            n1'            = setCache n1 name $ name ++ suffix
            n2             = setCache n0 name $ name ++ suffix
            n2'            = addChild n2 c
        in n1 == n1' && n2  == n2' &&
           "" == getCache n1 name &&
           (name++suffix) ==  getCache n2 name &&
           Just c == getChild n1 name &&
           isNothing (getChild n2 name) &&
           sameElements (c:getChildren n0) (getChildren n1) &&
           sameElements (getChildren n0)   (getChildren n2) &&
           sameElements cachePairs                       (getCachePairs n1) &&
           sameElements ((name,name++suffix):cachePairs) (getCachePairs n2)

buildConfig = foldl f
 where f acc (k,v1,_) = setConfig acc k v1

buildCache = foldl f
 where f acc (k,v1,_) = setCache acc k v1

buildChildren = foldl f
 where f acc name = addChild acc (setConfig (make name) name name)

moo f ts = let ts' = nubBy (\(a,_,_)(b,_,_)->a==b) ts
               tss = tails ts'
               tss' = filter (not.null) tss
           in  tss' /= [] ==> all f tss'

first (a,_,_) = a
toPair (x,y,_) = (x,y)


runOnUniqueTails f ns = let ns' = filter (/= "") $ nub ns
                            nss = tails ns'
                            nss' = filter (not.null) nss
                        in  nss' /= [] ==> all f nss'


