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
        let n             = buildNodeConfig name rest
            absentAdd     = setConfig n key v1
            presentAdd    = setConfig absentAdd key v2
            presentRemove = unsetConfig absentAdd key
            absentRemove  = unsetConfig n key
        in  areEqual [presentRemove, absentRemove, n] &&
            sameElements (map toPair rest) (getConfigPairs n) &&
            sameElements ((key,v1):map toPair rest) (getConfigPairs absentAdd) &&
            sameElements ((key,v2):map toPair rest) (getConfigPairs presentAdd) &&
            "" == getConfig n key && 
            v1 == getConfig absentAdd key &&
            v2 == getConfig presentAdd key

prop_getSetUnsetCache name = moo f . filter (\(a,b,c) -> "" `notElem` [a,b,c])
 where f ((key, v1, v2):rest) =
        let n             = buildNodeCache name rest
            absentAdd     = setCache n key v1
            presentAdd    = setCache absentAdd key v2
            presentRemove = unsetCache absentAdd key
            absentRemove  = unsetCache n key
        in  areEqual [presentRemove, absentRemove, n] &&
            sameElements (map toPair rest) (getCachePairs n) &&
            sameElements ((key,v1):map toPair rest) (getCachePairs absentAdd) &&
            sameElements ((key,v2):map toPair rest) (getCachePairs presentAdd) &&
            "" == getCache n key &&
            v1 == getCache absentAdd key &&
            v2 == getCache presentAdd key 

prop_getSetUnsetChildren parentName ns = let ns' = nub ns
                                             nss = tails ns'
                                             nss' = filter (not.null) nss
                                         in  nss' /= [] ==> all f nss'
 where f names@(name:rest) =
        let n             = buildNodeChildren parentName rest :: Node
            c1            = make name
            c2            = setConfig c1 "key" "value"
            absentAdd     = addChild n c1
            presentAdd    = addChild absentAdd c2
            presentRemove = removeChild absentAdd name
            absentRemove  = removeChild n name
        in areEqual [presentRemove, absentRemove, n] &&
           absentAdd == presentAdd &&
           isNothing (getChild n name) &&
           Just c1 == getChild absentAdd name &&
           sameElements rest (map getName $ getChildren n) &&
           sameElements names (map getName $ getChildren absentAdd)

buildNodeConfig name ts = let node = make name in f node ts
 where f n [] = n
       f n ((k,v1,_):xs) = f (setConfig n k v1) xs

buildNodeCache name ts = let node = make name in f node ts
 where f n [] = n
       f n ((k,v1,_):xs) = f (setCache n k v1) xs

buildNodeChildren parentName ns = let node = make parentName
                                      f = foldl (\m name -> addChild m (setConfig (make name) name name))
                                  in  f node ns

moo f ts = let ts' = nubBy (\(a,_,_)(b,_,_)->a==b) ts
               tss = tails ts'
               tss' = filter (not.null) tss
           in  tss' /= [] ==> all f tss'

first (a,_,_) = a
toPair (x,y,_) = (x,y)





