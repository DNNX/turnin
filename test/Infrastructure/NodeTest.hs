{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Infrastructure.NodeTest where
import Test.Framework
 
import Data.Maybe
import Data.List
import TestUtils
import System.IO.Unsafe

import Infrastructure.Node
{-# ANN module "HLint: ignore Use camelCase" #-}

prop_emptyNode name k = 
 let n = makeNode name
 in  "" == getConfig n k &&
     "" == getCache n k &&
     [] == getCacheKeys n &&
    isNothing (getChild n k) &&
     name == getName n &&
     [([name], n)] == getKeys n
      
prop_getSetUnsetConfig name = moo f
 where f ((key, v1, v2):rest) =                               
        let n             = buildNodeConfig name rest
            absentAdd     = setConfig n key v1
            presentAdd    = setConfig absentAdd key v2
            presentRemove = unsetConfig absentAdd key
            absentRemove  = unsetConfig n key 
        in  areEqual [presentRemove, absentRemove, n] &&
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
            sameElements (map first rest) (getCacheKeys n) &&
            sameElements (filter (not.null) $ key:map first rest) (getCacheKeys absentAdd) &&
            sameElements (filter (not.null) $ key:map first rest) (getCacheKeys presentAdd) &&
            "" == getCache n key && 
            v1 == getCache absentAdd key &&
            v2 == getCache presentAdd key
      
prop_getSetUnsetChildren parentName ns = let ns' = nub ns
                                             nss = tails ns'
                                             nss' = filter (not.null) nss
                                         in  nss' /= [] ==> all f nss'
 where f names@(name:rest) =  
        let n             = buildNodeChildren parentName rest
            c1            = makeNode name 
            c2            = setConfig c1 "key" "value"
            absentAdd     = setChild n c1
            presentAdd    = setChild absentAdd c2
            presentRemove = unsetChild absentAdd name
            absentRemove  = unsetChild n name
        in areEqual [presentRemove, absentRemove, n] && 
           absentAdd == presentAdd &&
           isNothing (getChild n name) &&
           Just c1 == getChild absentAdd name &&
           name    == getName c1 &&
           sameElements rest (getChildren n) &&
           sameElements names (getChildren absentAdd) &&
           sameElements (getKeysModel absentAdd) (getKeys absentAdd)
           
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
                           rest = concatMap (f key.fromJust.getChild c) $ getChildren c
                       in  (key,c):rest

moo f ts = let ts' = nubBy (\(a,_,_)(b,_,_)->a==b) ts
               tss = tails ts'
               tss' = filter (not.null) tss
           in  tss' /= [] ==> all f tss'
           
first (a,_,_) = a



                               
                                     
