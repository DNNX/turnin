{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Infrastructure.NodeTest where
import Test.Framework
 
import Data.Maybe
import Data.List
import TestUtils

import Infrastructure.Node
{-# ANN module "HLint: ignore Use camelCase" #-}

prop_emptyNode name k = 
 let n = makeNode name
 in  "" == getConfig n k &&
    isNothing (getChild n k) &&
     name == getName n &&
     [([name], n)] == getKeys n
      
prop_getSetUnsetConfig name ts = let ts' = nubBy (\(a,_,_)(b,_,_)->a==b) ts
                                     tss = tails ts'
                                     tss' = filter (not.null) tss
                                 in  tss' /= [] ==> all f tss'
 where f ((key, v1, v2):rest) =                               
        let n             = buildNodeConfig name rest
            absentAdd     = setConfig n key v1
            presentAdd    = setConfig absentAdd key v2
            presentRemove = setConfig absentAdd key ""
            absentRemove  = setConfig n key ""
        in  areEqual [presentRemove, absentRemove, n] &&
            "" == getConfig n key && 
            v1 == getConfig absentAdd key &&
            v2 == getConfig presentAdd key
      
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
            presentRemove = unsetChild presentAdd name
            absentRemove  = unsetChild n name
        in areEqual [presentRemove, absentRemove, n] && 
           absentAdd == presentAdd &&
           isNothing (getChild n name) &&
           Just c1 == getChild absentAdd name &&
           name    == getName c1 &&
           sameElements rest (getChildren n) &&
           sameElements names (getChildren n) &&
           sameElements [([parentName], absentAdd), ([parentName,name], c1)] (getKeys absentAdd)
           
           
buildNodeConfig name ts = let node = makeNode name in f node ts
 where f n [] = n
       f n ((k,v1,_):xs) = f (setConfig n k v1) xs
       
buildNodeChildren parentName ns = let node = makeNode parentName in f node ns
 where f n [] = n
       f n (name:rest) = let c = makeNode name
                             c' = setConfig c name name
                         in  f (setChild n c') rest       
                                     
