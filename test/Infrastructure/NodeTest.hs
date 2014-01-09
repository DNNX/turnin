{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Infrastructure.NodeTest where
import Test.Framework
 
import Infrastructure.Node
import Data.Maybe
import Data.List
{-# ANN module "HLint: ignore Use camelCase" #-}

prop_emptyNode name k = 
 let n = makeNode name
 in  "" == getConfig n k &&
    isNothing (getChild n k) &&
     name      == getName n &&
     [([name], n)] == getKeys n
      
prop_getSetUnsetConfig name key v1 v2 = 
 let n             = makeNode name
     absentAdd     = setConfig n key v1
     presentAdd    = setConfig absentAdd key v2
     presentRemove = unsetConfig absentAdd key
     absentRemove  = unsetConfig n key
 in  (presentRemove, absentRemove) == (n,n) &&
     "" == getConfig n key && 
     v1 == getConfig absentAdd key &&
     v2 == getConfig presentAdd key
      
prop_getSetUnsetChildren parentName name = 
 let n             = makeNode parentName
     c1            = makeNode name 
     c2            = setConfig c1 "key" "value"
     absentAdd     = setChild n c1
     presentAdd    = setChild absentAdd c2
     presentRemove = unsetChild presentAdd name
     absentRemove  = unsetChild n name
 in True &&  
     (presentRemove, absentRemove) == (n,n) && 
     absentAdd == presentAdd &&
     isNothing (getChild n name) &&
     Just c1 == getChild absentAdd name &&
     name    == getName c1 &&
     [([parentName], absentAdd), ([parentName,name], c1)] \\ getKeys absentAdd == []
