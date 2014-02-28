{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Infrastructure.CsvNodeTest where

import Test.Framework
import Data.List
import Data.Function
import TestUtils
import Control.Arrow

import Infrastructure.Node
import Infrastructure.CsvNode

{-# ANN module "HLint: ignore Use camelCase" #-}

test_emptyCsvNode = assertEqual [] $ getCsv (makeNode "") "key"
prop_emptyCsvNode name k = null (getCsv (makeNode name) k)

test_addRemoveGetSetValues = 
 let n = makeNode ""     
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
          
prop_addRemoveGetSetValues name ps = let ps' = nubBy ((==) `on` fst) ps 
                                         ps'' = map (second g) ps'
                                         g xs = nub $ filter (not.null) $ map (filter (/=',')) xs
                                     in  ps'' /= [] ==> f ps''
 where f ((k,xs):rest) = 
        let (ys,vs) = splitAt (length xs `div` 2) xs 
            n = addCsv (buildCsvNode name rest) k ys
            
            absentAdd = addCsv n k vs
            presentAdd = addCsv absentAdd k vs
            presentRemove = removeCsv absentAdd k vs 
            absentRemove = removeCsv n k vs
            
            absentSet = setCsv n k (vs++ys++vs++ys)
            presentSet = setCsv absentSet k (ys++vs++ys++vs)
            presentUnset = setCsv absentSet k (ys++ys)
            absentUnset = setCsv n k (ys++ys)
            
        in  sameElements (ys++vs) (getCsv absentAdd k) && 
            sameElements ys (getCsv n k) &&
            areEqual [presentRemove, absentRemove, presentUnset, absentUnset, n] &&
            areEqual [absentAdd, presentAdd, presentSet, absentSet]
          
buildCsvNode name ps = let node = makeNode name in f node ps
 where f n [] = n
       f n ((k,vs):rest) = f (setCsv n k vs) rest

       
       
       
       
       
       