{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Infrastructure.CsvNodeTest where
import Test.Framework

import Infrastructure.Node
import Infrastructure.CsvNode
 
import Data.List
import Data.Function

{-# ANN module "HLint: ignore Use camelCase" #-}

prop_emptyCsvNode name k = null (getCsv (makeNode name) k)
      
prop_addRemoveGetSetValues name ps = let ps' = nubBy ((==) `on` fst) ps 
                                         ps'' = map (\(x,vs) -> (x,nub $ filter (not.null) $ map (filter (/=',')) vs)) ps'
                                     in  ps'' /= [] ==> f ps''
 where f ((k,vs):rest) = 
        let n = buildCsvNode name rest
            absentAdd = addAll n k vs
            presentAdd = addAll absentAdd k vs
            presentRemove = removeAll absentAdd k vs
            absentRemove = removeAll n k vs
            
            absentSet = setCsv n k vs
            presentSet = setCsv absentSet k vs
            presentUnset = setCsv absentSet k []
            absentUnset = setCsv n k []
        in  [presentRemove, absentRemove, presentUnset, absentUnset] == replicate 4 n &&
            [absentAdd, presentAdd, presentSet] == replicate 3 absentSet
          
buildCsvNode name ps = let node = makeNode name in f node ps
 where f n [] = n
       f n ((k,vs):rest) = f (setCsv n k vs) rest
         
addAll n _ [] = n
addAll n k (x:xs) = addAll (addCsv n k x) k xs

removeAll n _ [] = n
removeAll n k (x:xs) = removeAll (removeCsv n k x) k xs