{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Infrastructure.CsvNodePropTest where

import Test.Framework
import Data.List
import Data.Function
import TestUtils
import Control.Arrow

import Infrastructure.Node
import Infrastructure.CsvNode

{-# ANN module "HLint: ignore Use camelCase" #-}

prop_emptyCsvNode name k = null (getCsv (make name) k)

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

buildCsvNode name ps = let node = make name in f node ps
 where f n [] = n
       f n ((k,vs):rest) = f (setCsv n k vs) rest






       