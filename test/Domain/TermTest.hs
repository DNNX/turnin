{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Domain.TermTest where
import Test.Framework

import TestUtils

import Domain.Term
 
{-# ANN module "HLint: ignore Use camelCase" #-}

prop_emptyTerm name = [""] == applyGets (makeTerm name) [getStartDate, getEndDate]
  
prop_dates name start end = 
 let [t] = applySets (makeTerm name) [(setStartDate, start), (setEndDate, end)]
 in  start == getStartDate t &&
     end == getEndDate t   