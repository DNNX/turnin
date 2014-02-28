{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Domain.TermTest where

import Test.Framework
import TestUtils

import Domain.Term
 
{-# ANN module "HLint: ignore Use camelCase" #-}

test_emptyTerm = let n = makeTerm "" in do
 assertEqual "" $ getStartDate n
 assertEqual "" $ getEndDate n
 
prop_emptyTerm name = [""] == applyGets (makeTerm name) [getStartDate, getEndDate]

test_dates = let n = makeTerm "" in do
 assertEqual "start" $ getStartDate $ setStartDate n "start"
 assertEqual "end" $ getEndDate $ setEndDate n "end"
 
prop_dates name start end = 
 let [t] = applySets (makeTerm name) [(setStartDate, start), (setEndDate, end)]
 in  start == getStartDate t &&
     end == getEndDate t   