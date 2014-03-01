{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Domain.TermUnitTest where

import Test.Framework

import Infrastructure.Node
import Domain.Term

{-# ANN module "HLint: ignore Use camelCase" #-}

test_emptyTerm = let n = make "" in do
 assertEqual "" $ getStartDate n
 assertEqual "" $ getEndDate n

test_dates = let n = make "" in do
 assertEqual "start" $ getStartDate $ setStartDate n "start"
 assertEqual "end" $ getEndDate $ setEndDate n "end"
