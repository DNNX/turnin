{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Domain.TermPropTest where

import Test.Framework
import TestUtils

import Infrastructure.Node
import Domain.Term

{-# ANN module "HLint: ignore Use camelCase" #-}

prop_emptyTerm name = [""] == applyGets (make name) [getStartDate, getEndDate]

prop_dates name start end =
 let [t] = applySets (make name) [(setStartDate, start), (setEndDate, end)]
 in  start == getStartDate t &&
     end == getEndDate t   