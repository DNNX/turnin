{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Service.AutomaticTermNamesTest where

import Test.Framework
import TestUtils
import Infrastructure.Date

import Service.AutomaticTermNames 

{-# ANN module "HLint: ignore Use camelCase" #-}

test_getTermNamesToCheck = let fr = fromRight in do
 assertEqual [ ("2013-1",fr $ makeDate 2013 1 1 0 0,fr $ makeDate 2013 5 1 0 0)
             , ("2013-2",fr $ makeDate 2013 5 1 0 0,fr $ makeDate 2013 9 1 0 0)
             , ("2013-3",fr $ makeDate 2013 9 1 0 0,fr $ makeDate 2014 1 1 0 0)
             , ("2014-1",fr $ makeDate 2014 1 1 0 0,fr $ makeDate 2014 5 1 0 0) ]
             
             (make 4 (fromRight $ makeDate 2013 2 1 00 00) $ map fr
              [ makeDateDelta 0 1 1 0 0
              , makeDateDelta 0 5 1 0 0
              , makeDateDelta 0 9 1 0 0 ])
                                              
 assertEqual True (all isAutoTerm ["2013-1","2013-2","2013-3"])
 assertEqual False (any isAutoTerm ["20131","2013_2","2013-c", "1234--1"])

                 


                       
                                                                