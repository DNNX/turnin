{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Service.AutomaticTermNamesTest where

import Test.Framework
import TestUtils
import Infrastructure.Date
import Data.List

import Service.AutomaticTermNames 

{-# ANN module "HLint: ignore Use camelCase" #-}

test_makeTermNames = let fr = fromRight in do
 assertEqual [ ("2013-1",fr $ makeDate 2013 1 1 0 0,fr $ makeDate 2013 5 1 0 0)
             , ("2013-2",fr $ makeDate 2013 5 1 0 0,fr $ makeDate 2013 9 1 0 0)
             , ("2013-3",fr $ makeDate 2013 9 1 0 0,fr $ makeDate 2014 1 1 0 0)
             , ("2014-1",fr $ makeDate 2014 1 1 0 0,fr $ makeDate 2014 5 1 0 0) ]
             
             (make 4 (fromRight $ makeDate 2013 2 1 00 00) $ map fr 
              [ makeDateDelta 0 1 1 0 0
              , makeDateDelta 0 5 1 0 0
              , makeDateDelta 0 9 1 0 0 ])
                                              
 assertEqual True (all isAutoTerm ["2013-1","2013-2","2013-3"])
 assertEqual False (any isAutoTerm ["20131","2013_2","2013-c", "1234--1", "1234-0", "0000-1", "10000-1"])

prop_makeTermNames y qs n' = length qs >= 2 ==>
 let n = clampS 1 100 n'
     year = clampS 21 5000 y
     (t:d':ts') = sort $ map toDateDelta qs
     ts = (t:ts')
     d = toDate year d' 
     toDateDelta (mo,day,h,mi) = fromRight $ makeDateDelta 0 (clampS 1 12 mo) (clampS 1 28 day) (clampS 0 23 h) (clampS 0 59 mi)
     expected = makeExpected year ts
     actual = make n d ts
 in  take (fromIntegral n) expected == actual &&
     all (isAutoTerm.(\(x,_,_)->x)) actual
 
 
toDate year delta = fromRight $ makeDate year (getDMonth delta) (getDDay delta) (getDHour delta) (getDMinute delta) 
 
makeExpected _    [] = []
makeExpected year ts = let ds = map (toDate year) ts
                           f xs = xs ++ map incYear (f xs)
                           incYear d = fromRight (d `add` fromRight (makeDateDelta 1 0 0 0 0))
                           ps = zip [0..] $ f ds
                           toDNN ((i,d1),(_,d2)) = (makeName year i (fromIntegral $ length ts),d1,d2)
                       in  zipWith (curry toDNN) ps $ tail ps
 
makeName y i n = let (q,r) = i `quotRem` n 
                     year = y + q
                     j = r + 1
                 in  pad year 4 ++ "-" ++ show j     


                       
                                                                