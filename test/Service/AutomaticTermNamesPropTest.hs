{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Service.AutomaticTermNamesPropTest where

import Test.Framework
import TestUtils
import Infrastructure.Date
import Data.List

import Service.AutomaticTermNames

{-# ANN module "HLint: ignore Use camelCase" #-}

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



                                                                