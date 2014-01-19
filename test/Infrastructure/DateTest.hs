{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Infrastructure.DateTest where

import Test.Framework
import Data.List
import TestUtils

import Infrastructure.Date

{-# ANN module "HLint: ignore Use camelCase" #-}

prop_dateReadAndWriting y mo d h mi = 
 let [year, month, day, hour, minute] = map (fromTrip clamp) 
      [(y,1,9999),(mo,1,12),(d,1,28),(h,0,23),(mi,0,59)]
     first = fromRight $ makeDate year month day hour minute
     second = fromRight $ stringToDate $ show first
     s = intercalate "-" $ map (uncurry pad) [(year,4),(month,2),(day,2),(hour,2),(minute,2)]
     third = fromRight $ stringToDate s
 in  year == getYear first &&
     month == getMonth first &&
     day == getDay first &&
     hour == getHour first &&
     minute == getMinute first && 
     first == second && second == third
  
prop_dateDeltaReadAndWriting y mo d h mi = 
 let [year, month, day, hour, minute] = map (fromTrip clamp) 
      [(y,1,9999),(mo,1,12),(d,1,28),(h,0,23),(mi,0,59)]
     first = fromRight $ makeDateDelta year month day hour minute
     second = fromRight $ stringToDateDelta $ show first
     s = intercalate "-" $ map (uncurry pad) [(year,4),(month,2),(day,2),(hour,2),(minute,2)]
     third = fromRight $ stringToDateDelta s
 in  year == getDYear first &&
     month == getDMonth first &&
     day == getDDay first &&
     hour == getDHour first &&
     minute == getDMinute first && 
     first == second && second == third
 
prop_dateComparison y mo d h mi p = 
 let parts@[year, month, day, hour, minute] = map (fromTrip clamp) 
      [(y,1,9998),(mo,1,11),(d,1,27),(h,0,22),(mi,0,58)]
     posToIncrement = clamp p 0 $ fromIntegral $ length parts
     d1 = makeDate year month day hour minute
     [year',month',day',hour',minute'] = increment posToIncrement parts
     d2 = makeDate year' month' day' hour' minute'
 in  (d1 == d2 && d2 == d1) ||
     (d1 /= d2 && d2 /= d1) ||
     (d1 >= d2 && d2 < d1) ||
     (d1 <= d2 && d1 > d1) ||
     (d1 > d2 && d2 <= d1) ||
     (d1 < d2 && d2 >= d1)
     
test_dateAddition = 
 let f a b c d e = fromRight $ makeDate a b c d e
     g a b c d e = fromRight $ makeDateDelta a b c d e in do
 assertEqual (f 2000 3 1 0 0) $ fromRight $ f 1999 12 31 0 0 `add` g 0 0 61 0 0
 assertEqual (f 1 1 1 1 0)    $ fromRight $ f 1 1 1 0 59     `add` g 0 0 0 0 1
 assertEqual (f 1 1 2 0 0)    $ fromRight $ f 1 1 1 23 0     `add` g 0 0 0 1 0
 assertEqual (f 1 2 1 0 0)    $ fromRight $ f 1 1 31 0 0     `add` g 0 0 1 0 0
 assertEqual (f 2 1 1 0 0)    $ fromRight $ f 1 12 1 0 0     `add` g 0 1 0 0 0
 assertEqual (f 2 1 1 0 0)    $ fromRight $ f 1 1 1 0 0      `add` g 1 0 0 0 0
 let d = f 9999 1 1 0 0; delta = g 1 0 0 0 0; prefix = "Date addition overflow when adding <" 
 assertEqual (Left $ prefix++show delta++"> to <"++show d++">") $ d `add` delta 
 
 assertEqual (f 1999 2 28 0 0) $ fromRight $ f 1999 1 31 0 0 `add` g 0 1 0 0 0
 assertEqual (f 2000 2 29 0 0) $ fromRight $ f 2000 1 31 0 0 `add` g 0 1 0 0 0
 assertEqual (f 1999 4 30 0 0) $ fromRight $ f 1999 3 31 0 0 `add` g 0 1 0 0 0
 assertEqual (f 2004 3 1 0 0) $ fromRight $ f 2004 2 28 0 0 `add` g 0 0 2 0 0
 assertEqual (f 2003 3 1 0 0) $ fromRight $ f 2003 2 28 0 0 `add` g 0 0 1 0 0
      
prop_additionNoOverflow y mo d h mi dy dmo dd dh dmi = 
 let [year, month, day, hour, minute] = map (fromTrip clamp) 
      [(y,1,9998),(mo,1,11),(d,1,27),(h,0,22),(mi,0,58)]
     [dYear, dMonth, dDay, dHour, dMinute] = map (fromTrip clamp) 
      [(dy,1,9998),(dmo,1,11),(dd,1,27),(dh,0,22),(dmi,0,58)]
      
     d1 = fromRight $ makeDate year month day hour minute
     delta = fromRight $ makeDateDelta dYear dMonth dDay dHour dMinute
     d2 = fromRight $ d1 `add` delta
 in  dYear == getDYear delta &&
     dMonth == getDMonth delta && 
     dDay == getDDay delta &&
     dHour == getDHour delta &&
     dMinute == getDMinute delta &&
     
     year + dYear == getYear d2 &&
     month + dMonth == getMonth d2 &&
     day + dDay == getDay d2 &&
     hour + dHour == getHour d2 &&
     minute + dMinute == getMinute d2
     
     