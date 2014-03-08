{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Infrastructure.DateUnitTest where

import Test.Framework
import Data.List
import TestUtils

import Infrastructure.Date
import Control.Monad

{-# ANN module "HLint: ignore Use camelCase" #-}
{-# ANN module "HLint: ignore Reduce duplication" #-}

test_dateReadAndWrite = do
 forM_ [(1,1,1,0,0),(9999,12,31,23,59),(5000,6,15,12,30)] $ \(year, month, day, hour, minute) -> do
   let Right first = makeDate year month day hour minute
       Right second = stringToDate $ show first
   assertEqual year $ getYear first
   assertEqual month $ getMonth first
   assertEqual day $ getDay first
   assertEqual hour $ getHour first
   assertEqual minute $ getMinute first
   assertEqual first  second
 assertEqual (stringToDate "0001-01-01-00-00") $ makeDate 1 1 1 0 0
 assertEqual (stringToDate "9999-12-31-23-59") $ makeDate 9999 12 31 23 59
 assertEqual (stringToDate "1234-6-15-12-30") $ makeDate 1234 6 15 12 30

test_dateDeltaReadAndWrite = do
 forM_ [(0,0,0,0,0),(9999,99,99,99,99),(5000,50,50,50,50)] $ \(year, month, day, hour, minute) -> do
   let Right first = makeDateDelta year month day hour minute
       Right second = stringToDateDelta $ show first
   assertEqual year $ getDYear first
   assertEqual month $ getDMonth first
   assertEqual day $ getDDay first
   assertEqual hour $ getDHour first
   assertEqual minute $ getDMinute first
   assertEqual first  second
 assertEqual (stringToDateDelta "0000-00-00-00-00") $ makeDateDelta 0 0 0 0 0
 assertEqual (stringToDateDelta "9999-99-99-99-99") $ makeDateDelta 9999 99 99 99 99
 assertEqual (stringToDateDelta "5000-50-50-50-50") $ makeDateDelta 5000 50 50 50 50

test_dateComparison =
 forM_ [(1,1,1,0,0),(9998,11,27,22,58),(5000,6,15,12,30)] $ \(y, mo, d, h, mi) ->
   forM_ [(1,0,0,0,0),(0,1,0,0,0),(0,0,1,0,0),(0,0,0,1,0),(0,0,0,0,1)] $ \(dy, dmo, dd, dh, dmi) -> do
     let Right d1 = makeDate y mo d h mi
         Right d2 = makeDate (y+dy) (mo+dmo) (d+dd) (h+dh) (mi+dmi)
     assertBool $ (d1 == d2 && d2 == d1) ||
                  (d1 /= d2 && d2 /= d1) ||
                  (d1 >= d2 && d2 < d1) ||
                  (d1 <= d2 && d2 > d1) ||
                  (d1 > d2 && d2 <= d1) ||
                  (d1 < d2 && d2 >= d1)

test_dateDeltaComparison =
 forM_ [(1,1,1,0,0),(9998,98,98,98,98),(5000,50,50,50,50)] $ \(y, mo, d, h, mi) ->
   forM_ [(1,0,0,0,0),(0,1,0,0,0),(0,0,1,0,0),(0,0,0,1,0),(0,0,0,0,1)] $ \(dy, dmo, dd, dh, dmi) -> do
     let Right d1 = makeDateDelta y mo d h mi
         Right d2 = makeDateDelta (y+dy) (mo+dmo) (d+dd) (h+dh) (mi+dmi)
     assertBool $ (d1 == d2 && d2 == d1) ||
                  (d1 /= d2 && d2 /= d1) ||
                  (d1 >= d2 && d2 < d1) ||
                  (d1 <= d2 && d2 > d1) ||
                  (d1 > d2 && d2 <= d1) ||
                  (d1 < d2 && d2 >= d1)

test_additionNoOverflow =
 forM_ [(1,1,1,0,0),(9998,11,27,22,58),(5000,6,15,12,30)] $ \(y, mo, d, h, mi) ->
   forM_ [(1,0,0,0,0),(0,1,0,0,0),(0,0,1,0,0),(0,0,0,1,0),(0,0,0,0,1)] $ \(dy, dmo, dd, dh, dmi) -> do
     let Right d1 = makeDate y mo d h mi
         Right d2 = d1 `add` fromRight (makeDateDelta dy dmo dd dh dmi)
     assertEqual (y + dy) $ getYear d2
     assertEqual (mo + dmo) $ getMonth d2
     assertEqual (d + dd) $ getDay d2
     assertEqual (h + dh) $ getHour d2
     assertEqual (mi + dmi) $ getMinute d2

test_dateAddition =
 let f a b c d e = fromRight $ makeDate a b c d e
     g a b c d e = fromRight $ makeDateDelta a b c d e in do
 assertEqual (f 2000 3 1 0 0) $ fromRight $ f 1999 12 31 0 0 `add` g 0 0 61 0 0
 assertEqual (f 1 1 1 1 0)    $ fromRight $ f 1 1 1 0 59     `add` g 0 0 0 0 1
 assertEqual (f 1 1 2 0 0)    $ fromRight $ f 1 1 1 23 0     `add` g 0 0 0 1 0
 assertEqual (f 1 2 1 0 0)    $ fromRight $ f 1 1 31 0 0     `add` g 0 0 1 0 0
 assertEqual (f 2 1 1 0 0)    $ fromRight $ f 1 12 1 0 0     `add` g 0 1 0 0 0
 assertEqual (f 2 1 1 0 0)    $ fromRight $ f 1 1 1 0 0      `add` g 1 0 0 0 0
 let d = f 9999 1 1 0 0; delta = g 1 0 0 0 0;
 assertEqual (Left $ "Date addition overflow when adding delta <" ++show delta++"> to date <"++show d++">") $ d `add` delta

 assertEqual (f 1999 2 28 0 0) $ fromRight $ f 1999 1 31 0 0 `add` g 0 1 0 0 0
 assertEqual (f 2000 2 29 0 0) $ fromRight $ f 2000 1 31 0 0 `add` g 0 1 0 0 0
 assertEqual (f 1999 4 30 0 0) $ fromRight $ f 1999 3 31 0 0 `add` g 0 1 0 0 0
 assertEqual (f 2004 3 1 0 0) $ fromRight $ f 2004 2 28 0 0 `add` g 0 0 2 0 0
 assertEqual (f 2003 3 1 0 0) $ fromRight $ f 2003 2 28 0 0 `add` g 0 0 1 0 0

test_nbDaysInMonth = do
 forM_ [1,3,5,7,8,10,12] $ \mo -> assertBool $ isRight $ makeDate 2000 mo 31 00 00
 forM_ [4,6,9,11] $ \mo -> do
   assertBool $ isRight $ makeDate 2003 mo 30 00 00
   assertBool $ isLeft $ makeDate 2003 mo 31 00 00
 assertBool $ isRight $ makeDate 2003 2 28 00 00
 assertBool $ isLeft $ makeDate 2003 2 29 00 00
 assertBool $ isRight $ makeDate 2004 2 29 00 00
 assertBool $ isLeft $ makeDate 2004 2 30 00 00

test_dateFormatErrors = do
 assertBool $ errorCase makeDate 0     6 15 12 30 "year" 1 9999 0 4 ""
 assertBool $ errorCase makeDate 10000 6 15 12 30 "year" 1 9999 10000 4 ""
 assertBool $ errorCase makeDate 5000 0  15 12 30 "month" 1 12 0 2 ""
 assertBool $ errorCase makeDate 5000 13 15 12 30 "month" 1 12 13 2 ""
 assertBool $ errorCase makeDate 5000 6 0  12 30 "day" 1 30 0 2 " for month '06' and year '5000'"
 assertBool $ errorCase makeDate 5000 6 31 12 30 "day" 1 30 31 2 " for month '06' and year '5000'"
 assertBool $ errorCase makeDate 5000 6 15 (-1) 30 "hour" 0 23 (-1) 2 ""
 assertBool $ errorCase makeDate 5000 6 15 24 30 "hour" 0 23 24 2 ""
 assertBool $ errorCase makeDate 5000 6 15 12 (-1) "minute" 0 59 (-1) 2 ""
 assertBool $ errorCase makeDate 5000 6 15 12 60 "minute" 0 59 60 2 ""

test_dateDeltaFormatErrors = do
 assertBool $ errorCase makeDateDelta (-1)  50   50   50   50   "year"   0 9999 (-1)  4 ""
 assertBool $ errorCase makeDateDelta 10000 50   50   50   50   "year"   0 9999 10000 4 ""
 assertBool $ errorCase makeDateDelta 5000  (-1) 50   50   50   "month"  0 99   (-1)  2 ""
 assertBool $ errorCase makeDateDelta 5000  100  50   50   50   "month"  0 99   100   2 ""
 assertBool $ errorCase makeDateDelta 5000  50   (-1) 50   50   "day"    0 99   (-1)  2 ""
 assertBool $ errorCase makeDateDelta 5000  50   100  50   50   "day"    0 99   100   2 ""
 assertBool $ errorCase makeDateDelta 5000  50   50   (-1) 50   "hour"   0 99   (-1)  2 ""
 assertBool $ errorCase makeDateDelta 5000  50   50   100  50   "hour"   0 99   100   2 ""
 assertBool $ errorCase makeDateDelta 5000  50   50   50   (-1) "minute" 0 99   (-1)  2 ""
 assertBool $ errorCase makeDateDelta 5000  50   50   50   100  "minute" 0 99   100   2 ""

errorCase f y mo d h mi name minV maxV v size rest = let date = f y mo d h mi in
 case date of
  Right _ -> False
  Left m -> m == ("Invalid date <"++toDateString y mo d h mi ++">, "++name++
                  " '"++pad v size++"' must be between '"++pad minV size++"' and '"++pad maxV size++"'" ++ rest)

toDateString y mo d h mi = intercalate "-" (map (uncurry pad) [(y,4),(mo,2),(d,2),(h,2),(mi,2)])

referenceAdd (y,mo,d,h,mi) (dy,dmo,dd,dh,dmi) =
 let year = y + dy
     month = mo + dmo
     hour = h + dh
     minute = mi + dmi
 in  referenceAdd_month year month (d,dd) hour minute

referenceAdd_month y mo ddd h mi
 | mo > 12  = referenceAdd_month (y+1) (mo-12) ddd h mi
 | otherwise = referenceAdd_day1 y mo ddd h mi

referenceAdd_day1 y mo (d,dd)= referenceAdd_minute y mo (d',dd)
 where d' = min d $ nbDaysInMonth y mo

referenceAdd_minute y mo ddd h mi
 | mi >= 60  = referenceAdd_minute  y mo ddd (h+1) (mi-60)
 | otherwise = referenceAdd_hour y mo ddd h mi

referenceAdd_hour y mo (d,dd) h mi
 | h >= 24   = referenceAdd_hour y mo (d+1,dd) (h-24) mi
 | otherwise = referenceAdd_day2 y mo (d+dd) h mi (nbDaysInMonth y mo)


referenceAdd_day2 y mo d h mi n
 | d > n = let d' = d - n
               m' = mo + 1
               (y',m'') = if m' > 12 then (y+1,m'-12) else (y,m')
               n' = nbDaysInMonth y' m''
           in  referenceAdd_day2 y' m'' d' h mi n'
 | otherwise = (y,mo,d,h,mi)