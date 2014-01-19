{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Infrastructure.DateTest where

import Test.Framework
import Data.List
import TestUtils

import Infrastructure.Date

{-# ANN module "HLint: ignore Use camelCase" #-}

prop_dateReadAndWrite y mo d h mi = 
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
  
  
prop_dateDeltaReadAndWrite y mo d h mi = 
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
 let d = f 9999 1 1 0 0; delta = g 1 0 0 0 0;
 assertEqual (Left $ "Date addition overflow when adding delta <" ++show delta++"> to date <"++show d++">") $ d `add` delta 
 
 assertEqual (f 1999 2 28 0 0) $ fromRight $ f 1999 1 31 0 0 `add` g 0 1 0 0 0
 assertEqual (f 2000 2 29 0 0) $ fromRight $ f 2000 1 31 0 0 `add` g 0 1 0 0 0
 assertEqual (f 1999 4 30 0 0) $ fromRight $ f 1999 3 31 0 0 `add` g 0 1 0 0 0
 assertEqual (f 2004 3 1 0 0) $ fromRight $ f 2004 2 28 0 0 `add` g 0 0 2 0 0
 assertEqual (f 2003 3 1 0 0) $ fromRight $ f 2003 2 28 0 0 `add` g 0 0 1 0 0
      
      
prop_additionNoOverflow y mo d h mi dy dmo dd dh dmi = 
 let [year, month, day, hour, minute] = map (fromTrip clamp) [(y,1,9998),(mo,1,11),(d,1,27),(h,0,22),(mi,0,58)]
     [dYear, dMonth, dDay, dHour, dMinute] = map (fromTrip clamp) [(dy,0,9999-year),(dmo,0,12-month),(dd,0,28-day),(dh,0,23-hour),(dmi,0,59-minute)]
      
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
      
      
prop_dateAddition y mo d h mi dy dmo dd dh dmi = 
 let [year, month, day, hour, minute] = map (fromTrip clamp) [(y,1,9999),(mo,1,12),(d,1,nbDaysInMonth year month),(h,0,23),(mi,0,59)]
     [dYear, dMonth, dDay, dHour, dMinute] = map (fromTrip clamp) [(dy,0,9999),(dmo,0,99),(dd,0,99),(dh,0,99),(dmi,0,99)]
      
     d1 = fromRight $ makeDate year month day hour minute
     delta = fromRight $ makeDateDelta dYear dMonth dDay dHour dMinute
     
     (year', month', day', hour', minute') = referenceAdd (year,month,day,hour,minute) (dYear,dMonth,dDay,dHour,dMinute)
     either_d2 = d1 `add` delta
    
 in  case either_d2 of
      Left m   -> year' > (9999 :: Integer) && 
                   m == "Date addition overflow when adding delta <"++show delta++"> to date <"++show d++">"
      Right d2 -> year' <= (9999 :: Integer) &&
                   year' == getYear d2 &&
                   month' == getMonth d2 &&
                   day' == getDay d2 &&
                   hour' == getHour d2 &&
                   minute' == getMinute d2
            
            
prop_nbDaysInMonth y d h mi ys =
 let years = nub $ map (\x -> clamp x 1 9999) ys
     [year, hour, minute] = map (fromTrip clamp) [(y,1,9999),(h,0,23),(mi,0,59)]
     dates31Days = map (\month -> makeDate year month (clamp d 1 31) hour minute) [1,3,5,7,8,10,12]
     dates30Days = map (\month -> makeDate year month (clamp d 1 30) hour minute) [4,6,9,11]
     errors31Days = mop (\month -> makeDate year month 31 hour minute) [4,6,9,11]
     
     (leaps, nonLeaps) = partition isLeap years
     leapDates     = map (\year' -> makeDate year' 2 (clamp d 1 29) hour minute) leaps
     nonLeapDates  = map (\year' -> makeDate year' 2 (clamp d 1 28) hour minute) nonLeaps
     
     day1 = clamp d 30 31
     day2 = clamp d 29 31
     leapErrors    = mop (\year' -> makeDate year' 2 day1 hour minute) leaps
     nonLeapErrors = mop (\year' -> makeDate year' 2 day2 hour minute) nonLeaps
     
     mop f = map (\x -> (x, f x))
     
 in  all isRight dates31Days &&
     all isRight dates30Days &&
     all isRight leapDates &&
     all isRight nonLeapDates &&
     all (\(m,date) -> date == Left ("Invalid date <"++toDateString year m 31 hour minute++">, day '31' must be between '01' and '30' for month '"++pad m 2++"' and year '"++pad year 4++"'")) errors31Days &&
     all (\(y',date) -> date == Left ("Invalid date <"++toDateString y' 2 day1 hour minute++">, day '"++pad day1 2++"' must be between '01' and '29' for month '"++pad 2 2++"' and year '"++pad y' 4++"'")) leapErrors &&
     all (\(y',date) -> date == Left ("Invalid date <"++toDateString y' 2 day2 hour minute++">, day '"++pad day2 2++"' must be between '01' and '28' for month '"++pad 2 2++"' and year '"++pad y' 4++"'")) nonLeapErrors
            
            
prop_dateFormatErrors year month day hour minute 
                      dYear dMonth dDay dHour dMinute =
 let xs = [(year,1,9999),(month,1,12),(day,1,28),(hour,0,23),(minute,0,59)]
     ys = [(dYear,0,9999),(dMonth,0,99),(dDay,0,99),(dHour,0,99),(dMinute,0,99)]
     [y, mo, d, h, mi]       = map (fromTrip clamp) xs 
     [dy, dmo, dd, dh, dmi]  = map (fromTrip clamp) ys
     [y',mo',_,h',mi']      = map (fromTrip unclamp) xs
     [dy',dmo',dd',dh',dmi'] = map (fromTrip unclamp) ys 
     d' = unclamp day 1 31
     
 in  errorCase makeDate y' mo d h mi "year" 1 9999 y' 4 "" &&
     errorCase makeDate y mo' d h mi "month" 1 12 mo' 2 "" &&
     errorCase makeDate y mo d' h mi "day" 1 (nbDaysInMonth y mo) d' 2 (" for month '"++pad mo 2++"' and year '"++pad y 4++"'") &&
     errorCase makeDate y mo d h' mi "hour" 0 23 h' 2 "" &&
     errorCase makeDate y mo d h mi' "minute" 0 59 mi' 2 "" &&
     errorCase makeDateDelta dy' dmo dd dh dmi "year" 0 9999 dy' 4 "" &&
     errorCase makeDateDelta dy dmo' dd dh dmi "month" 0 99 dmo' 2 "" &&
     errorCase makeDateDelta dy dmo dd' dh dmi "day" 0 99 dd' 2 "" &&
     errorCase makeDateDelta dy dmo dd dh' dmi "hour" 0 99 dh' 2 "" &&
     errorCase makeDateDelta dy dmo dd dh dmi' "minute" 0 99 dmi' 2 ""
     
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
     
     

     
     
     
     
     
     
     
     
     
     
     
     
     
     
     