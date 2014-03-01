module Infrastructure.Date
( Date()
, DateDelta()
, makeDate
, makeDateDelta
, stringToDate
, stringToDateDelta
, getYear
, getMonth
, getDay
, getHour
, getMinute
, getDYear
, getDMonth
, getDDay
, getDHour
, getDMinute
, add
, nbDaysInMonth
, isLeap
, pad
, readPart
) where

import Control.Monad.Instances()
import Data.List
import Data.List.Split
import Data.Maybe

data Date = D
 { getYear   :: Int
 , getMonth  :: Int
 , getDay    :: Int
 , getHour   :: Int
 , getMinute :: Int
 } deriving (Eq,Ord)

data DateDelta = DD
 { getDYear   :: Int
 , getDMonth  :: Int
 , getDDay    :: Int
 , getDHour   :: Int
 , getDMinute :: Int
 } deriving (Eq, Ord)

type MakeD a = Int -> Int -> Int -> Int -> Int -> a
type MakeEitherD a = Int -> Int -> Int -> Int -> Int -> Either String a
type MakeDParts = (Int,Int,Int,Int,Int)

makeD :: Show a => MakeDParts -> MakeD a -> [a -> Int] -> [(Int,Int,String)] -> Either String a
makeD (y,mo,d,h,mi) f [gy,gmo,gd,gh,gmi] [year,month,day,hour,minute] = let x = f y mo d h mi in do
 check x "year" gy 4 year
 check x "month" gmo 2 month
 check x "day" gd 2 day
 check x "hour" gh 2 hour
 check x "minute" gmi 2 minute
 return x
makeD _ _ _ _ = error "Pattern should not fail"

makeDate :: MakeEitherD Date
makeDate y mo d h mi = makeD (y,mo,d,h,mi) D
                            [getYear,getMonth,getDay,getHour,getMinute]
                            [(1,9999,""),(1,12,""),
                            (1,nbDaysInMonth y mo," for month '"++pad mo 2++"' and year '"++pad y 4++"'"),
                            (0,23,""),(0,59,"")]
makeDateDelta :: MakeEitherD DateDelta
makeDateDelta y mo d h mi = makeD (y,mo,d,h,mi) DD
                            [getDYear,getDMonth,getDDay,getDHour,getDMinute]
                            [(0,9999,""),(0,99,""),(0,99,""),(0,99,""),(0,99,"")]

stringToD :: MakeEitherD a -> String -> Either String a
stringToD f s = let parts = splitOn "-" s
                in  if not (validParts parts) then Left $ "Invalid date <" ++ s ++ "> format is 'yyyy-mm-dd-hh-mm'"
                    else let [y,mo,d,h,mi] = map read parts in  f y mo d h mi

stringToDate :: String -> Either String Date
stringToDate = stringToD makeDate

stringToDateDelta :: String -> Either String DateDelta
stringToDateDelta = stringToD makeDateDelta

showD y mo d h mi = intercalate "-" $ map (uncurry pad) [(y,4),(mo,2),(d,2),(h,2),(mi,2)]
instance Show Date where show (D  y mo d h mi) = showD y mo d h mi
instance Show DateDelta where show (DD y mo d h mi) = showD y mo d h mi

nbDaysInMonth :: Int -> Int -> Int
nbDaysInMonth y m
 | m `elem` [1,3,5,7,8,10,12] = 31
 | m `elem` [4,6,9,11]        = 30
 | isLeap y                   = 29
 | otherwise                  = 28

isLeap y
 | y `mod` 400 == 0 = True
 | y `mod` 100 == 0 = False
 | y `mod` 4 == 0   = True
 | otherwise        = False

add :: Date -> DateDelta -> Either String Date
add date@(D y mo d h mi) delta@(DD dy dmo dd dh dmi) = add' (y,mo,d,h,mi) (dy,dmo,dd,dh,dmi) (date, delta)

-- private utils
check :: Show a => a -> String -> (a -> Int) -> Int -> (Int,Int,String) -> Either String ()
check date name getV size (minV,maxV,rest) = let v = getV date in
 if minV <= v && v <= maxV then Right ()
 else Left $ "Invalid date <"++show date++">, "++name++" '"++pad v size++"' must be between '"++
               pad minV size ++ "' and '" ++
               pad maxV size ++ "'" ++ rest

validParts parts = length parts == 5 && all (isJust.readPart) parts

pad :: Int -> Int -> String
pad v l = let s = show v
              s' = replicate (fromIntegral l- length s) '0'
          in  s' ++ s

readPart ::  String -> Maybe Int
readPart s = case reads s of
              [(x, "")] -> Just x
              _ -> Nothing

add' (y,mo,d,h,mi) (dy,dmo,dd,dh,dmi) r =
 let year = y + dy
     month = mo + dmo
     hour = h + dh
     minute = mi + dmi
 in  addMonth year month (d,dd) hour minute r

addMonth y mo ddd h mi
 | mo > 12  = addMonth (y+1) (mo-12) ddd h mi
 | otherwise = addDay1 y mo ddd h mi

addDay1 y mo (d,dd) = addMinute y mo (d',dd)
 where d' = min d $ nbDaysInMonth y mo

addMinute y mo ddd h mi r
 | mi >= 60  = addMinute  y mo ddd (h+1) (mi-60) r
 | otherwise = addHour y mo ddd h mi r

addHour y mo (d,dd) h mi r
 | h >= 24   = addHour y mo (d+1,dd) (h-24) mi r
 | otherwise = addDay2 y mo (d+dd) h mi (nbDaysInMonth y mo) r


addDay2 y mo d h mi n r
 | d > n = let d' = d - n
               m' = mo + 1
               (y',m'') = if m' > 12 then (y+1,m'-12) else (y,m')
               n' = nbDaysInMonth y' m''
           in  addDay2 y' m'' d' h mi n' r
 | otherwise = addMake y mo d h mi r

addMake y mo d h mi (date, delta)
 | y > 9999  = Left $ "Date addition overflow when adding delta <"++show delta++"> to date <"++show date++">"
 | otherwise = makeDate y mo d h mi









