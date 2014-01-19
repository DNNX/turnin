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
) where

import Control.Monad.Instances()
import Data.List
import Data.List.Split
import Data.Maybe

data Date = D 
 { getYear   :: Integer
 , getMonth  :: Integer
 , getDay    :: Integer
 , getHour   :: Integer
 , getMinute :: Integer
 } deriving (Eq,Ord)
 
data DateDelta = DD
 { getDYear   :: Integer
 , getDMonth  :: Integer
 , getDDay    :: Integer
 , getDHour   :: Integer
 , getDMinute :: Integer
 } deriving Eq
 
makeDate :: Integer -> Integer -> Integer -> Integer -> Integer -> Either String Date 
makeDate y mo d h mi = do  
 year <- check "year" 1 9999 y 4 
 month <- check "month" 1 12 mo 2
 day <- check "day" 1 (nbDaysInMonth y mo) d 2
 hour <- check "hour" 0 23 h 2
 minute <- check "minute" 0 59 mi 2
 return $ D year month day hour minute
 
makeDateDelta :: Integer -> Integer -> Integer -> Integer -> Integer -> Either String DateDelta
makeDateDelta dy dmo dd dh dmi = do
 dYear <- check "delta year" 0 9999 dy 4 
 dMonth <- check "delta month" 0 99 dmo 2
 dDay <- check "delta day" 0 99 dd 2
 dHour <- check "delta hour" 0 99 dh 2
 dMinute <- check "delta minute" 0 99 dmi 2
 return $ DD dYear dMonth dDay dHour dMinute

stringToDate :: String -> Either String Date
stringToDate s = let parts = splitOn "-" s
                 in  if not (validParts parts) then Left $ "Invalid date <" ++ s ++ "> format is 'yyyy-mm-dd-hh-mm'"
                     else let [y,mo,d,h,mi] = map read parts in  makeDate y mo d h mi

stringToDateDelta :: String -> Either String DateDelta
stringToDateDelta s = let parts = splitOn "-" s
                      in  if not (validParts parts) then Left $ "Invalid date delta <" ++ s ++ "> format is 'yyyy-mm-dd-hh-mm'"
                          else let [y,mo,d,h,mi] = map read parts in  makeDateDelta y mo d h mi

instance Show Date where
 show (D y mo d h mi) = intercalate "-" $ map (uncurry pad) [(y,4),(mo,2),(d,2),(h,2),(mi,2)]

instance Show DateDelta where
 show (DD y mo d h mi) = intercalate "-" $ map (uncurry pad) [(y,4),(mo,2),(d,2),(h,2),(mi,2)]

add :: Date -> DateDelta -> Either String Date
add = error "Not implemented: Date.add"

-- private utils
check :: String -> Integer -> Integer -> Integer -> Integer -> Either String Integer
check label minV maxV v padSize = 
 if minV <= v && v <= maxV 
 then Right v
 else Left $ "Invalid date " ++ label ++ ", value <" ++ show v ++ "> must be between <" ++ 
               pad minV padSize ++ "> and <" ++ 
               pad maxV padSize ++ ">"

validParts parts = length parts == 5 && all (isJust.readPart) parts

nbDaysInMonth :: Integer -> Integer -> Integer
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


pad :: Integer -> Integer -> String
pad v l = let s = show v
              s' = replicate (fromIntegral l- length s) '0'
          in  s' ++ s

readPart ::  String -> Maybe Integer
readPart s = case reads s of
              [(x, "")] -> Just x
              _ -> Nothing





