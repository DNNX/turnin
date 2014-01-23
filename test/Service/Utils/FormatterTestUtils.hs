module Service.Utils.FormatterTestUtils where

import TestUtils
import Data.Char
import Service.Accents
import Service.AsciiArt

import Test.Framework
import Control.Applicative
import Data.List

type AsciiString = [Int]
toAsciiString :: AsciiString -> String
toAsciiString = map (chr.clampS 32 126)

makeAccents :: AsciiString -> (String, String)
makeAccents s = let expected = toAsciiString s
                    toFormat = g 0 $ map toAccents expected
                    g _ []       = []
                    g i (xs:xss) = (xs !! (i `mod` length xs)) : g (i+1) xss
                in  (expected, toFormat)
    
makeLineEnds :: [AsciiString] -> (String, String)
makeLineEnds ls = let the_lines = map toAsciiString ls
                      expected = unlines the_lines
                      toFormat = concatMap f $ zip the_lines $ cycle ["\r\n","\n"]
                      f (line, sep) = let (li,ne) = splitAt (length line `div` 2) line in  li ++ "\r" ++ ne ++ sep
                  in  (expected, toFormat)          

makeTooLongLines :: AsciiString -> Int -> Int -> Int -> (Int, String, String)
makeTooLongLines s n' x' y' = let n = clampS 1 10 n'
                                  x = clampS 1 10 x'
                                  y = clampS 1 10 y'
                                  the_lines = getLineComponents n (map (clampS 32 126) s) x y
                                  expected = unlines $ concat the_lines
                                  toFormat = unlines $ map concat the_lines
                              in  (n,expected,toFormat)
                              
getLineComponents :: Int -> AsciiString -> Int -> Int -> [[String]]                        
getLineComponents n s x y = map (map(map chr).f) [1..  x]
 where f i = map g [1..y] ++ if i `mod` 2 /= 0 then [] else [getFrom i (i`mod`n) s]
              where g j = getFrom (i*j) n s 
              
getFrom :: Int -> Int -> [a] -> [a]              
getFrom i n s = take n $ drop (i*n) $ cycle s                              

makeRepeatLines :: RepeatTree -> Int -> (Int, String, String)
makeRepeatLines = undefined

makeHeaderAndFooter :: String -> String -> (String, String, String)
makeHeaderAndFooter k toFormat =
 let key = filter (`elem` (['a'..'z']++['A'..'Z']++['0'..'9']++"_")) k
     expected = applyHeaderFooter key toFormat
 in  (key, expected, toFormat)
  
applyHeaderFooter key toFormat = unlines [toAsciiArt key, key, begin, toFormat, end]
 where begin = " ============ output begin ============ "
       end   = " ============  output end  ============ " 

makeMerge :: [String] -> (String, [String])
makeMerge toMerge = (unlines toMerge, toMerge)

data RepeatTree = Leaf String | Node [(Int, RepeatTree)] deriving (Show,Eq)

instance Arbitrary RepeatTree where 
 arbitrary = sized arbTree
  where arbTree :: Int -> Gen RepeatTree
        arbTree 1 = Leaf <$> (fmap toAsciiString arbitrary)
        arbTree n = do
         let n' = 1 + n `div` 8
         ms <- arbitrary
         ts <- mapM arbTree [1..n'] 
         return $ Node $ zip (map (clampS 2 10) ms) $ nub ts
         
 shrink (Leaf s)  = map Leaf $ shrink s
 shrink (Node ys) = map snd ys

makeExpected :: RepeatTree -> Int -> String
makeExpected t n = unlines $ makeExpectedLines t n

makeToFormat :: RepeatTree -> String
makeToFormat = unlines . makeLines


makeExpectedLines :: RepeatTree -> Int -> [String]
makeExpectedLines (Leaf s) _ = [s]
makeExpectedLines (Node xs) n = f xs
 where f :: [(Int,RepeatTree)] -> [String]
       f [] = []
       f ((m,t):ys) = let previous_lines = (makeExpectedLines t n)
                          n_repeats =  concat (replicate (min m n) previous_lines)
                          maybe_truncation = if m > n then repeatMessage (length previous_lines) (m-n) else [] 
                          rest = f ys
                      in  n_repeats ++ maybe_truncation ++ rest

makeLines :: RepeatTree -> [String]
makeLines (Leaf s) = [s]
makeLines (Node xs) = f xs
 where f :: [(Int,RepeatTree)] -> [String]
       f [] = []
       f ((n,t):ys) =  concat (replicate n (makeLines t)) ++ f ys
      

repeatMessage nbLines nbTimes = let ls = makePlural "line" nbLines
                                    ts = makePlural "time" nbTimes 
                                in  ["[The previous "++show nbLines++" "++ls++" repeat "++show nbTimes++" more "++ts++"]"] 

makePlural s n
 | n < 0 = error "Plural of a negative number ["++show n++"] and string ["++s++"]"
 | n < 2 = s
 | otherwise = s ++ "s"

