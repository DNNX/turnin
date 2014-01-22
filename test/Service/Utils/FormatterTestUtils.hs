module Service.Utils.FormatterTestUtils where

import TestUtils
import Data.Char
import Service.Accents
import Service.AsciiArt

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

makeRepeatLines :: String -> Int -> (Int, String, String)
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



