module Service.Utils.FormatterTestUtils where

import TestUtils
import Data.Char
import Service.Accents
import Service.AsciiArt

import Test.Framework
import Data.List

import Control.Arrow ((***))

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

makeTooLongLines :: AsciiString -> Int -> [(Int,Int)] -> (Int, String, String)
makeTooLongLines as n' ys = let s = toAsciiString as
                                n = clampS 1 10 n'
                                xs = map (clampS 1 10 *** clampS 0 (n - 1)) ys
                                parts = makeParts s n xs
                                expected = unlines $ concat parts
                                toFormat = unlines $ map concat parts
                            in  (n, expected, toFormat)

makeParts :: String -> Int -> [(Int,Int)] -> [[String]]
makeParts s' n' xs' = filter (not.null) $ reverse $ map reverse $ f [[]] s' n' xs'
 where f acc    _  _ []         = acc
       f acc    [] _ _          = acc
       f acc    s  n ((0,0):xs) = f acc s n xs
       f (l:ls) s  n ((0,b):xs) = let (begin,rest) = splitAt b s in f ([]:(begin:l):ls) rest n xs
       f (l:ls) s  n ((a,b):xs) = let (begin,rest) = splitAt n s in f ((begin:l):ls) rest n ((a-1,b):xs)
       f _      _  _ _          = error "Should not get here"

getLineComponents :: Int -> AsciiString -> Int -> Int -> [[String]]
getLineComponents n s x y = map (map(map chr).f) [1..  x]
 where f i = map g [1..y] ++ if i `mod` 2 /= 0 then [] else [getFrom i (i`mod`n) s]
              where g j = getFrom (i*j) n s

getFrom :: Int -> Int -> [a] -> [a]
getFrom i n s = take n $ drop (i*n) $ cycle s

makeRepeatLines :: Tree -> Int -> (String, String)
makeRepeatLines t n = (unlines $ makeExpectedLines n t, unlines $ makeToFormatLines t)

makeHeaderAndFooter :: String -> String -> (String, String, String)
makeHeaderAndFooter toFormat k =
 let key = filter (`elem` (['a'..'z']++['A'..'Z']++['0'..'9']++"_")) k
     expected = applyHeaderFooter key toFormat
 in  (key, expected, toFormat)

applyHeaderFooter key toFormat = unlines [toAsciiArt key, key, begin, toFormat, end]
 where begin = " ============ output begin ============ "
       end   = " ============  output end  ============ "

makeMerge :: [String] -> (String, [String])
makeMerge toMerge = (unlines toMerge, toMerge)

data Tree = L String | N Int [Tree] deriving (Show,Eq)

instance Arbitrary Tree where
 arbitrary = sized arbTree
  where arbTree :: Int -> Gen Tree
        arbTree 1 = do
         NonEmpty l <- arbitrary
         return $ L $ toAsciiString l
        arbTree n = do
         let n' = 1 + n `div` 16
         ms <- arbitrary
         ts <- mapM arbTree [1..n']
         return $ N (clampS 2 10 ms) $ nub ts

 shrink (L s)  = map L $ shrink s
 shrink (N _ xs) = xs

makeExpectedLines :: Int -> Tree -> [String]
makeExpectedLines _ (L s) = [s]
makeExpectedLines n (N nb xs) = let ls = concatMap (makeExpectedLines n) xs
                                    n_repeats = concat (replicate (min n nb) ls)
                                    maybe_truncation = if nb > n then repeatMessage (length ls) (nb-n) else []
                                in  n_repeats ++ maybe_truncation

makeToFormatLines :: Tree -> [String]
makeToFormatLines (L s) = [s]
makeToFormatLines (N n xs) = concat $ replicate n $ concatMap makeToFormatLines xs


repeatMessage nbLines nbTimes = let ls = makePlural "line" nbLines
                                    ts = makePlural "time" nbTimes
                                in  ["[The previous "++show nbLines++" "++ls++" repeat "++show nbTimes++" "++ts++"]"]

makePlural s n
 | n < 0 = error "Plural of a negative number ["++show n++"] and string ["++s++"]"
 | n < 2 = s
 | otherwise = s ++ "s"












