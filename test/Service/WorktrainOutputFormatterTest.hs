{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Service.WorktrainOutputFormatterTest where

import Test.Framework
import Service.Utils.FormatterTestUtils

import Service.WorktrainOutputFormatter 
import System.IO.Unsafe
import TestUtils

{-# ANN module "HLint: ignore Use camelCase" #-}

prop_stripAccents s = s /= [] ==>
 let (expected, toFormat) = makeAccents s
 in  expected == stripAccents toFormat
  
prop_lineEnd s = s /= [] ==>
 let (expected, toFormat) = makeLineEnds s
 in  expected == formatLineEnds toFormat
 
prop_maxLineWidth s (Positive n) x y = s /= [] ==>
 let (maxLineWidth, expected, toFormat) = makeTooLongLines s n x y
 in  expected == wrapLines toFormat (fromIntegral maxLineWidth)
 
prop_repeats s (Positive n) =
 let (maxNbRepeats, expected, toFormat) = makeRepeatLines s n
 in  expected == formatRepetitions toFormat (fromIntegral  maxNbRepeats)
  
prop_moo t n' = let n = clampS 3 6 n' in unsafePerformIO $ do
 putStrLn "" 
 print t 
 print $ makeExpected t n
 putStrLn "\n\n\n\n\n"
 return True 
  
prop_headerAndFooter s k = s /= [] && k /= [] ==>
 let (key, expected, toFormat) = makeHeaderAndFooter s k
 in  expected == addHeaderAndFooter toFormat key
  
prop_merge s = s /= [] ==>
 let (expected, toMerge) = makeMerge s
 in  expected == mergeOutputs toMerge
 
   