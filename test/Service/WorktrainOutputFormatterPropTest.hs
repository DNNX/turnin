{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Service.WorktrainOutputFormatterPropTest where

import Test.Framework
import Service.Utils.FormatterTestUtils

import Service.WorktrainOutputFormatter
import TestUtils

{-# ANN module "HLint: ignore Use camelCase" #-}

prop_stripAccents s = s /= [] ==>
 let (expected, toFormat) = makeAccents s
 in  expected == stripAccents toFormat

prop_lineEnd s = s /= [] ==>
 let (expected, toFormat) = makeLineEnds s
 in  expected == formatLineEnds toFormat

prop_maxLineWidth s (Positive n) xs = s /= [] ==>
 let (maxLineWidth, expected, toFormat) = makeTooLongLines s n xs
     result = wrapLines toFormat (fromIntegral maxLineWidth)
 in  expected == result

prop_headerAndFooter s k = s /= [] && k /= [] ==>
 let (key, expected, toFormat) = makeHeaderAndFooter s k
     actual = addHeaderAndFooter toFormat key
 in  expected == actual

prop_merge s = s /= [] ==>
 let (expected, toMerge) = makeMerge s
 in  expected == mergeOutputs toMerge

prop_repeats s (Positive n') = let n = clampS 2 10 n' in
 let (expected, toFormat) = makeRepeatLines s n
 in  expected == formatRepetitions toFormat (fromIntegral  n)
  