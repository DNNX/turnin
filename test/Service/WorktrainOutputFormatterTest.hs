{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Service.WorktrainOutputFormatterTest where

import Test.Framework
import Service.Utils.FormatterTestUtils

import Service.WorktrainOutputFormatter 

{-# ANN module "HLint: ignore Use camelCase" #-}

prop_stripAccents s = s /= [] ==>
 let (expected, toFormat) = makeAccents s
 in  expected == stripAccents toFormat
  
prop_lineEnd s = s /= [] ==>
 let (expected, toFormat) = makeLineEnds s
 in  expected == formatLineEnds toFormat
 
prop_maxLineWidth s n x y = s /= [] && n /= 0 ==>
 let (maxLineWidth, expected, toFormat) = makeTooLongLines s n x y
 in  expected == wrapLines toFormat (fromIntegral maxLineWidth)
 
prop_repeats s n = s /= [] && n /= 0 ==>
 let (maxNbRepeats, expected, toFormat) = makeRepeatLines s n
 in  expected == formatRepetitions toFormat (fromIntegral  maxNbRepeats)
 
prop_headerAndFooter s k = s /= [] && k /= [] ==>
 let (key, expected, toFormat) = makeHeaderAndFooter s k
 in  expected == addHeaderAndFooter toFormat key
  
prop_merge s = s /= [] ==>
 let (expected, toMerge) = makeMerge s
 in  expected == mergeOutputs toMerge

  