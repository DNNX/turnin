{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Interface.CommandLineParserTest where
import Test.Framework

import Interface.CommandLineLexicon
import Interface.CommandLineParserTestUtils
import Interface.CommandLineParser

{-# ANN module "HLint: ignore Use camelCase" #-}

prop_configThresholdSetSuccess cu ch =
 validArgs [cu,ch] ==>
  testCmd (cu,ch) h [configSub, thresholdSub, setSub] ([currentOpt, chooseOpt], [cu, ch])
   where h (Global(
            Config(ConfigOpts(
             ConfigThreshold(ConfigThresholdOpts(
              ConfigThresholdSet(ConfigThresholdSetOpts a b))))))) = (a,b)





