{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Interface.CommandLineParserTest where
import Test.Framework

import Interface.CommandLineLexicon
import Interface.CommandLineParserTestUtils
import Interface.CommandLineParser

{-# ANN module "HLint: ignore Use camelCase" #-}

prop_configThresholdSetSuccess cu ch =
 validArgs [cu, ch] ==> 
  testSuccess (cu,ch) h [configSub, thresholdSub, setSub] ([configThresholdSetCurrentOpt, configThresholdSetChooseOpt], [cu, ch])
   where h (Global(
            Config(ConfigOpts(
             ConfigThreshold(ConfigThresholdOpts(
              ConfigThresholdSet(ConfigThresholdSetOpts a b))))))) = (a, b)

prop_configThresholdListSuccess = 
  testSuccess noOptsToGet h [configSub, thresholdSub, listSub] noOpts
   where h (Global( 
            Config(ConfigOpts(
             ConfigThreshold(ConfigThresholdOpts(
              ConfigThresholdList ConfigThresholdListOpts)))))) = noOptsToGet
 
prop_configTermDateSetSuccess t1 t2 t3 = 
 validArgs [t1, t2, t3] ==>
  testSuccess (t1,t2,t3) h [configSub, termDateSub, setSub] ([configTermDateSetTerm1Opt, configTermDateSetTerm2Opt, configTermDateSetTerm3Opt], 
                                                             [t1,t2,t3])
   where h (Global(
            Config(ConfigOpts( 
             ConfigTermDate(ConfigTermDateOpts(
              ConfigTermDateSet(ConfigTermDateSetOpts a b c))))))) = (a, b, c)

prop_configTermDateListSuccess =
  testSuccess noOptsToGet h [configSub, termDateSub, listSub] noOpts
   where h (Global( 
            Config(ConfigOpts(
             ConfigTermDate(ConfigTermDateOpts(
              ConfigTermDateList ConfigTermDateListOpts)))))) = noOptsToGet
 
prop_configProjectDateSetSuccess end late = 
 validArgs [end, late] ==>
  testSuccess (end, late) h [configSub, projectDateSub, setSub] ([configProjectDateSetEndOpt, configProjectDateSetLateOpt], 
                                                              [end, late])
   where h (Global(
            Config(ConfigOpts( 
             ConfigProjectDate(ConfigProjectDateOpts(
              ConfigProjectDateSet(ConfigProjectDateSetOpts a b))))))) = (a, b)

prop_configProjectDateListSuccess =
  testSuccess noOptsToGet h [configSub, projectDateSub, listSub] noOpts
   where h (Global( 
            Config(ConfigOpts(
             ConfigProjectDate(ConfigProjectDateOpts(
              ConfigProjectDateList ConfigProjectDateListOpts)))))) = noOptsToGet
              
              


 

 
