{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Interface.CommandLineParserTest where
import Test.Framework
import Interface.CommandLineParserTest.Utils

import Interface.Lexicon
import Interface.CommandLineParser
import Interface.CommandLineParser.Config
import Interface.CommandLineParser.Repo
import Interface.CommandLineParser.Term

{-# ANN module "HLint: ignore Use camelCase" #-}

-- Config 
prop_configThresholdSetSuccess cu ch =
 validOpts [cu, ch] ==>
  testSuccess (cu,ch) h [configSub, thresholdSub, setSub] (thresholdOpts cu ch) noArgs
   where h (Global(
            Config(ConfigOpts(
             ConfigThreshold(ConfigThresholdOpts(
              ConfigThresholdSet(ConfigThresholdSetOpts a b))))))) = (a,b)

prop_configThresholdListSuccess =
  testSuccess noArgsToGet h [configSub, thresholdSub, listSub] noOpts noArgs
   where h (Global(
            Config(ConfigOpts(
             ConfigThreshold(ConfigThresholdOpts(
              ConfigThresholdList ConfigThresholdListOpts)))))) = noArgsToGet

prop_configTermDateSetSuccess t1 t2 t3 =
 validOpts [t1, t2, t3] ==>
  testSuccess (t1,t2,t3) h [configSub, termDateSub, setSub] (configTermDateOpts t1 t2 t3) noArgs
   where h (Global(
            Config(ConfigOpts( 
             ConfigTermDate(ConfigTermDateOpts(
              ConfigTermDateSet(ConfigTermDateSetOpts a b c))))))) = (a,b,c)

prop_configTermDateListSuccess =
  testSuccess noArgsToGet h [configSub, termDateSub, listSub] noOpts noArgs
   where h (Global(
            Config(ConfigOpts(
             ConfigTermDate(ConfigTermDateOpts(
              ConfigTermDateList ConfigTermDateListOpts)))))) = noArgsToGet

prop_configProjectDateSetSuccess end late =
 validOpts [end, late] ==>
  testSuccess (end, late) h [configSub, projectDateSub, setSub] (configProjectDateOpts end late) noArgs
   where h (Global(
            Config(ConfigOpts(
             ConfigProjectDate(ConfigProjectDateOpts(
              ConfigProjectDateSet(ConfigProjectDateSetOpts a b))))))) = (a,b)

prop_configProjectDateListSuccess =
  testSuccess noArgsToGet h [configSub, projectDateSub, listSub] noOpts noArgs
   where h (Global(
            Config(ConfigOpts(
             ConfigProjectDate(ConfigProjectDateOpts(
              ConfigProjectDateList ConfigProjectDateListOpts)))))) = noArgsToGet

prop_configAcceptExecSetSuccess v =
 let val = ["yYnN" !! (v `mod` 4)]
 in testSuccess val h [configSub, acceptExecSub, setSub] noOpts [val]
     where h (Global(
              Config(ConfigOpts(
               ConfigAcceptExec(ConfigAcceptExecOpts(
                ConfigAcceptExecSet(ConfigAcceptExecSetOpts a))))))) = a

prop_configAcceptExecListSuccess =
  testSuccess noArgsToGet h [configSub, acceptExecSub, listSub] noOpts noArgs
   where h (Global(
            Config(ConfigOpts(
             ConfigAcceptExec(ConfigAcceptExecOpts(
              ConfigAcceptExecList ConfigAcceptExecListOpts)))))) = noArgsToGet

prop_configTimeLimitSetSuccess v =
 let val = show $ abs (v :: Double)
 in  testSuccess val h [configSub, timeLimitSub, setSub] noOpts [val]
      where h (Global(
               Config(ConfigOpts(
                ConfigTimeLimit(ConfigTimeLimitOpts(
                 ConfigTimeLimitSet(ConfigTimeLimitSetOpts a))))))) = a

prop_configTimeLimitListSuccess =
  testSuccess noArgsToGet h [configSub, timeLimitSub, listSub] noOpts noArgs
   where h (Global(
            Config(ConfigOpts(
             ConfigTimeLimit(ConfigTimeLimitOpts(
              ConfigTimeLimitList ConfigTimeLimitListOpts)))))) = noArgsToGet

prop_configSpaceLimitSetSuccess v =
 let val = show $ abs (v :: Integer)
 in  testSuccess val h [configSub, spaceLimitSub, setSub] noOpts [val]
      where h (Global(
               Config(ConfigOpts(
                ConfigSpaceLimit(ConfigSpaceLimitOpts(
                 ConfigSpaceLimitSet(ConfigSpaceLimitSetOpts a))))))) = a

prop_configSpaceLimitListSuccess =
  testSuccess noArgsToGet h [configSub, spaceLimitSub, listSub] noOpts noArgs
   where h (Global(
            Config(ConfigOpts(
             ConfigSpaceLimit(ConfigSpaceLimitOpts(
              ConfigSpaceLimitList ConfigSpaceLimitListOpts)))))) = noArgsToGet

prop_configAdminGroupsSetSuccess gs =
 gs /= [] ==>
  let groups = map noLeadingHyphens gs
  in  testSuccess groups h [configSub, adminGroupsSub, setSub] noOpts groups
       where h (Global(
                Config(ConfigOpts(
                 ConfigAdminGroups(ConfigAdminGroupsOpts(
                  ConfigAdminGroupsSet(ConfigAdminGroupsSetOpts a))))))) = a

prop_configAdminGroupsListSuccess =
  testSuccess noArgsToGet h [configSub, adminGroupsSub, listSub] noOpts noArgs
   where h (Global(
            Config(ConfigOpts(
             ConfigAdminGroups(ConfigAdminGroupsOpts(
              ConfigAdminGroupsList ConfigAdminGroupsListOpts)))))) = noArgsToGet

prop_configTeacherGroupsSetSuccess gs =
 let groups = map noLeadingHyphens gs
 in  testSuccess groups h [configSub, teacherGroupsSub, setSub] noOpts groups
      where h (Global(
               Config(ConfigOpts(
                ConfigTeacherGroups(ConfigTeacherGroupsOpts(
                 ConfigTeacherGroupsSet(ConfigTeacherGroupsSetOpts a))))))) = a

prop_configTeacherGroupsListSuccess =
  testSuccess noArgsToGet h [configSub, teacherGroupsSub, listSub] noOpts noArgs
   where h (Global(
            Config(ConfigOpts(
             ConfigTeacherGroups(ConfigTeacherGroupsOpts(
              ConfigTeacherGroupsList ConfigTeacherGroupsListOpts)))))) = noArgsToGet

prop_configCorrectorIsSuccess n =
 let name = noLeadingHyphens n
 in  testSuccess name h [configSub, correctorSub, isSub] noOpts [name]
      where h (Global(
               Config(ConfigOpts(
                ConfigCorrector(ConfigCorrectorOpts(
                 ConfigCorrectorIs(ConfigCorrectorIsOpts a))))))) = a

prop_configCorrectorAddSuccess n =
 let name = noLeadingHyphens n
 in  testSuccess name h [configSub, correctorSub, addSub] noOpts [name]
      where h (Global(
               Config(ConfigOpts(
                ConfigCorrector(ConfigCorrectorOpts(
                 ConfigCorrectorAdd(ConfigCorrectorAddOpts a))))))) = a

prop_configCorrectorRemoveSuccess n =
 let name = noLeadingHyphens n
 in  testSuccess name h [configSub, correctorSub, removeSub] noOpts [name]
      where h (Global(
               Config(ConfigOpts(
                ConfigCorrector(ConfigCorrectorOpts(
                 ConfigCorrectorRemove(ConfigCorrectorRemoveOpts a))))))) = a
                 
-- Repo                
prop_repoAddSuccess n =
 let name = noLeadingHyphens n
 in  testSuccess name h [repoSub, addSub] noOpts [name]
      where h (Global(
               Repo(RepoOpts(
                RepoAdd(RepoAddOpts a))))) = a
                
prop_repoRemoveSuccess repoNN =
 validOpts [repoNN] ==>
  testSuccess repoNN h [repoSub, removeSub] (repoOpts repoNN) noArgs
   where h (Global(
            Repo(RepoOpts(
             RepoRemove(RepoRemoveOpts a))))) = a
            
prop_repoListSuccess =
 testSuccess noArgsToGet h [repoSub, listSub] noOpts noArgs
  where h (Global(
           Repo(RepoOpts(
            RepoList RepoListOpts)))) = noArgsToGet
 
-- Term         
prop_termAddSuccess repoNN n s e =
 validOpts [repoNN] ==>
  let args@[name, start, end] = map noLeadingHyphens [n, s, e]
  in  testSuccess (repoNN, name, start, end) h [termSub, addSub] (repoOpts repoNN) args
       where h (Global(
                Term(TermOpts( 
                 TermAdd(TermAddOpts a b c d))))) = (a,b,c,d)
                
prop_termRemoveSuccess repoNN termNN =
 validOpts [repoNN, termNN] ==>
  testSuccess (repoNN,termNN) h [termSub, removeSub] (termOpts repoNN termNN) noArgs
   where h (Global(
            Term(TermOpts(
             TermRemove(TermRemoveOpts a b))))) = (a,b)
             
prop_termListSuccess repoNN =
 validOpts [repoNN] ==>
  testSuccess repoNN h [termSub, listSub] (repoOpts repoNN) noArgs
   where h (Global(
            Term(TermOpts(
             TermList(TermListOpts a))))) = a
             
prop_termDateSetSuccess repoNN termNN start end =
 validOpts [repoNN, termNN, start, end] ==>
  testSuccess (repoNN, termNN, start, end) h [termSub, dateSub, setSub] (termDateOpts repoNN termNN start end) noArgs
   where h (Global(
            Term(TermOpts(
             TermDate(TermDateOpts(
              TermDateSet(TermDateSetOpts a b c d))))))) = (a,b,c,d)
 
prop_termDateListSuccess repoNN termNN =
 validOpts [repoNN, termNN] ==>
  testSuccess (repoNN, termNN) h [termSub, dateSub, listSub] (termOpts repoNN termNN) noArgs
   where h (Global(
            Term(TermOpts(
             TermDate(TermDateOpts(
              TermDateList(TermDateListOpts a b))))))) = (a,b)
              
              
              
              