{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Security.CommandLineParserSecurityTest where

import Test.Framework
import Interface.CommandLineParser.ParserTestUtils
import Options.Applicative
import Interface.Lexicon
import Interface.CommandLineParser
import Data.Maybe

import Security.SecurityManager 

{-# ANN module "HLint: ignore Use camelCase" #-} 

prop_configThresholdSet cu ch = validOpts [ch,cu] ==>
 all f $ makeCmd [configSub,thresholdSub,setSub] (configThresholdOpts cu ch) noArgs
 where f cmd = failure cmd studentRole &&
               failure cmd correctorRole &&
               failure cmd teacherRole &&
               success cmd adminRole

prop_configThresholdList = 
 all f $ makeCmd [configSub,thresholdSub,listSub] noOpts noArgs 
 where f cmd = failure cmd studentRole &&
               failure cmd correctorRole &&
               success cmd teacherRole &&
               success cmd adminRole

prop_configTermDateSet t1 t2 t3 =  validOpts [t1,t2,t3] ==>
 all f $ makeCmd [configSub,termDateSub,setSub] (configTermDateOpts t1 t2 t3) noArgs
 where f cmd = failure cmd studentRole &&
               failure cmd correctorRole &&
               failure cmd teacherRole &&
               success cmd adminRole

prop_configTermList = 
 all f $ makeCmd [configSub,termDateSub,listSub] noOpts noArgs 
 where f cmd = failure cmd studentRole &&
               failure cmd correctorRole &&
               success cmd teacherRole &&
               success cmd adminRole
               
prop_configProjectDateSet end late = validOpts [end,late] ==>
 all f $ makeCmd [configSub,projectDateSub,setSub] (configProjectDateOpts end late) noArgs
 where f cmd = failure cmd studentRole &&
               failure cmd correctorRole &&
               failure cmd teacherRole &&
               success cmd adminRole

prop_configProjectDateList = 
 all f $ makeCmd [configSub,projectDateSub,listSub] noOpts noArgs 
 where f cmd = failure cmd studentRole &&
               failure cmd correctorRole &&
               success cmd teacherRole &&
               success cmd adminRole

prop_configAcceptExecSet v = let val = ["yYnN" !! (v `mod` 4)] in
 all f $ makeCmd [configSub,acceptExecSub,setSub] noOpts [val]
 where f cmd = failure cmd studentRole &&
               failure cmd correctorRole &&
               failure cmd teacherRole &&
               success cmd adminRole

prop_configAcceptExecList = 
 all f $ makeCmd [configSub,acceptExecSub,listSub] noOpts noArgs 
 where f cmd = failure cmd studentRole &&
               failure cmd correctorRole &&
               success cmd teacherRole &&
               success cmd adminRole
               
prop_configTrainTimeLimitSet v = let val = noLeadingHyphens v in
 all f $ makeCmd [configSub,timeLimitSub,setSub] noOpts [val]
 where f cmd = failure cmd studentRole &&
               failure cmd correctorRole &&
               failure cmd teacherRole &&
               success cmd adminRole

prop_configTrainTimeLimitList = 
 all f $ makeCmd [configSub,timeLimitSub,listSub] noOpts noArgs 
 where f cmd = failure cmd studentRole &&
               failure cmd correctorRole &&
               success cmd teacherRole &&
               success cmd adminRole
               
prop_configTrainSpaceLimitSet v = let val = noLeadingHyphens v in
 all f $ makeCmd [configSub,spaceLimitSub,setSub] noOpts [val]
 where f cmd = failure cmd studentRole &&
               failure cmd correctorRole &&
               failure cmd teacherRole &&
               success cmd adminRole

prop_configTrainSpaceLimitList = 
 all f $ makeCmd [configSub,spaceLimitSub,listSub] noOpts noArgs 
 where f cmd = failure cmd studentRole &&
               failure cmd correctorRole &&
               success cmd teacherRole &&
               success cmd adminRole
               
prop_configAdminGroupsSet gs = let groups = validArgs gs in groups /= [] ==>
 all f $ makeCmd [configSub,adminGroupsSub,setSub] noOpts groups
 where f cmd = failure cmd studentRole &&
               failure cmd correctorRole &&
               failure cmd teacherRole &&
               success cmd adminRole

prop_configAdminGroupsList = 
 all f $ makeCmd [configSub,adminGroupsSub,listSub] noOpts noArgs 
 where f cmd = failure cmd studentRole &&
               failure cmd correctorRole &&
               success cmd teacherRole &&
               success cmd adminRole        
               
prop_configTeacherGroupsSet gs = let groups = validArgs gs in
 all f $ makeCmd [configSub,teacherGroupsSub,setSub] noOpts groups
 where f cmd = failure cmd studentRole &&
               failure cmd correctorRole &&
               failure cmd teacherRole &&
               success cmd adminRole

prop_configTeacherGroupsList = 
 all f $ makeCmd [configSub,teacherGroupsSub,listSub] noOpts noArgs 
 where f cmd = failure cmd studentRole &&
               failure cmd correctorRole &&
               success cmd teacherRole &&
               success cmd adminRole        
               
prop_configCorrectorsAdd n = let name = noLeadingHyphens n in
 all f $ makeCmd [configSub,correctorSub,addSub] noOpts [name]
 where f cmd = failure cmd studentRole &&
               failure cmd correctorRole &&
               success cmd teacherRole &&
               success cmd adminRole

prop_configCorrectorsRemove n = let name = noLeadingHyphens n in 
 all f $ makeCmd [configSub,correctorSub,removeSub] noOpts [name]
 where f cmd = failure cmd studentRole &&
               failure cmd correctorRole &&
               success cmd teacherRole &&
               success cmd adminRole

prop_configCorrectorsIs n = let name = noLeadingHyphens n in 
 all f $ makeCmd [configSub,correctorSub,isSub] noOpts [name]
 where f cmd = failure cmd studentRole &&
               failure cmd correctorRole &&
               success cmd teacherRole &&
               success cmd adminRole

prop_repoAdd n = let ns = validArgs [n]; [name] = ns in ns /= [] ==>
 all f $ makeCmd [repoSub,addSub] noOpts [name]
 where f cmd = failure cmd studentRole &&
               failure cmd correctorRole &&
               failure cmd teacherRole &&
               success cmd adminRole

prop_repoRemove repoNN = validOpts [repoNN] ==>
 all f $ makeCmd [repoSub,removeSub] (repoOpts repoNN) noArgs
 where f cmd = failure cmd studentRole &&
               failure cmd correctorRole &&
               failure cmd teacherRole &&
               success cmd adminRole

prop_repoList = 
 all f $ makeCmd [repoSub,listSub] noOpts noArgs
 where f cmd = failure cmd studentRole &&
               success cmd correctorRole &&
               success cmd teacherRole &&
               success cmd adminRole

prop_termAdd repoNN n s e = let args = validArgs [n, s, e] in length args == 3 ==>
 all f $ makeCmd [termSub,addSub] (repoOpts repoNN) args
 where f cmd = failure cmd studentRole &&
               failure cmd correctorRole &&
               failure cmd teacherRole &&
               success cmd adminRole

prop_termRemove repoNN termNN = validOpts [repoNN,termNN] ==>
 all f $ makeCmd [termSub,removeSub] (termOpts repoNN termNN) noArgs
 where f cmd = failure cmd studentRole &&
               failure cmd correctorRole &&
               failure cmd teacherRole &&
               success cmd adminRole

prop_termList repoNN = validOpts [repoNN] ==>
 all f $ makeCmd [termSub,listSub] (repoOpts repoNN) noArgs
 where f cmd = failure cmd studentRole &&
               success cmd correctorRole &&
               success cmd teacherRole &&
               success cmd adminRole

prop_termDateSet repoNN termNN start end = validOpts [repoNN,termNN,start,end] ==>
 all f $ makeCmd [termSub,dateSub,setSub] (termDateOpts repoNN termNN start end) noArgs
 where f cmd = failure cmd studentRole &&
               failure cmd correctorRole &&
               failure cmd teacherRole &&
               success cmd adminRole

prop_termDateList repoNN termNN = validOpts [repoNN,termNN] ==>
 all f $ makeCmd [termSub,dateSub,listSub] (termOpts repoNN termNN) noArgs
 where f cmd = failure cmd studentRole &&
               success cmd correctorRole &&
               success cmd teacherRole &&
               success cmd adminRole
               
prop_courseAdd repoNN termNN n = validOpts [repoNN,termNN] ==>
 let ns = validArgs [n]; [name] = ns in ns /= [] ==>
 all f $ makeCmd [courseSub,addSub] (termOpts repoNN termNN) [name]
 where f cmd = failure cmd studentRole &&
               failure cmd correctorRole &&
               success cmd teacherRole &&
               success cmd adminRole

prop_courseRemove repoNN termNN courseNN = validOpts [repoNN,termNN,courseNN] ==>
 all f $ makeCmd [courseSub,removeSub] (courseOpts repoNN termNN courseNN) noArgs
 where f cmd = failure cmd studentRole &&
               failure cmd correctorRole &&
               success cmd teacherRole &&
               success cmd adminRole

prop_courseList repoNN termNN = validOpts [repoNN,termNN] ==>
 all f $ makeCmd [courseSub,listSub] (termOpts repoNN termNN) noArgs
 where f cmd = failure cmd studentRole &&
               success cmd correctorRole &&
               success cmd teacherRole &&
               success cmd adminRole
               
prop_courseTeacherAdd repoNN termNN courseNN ns = validOpts [repoNN,termNN,courseNN] ==> 
 let names = validArgs ns in names /= [] ==>
 all f $ makeCmd [courseSub,teacherSub,addSub] (courseOpts repoNN termNN courseNN) names
 where f cmd = failure cmd studentRole &&
                failure cmd correctorRole &&
                success cmd teacherRole &&
                success cmd adminRole

prop_courseTeacherRemove repoNN termNN courseNN ns = validOpts [repoNN,termNN,courseNN] ==> 
 let names = validArgs ns in names /= [] ==>
 all f $ makeCmd [courseSub,teacherSub,removeSub] (courseOpts repoNN termNN courseNN) names
 where f cmd = failure cmd studentRole &&
               failure cmd correctorRole &&
               success cmd teacherRole &&
               success cmd adminRole

prop_courseTeacherList repoNN termNN courseNN = validOpts [repoNN,termNN,courseNN] ==>
 all f $ makeCmd [courseSub,teacherSub,listSub] (courseOpts repoNN termNN courseNN) noArgs
 where f cmd = failure cmd studentRole &&
               success cmd correctorRole &&
               success cmd teacherRole &&
               success cmd adminRole
    
prop_courseCorrectorAdd repoNN termNN courseNN ns = validOpts [repoNN,termNN,courseNN] ==>
 let names = validArgs ns in names /= [] ==>
 all f $ makeCmd [courseSub,correctorSub,addSub] (courseOpts repoNN termNN courseNN) names
 where f cmd = failure cmd studentRole &&
               failure cmd correctorRole &&
               success cmd teacherRole &&
               success cmd adminRole

prop_courseCorrectorRemove repoNN termNN courseNN ns = validOpts [repoNN,termNN,courseNN] ==>
 let names = validArgs ns in names /= [] ==>
 all f $ makeCmd [courseSub,correctorSub,removeSub] (courseOpts repoNN termNN courseNN) names
 where f cmd = failure cmd studentRole &&
               failure cmd correctorRole &&
               success cmd teacherRole &&
               success cmd adminRole

prop_courseCorrectorList repoNN termNN courseNN = validOpts [repoNN,termNN,courseNN] ==>
 all f $ makeCmd [courseSub,correctorSub,listSub] (courseOpts repoNN termNN courseNN) noArgs
 where f cmd = failure cmd studentRole &&
               success cmd correctorRole &&
               success cmd teacherRole &&
               success cmd adminRole
               
prop_groupAdd repoNN termNN courseNN n = validOpts [repoNN,termNN,courseNN] ==>
 let ns = validArgs [n]; [name] = ns in ns /= [] ==>
 all f $ makeCmd [groupSub,addSub] (courseOpts repoNN termNN courseNN) [name]
 where f cmd = failure cmd studentRole &&
               failure cmd correctorRole &&
               success cmd teacherRole &&
               success cmd adminRole

prop_groupRemove repoNN termNN courseNN groupNN = validOpts [repoNN,termNN,courseNN,groupNN] ==>
 all f $ makeCmd [groupSub,removeSub] (groupOpts repoNN termNN courseNN groupNN) noArgs
 where f cmd = failure cmd studentRole &&
               failure cmd correctorRole &&
               success cmd teacherRole &&
               success cmd adminRole

prop_groupList repoNN termNN courseNN = validOpts [repoNN,termNN,courseNN] ==>
 all f $ makeCmd [groupSub,listSub] (courseOpts repoNN termNN courseNN) noArgs
 where f cmd = failure cmd studentRole &&
               success cmd correctorRole &&
               success cmd teacherRole &&
               success cmd adminRole
               
prop_groupTeacherAdd repoNN termNN courseNN groupNN ns = validOpts [repoNN,termNN,courseNN,groupNN] ==>
 let names = validArgs ns in names /= [] ==>
 all f $ makeCmd [groupSub,teacherSub,addSub] (groupOpts repoNN termNN courseNN groupNN) names
 where f cmd = failure cmd studentRole &&
               failure cmd correctorRole &&
               success cmd teacherRole &&
               success cmd adminRole

prop_groupTeacherRemove repoNN termNN courseNN groupNN ns = validOpts [repoNN,termNN,courseNN,groupNN] ==>
 let names = validArgs ns in names /= [] ==>
 all f $ makeCmd [groupSub,teacherSub,removeSub] (groupOpts repoNN termNN courseNN groupNN) names
 where f cmd = failure cmd studentRole &&
               failure cmd correctorRole &&
               success cmd teacherRole &&
               success cmd adminRole

prop_groupTeacherList repoNN termNN courseNN groupNN = validOpts [repoNN,termNN,courseNN,groupNN] ==>
 all f $ makeCmd [groupSub,teacherSub,listSub] (groupOpts repoNN termNN courseNN groupNN) noArgs
 where f cmd = failure cmd studentRole &&
               success cmd correctorRole &&
               success cmd teacherRole &&
               success cmd adminRole
    
prop_groupCorrectorAdd repoNN termNN courseNN groupNN ns = validOpts [repoNN,termNN,courseNN,groupNN] ==>
 let names = validArgs ns in names /= [] ==>
 all f $ makeCmd [groupSub,correctorSub,addSub] (groupOpts repoNN termNN courseNN groupNN) names
 where f cmd = failure cmd studentRole &&
               failure cmd correctorRole &&
               success cmd teacherRole &&
               success cmd adminRole

prop_groupCorrectorRemove repoNN termNN courseNN groupNN ns = validOpts [repoNN,termNN,courseNN,groupNN] ==>
 let names = validArgs ns in names /= [] ==>
 all f $ makeCmd [groupSub,correctorSub,removeSub] (groupOpts repoNN termNN courseNN groupNN) names
 where f cmd = failure cmd studentRole &&
               failure cmd correctorRole &&
               success cmd teacherRole &&
               success cmd adminRole

prop_groupCorrectorList repoNN termNN groupNN courseNN = validOpts [repoNN,termNN,courseNN,groupNN] ==>
 all f $ makeCmd [groupSub,correctorSub,listSub] (groupOpts repoNN termNN courseNN groupNN) noArgs
 where f cmd = failure cmd studentRole &&
               success cmd correctorRole &&
               success cmd teacherRole &&
               success cmd adminRole

prop_projectAdd repoNN termNN courseNN groupNN n = validOpts [repoNN,termNN,courseNN,groupNN] ==>
 let ns = validArgs [n]; [name] = ns in ns /= [] ==>
 all f $ makeCmd [projectSub,addSub] (groupOpts repoNN termNN courseNN groupNN) [name]
 where f cmd = failure cmd studentRole &&
               failure cmd correctorRole &&
               success cmd teacherRole &&
               success cmd adminRole

prop_projectRemove repoNN termNN courseNN groupNN projectNN = validOpts [repoNN,termNN,courseNN,groupNN,projectNN] ==>
 all f $ makeCmd [projectSub,removeSub] (projectOpts repoNN termNN courseNN groupNN projectNN) noArgs
 where f cmd = failure cmd studentRole &&
               failure cmd correctorRole &&
               success cmd teacherRole &&
               success cmd adminRole

prop_projectList repoNN termNN courseNN groupNN = validOpts [repoNN,termNN,courseNN,groupNN] ==>
 all f $ makeCmd [projectSub,listSub] (groupOpts repoNN termNN courseNN groupNN) noArgs
 where f cmd = failure cmd studentRole &&
               success cmd correctorRole &&
               success cmd teacherRole &&
               success cmd adminRole               
                     
prop_projectDateSet repoNN termNN courseNN groupNN projectNN start end late = 
 validOpts [repoNN,termNN,courseNN,groupNN,projectNN,start,end,late] ==>
 all f $ makeCmd [projectSub,dateSub, setSub] (projectOpts repoNN termNN courseNN groupNN projectNN) noArgs
 where f cmd = failure cmd studentRole &&
               failure cmd correctorRole &&
               success cmd teacherRole &&
               success cmd adminRole         
                     
prop_projectDateList repoNN termNN courseNN groupNN projectNN = 
 validOpts [repoNN,termNN,courseNN,groupNN,projectNN] ==>
 all f $ makeCmd [projectSub,dateSub,listSub] (projectOpts repoNN termNN courseNN groupNN projectNN) noArgs
 where f cmd = failure cmd studentRole &&
               success cmd correctorRole &&
               success cmd teacherRole &&
               success cmd adminRole               
                     
prop_projectValidateAcceptExecSet repoNN termNN courseNN groupNN projectNN v =
 validOpts [repoNN,termNN,courseNN,groupNN,projectNN] ==>
 let val = ["yYnN" !! (v `mod` 4)] in
 all f $ makeCmd [projectSub,validateSub,acceptExecSub,setSub] (projectOpts repoNN termNN courseNN groupNN projectNN) [val]
 where f cmd = failure cmd studentRole &&
               failure cmd correctorRole &&
               success cmd teacherRole &&
               success cmd adminRole     
                     
prop_projectValidateAcceptExecList repoNN termNN courseNN groupNN projectNN =
 validOpts [repoNN,termNN,courseNN,groupNN,projectNN] ==>
 all f $ makeCmd [projectSub,validateSub,acceptExecSub,listSub] (projectOpts repoNN termNN courseNN groupNN projectNN) noArgs
 where f cmd = failure cmd studentRole &&
               success cmd correctorRole &&
               success cmd teacherRole &&
               success cmd adminRole     
                     
prop_projectValidateNamesAdd repoNN termNN courseNN groupNN projectNN ns =
 validOpts [repoNN,termNN,courseNN,groupNN,projectNN] ==>
 let names = validArgs ns in names /= [] ==>
 all f $ makeCmd [projectSub,validateSub,nameSub,addSub] (projectOpts repoNN termNN courseNN groupNN projectNN) names
 where f cmd = failure cmd studentRole &&
               failure cmd correctorRole &&
               success cmd teacherRole &&
               success cmd adminRole     
                            
prop_projectValidateNamesRemove repoNN termNN courseNN groupNN projectNN ns =
 validOpts [repoNN,termNN,courseNN,groupNN,projectNN] ==>
 let names = validArgs ns in names /= [] ==>
 all f $ makeCmd [projectSub,validateSub,nameSub,removeSub] (projectOpts repoNN termNN courseNN groupNN projectNN) names
 where f cmd = failure cmd studentRole &&
               failure cmd correctorRole &&
               success cmd teacherRole &&
               success cmd adminRole  
                             
prop_projectValidateNamesList repoNN termNN courseNN groupNN projectNN =
 validOpts [repoNN,termNN,courseNN,groupNN,projectNN] ==>
 all f $ makeCmd [projectSub,validateSub,nameSub,listSub] (projectOpts repoNN termNN courseNN groupNN projectNN) noArgs 
 where f cmd = failure cmd studentRole &&
               success cmd correctorRole &&
               success cmd teacherRole &&
               success cmd adminRole     
                 
prop_projectValidateCommandSet repoNN termNN courseNN groupNN projectNN co = 
 validOpts [repoNN,termNN,courseNN,groupNN,projectNN] ==>
 let cs = validArgs [co]; [comm] = cs in cs /= [] ==>
 all f $ makeCmd [projectSub,validateSub,commandSub,setSub] (projectOpts repoNN termNN courseNN groupNN projectNN) [comm]
 where f cmd = failure cmd studentRole &&
               failure cmd correctorRole &&
               success cmd teacherRole && 
               success cmd adminRole     
                            
prop_projectValidateCommandUnset repoNN termNN courseNN groupNN projectNN =
 validOpts [repoNN,termNN,courseNN,groupNN,projectNN] ==>
 all f $ makeCmd [projectSub,validateSub,commandSub,unsetSub] (projectOpts repoNN termNN courseNN groupNN projectNN) noArgs
 where f cmd = failure cmd studentRole &&
               failure cmd correctorRole &&
               success cmd teacherRole &&
               success cmd adminRole  
                             
prop_projectValidateCommandList repoNN termNN courseNN groupNN projectNN =
 validOpts [repoNN,termNN,courseNN,groupNN,projectNN] ==>
 all f $ makeCmd [projectSub,validateSub,commandSub,listSub] (projectOpts repoNN termNN courseNN groupNN projectNN) noArgs 
 where f cmd = failure cmd studentRole &&
               success cmd correctorRole &&
               success cmd teacherRole &&
               success cmd adminRole     
               
prop_projectValidateScriptSet repoNN termNN courseNN groupNN projectNN n = 
 validOpts [repoNN,termNN,courseNN,groupNN,projectNN] ==>
 let ns = validArgs [n]; [scriptName] = ns in ns /= [] ==>
 all f $ makeCmd [projectSub,validateSub,scriptSub,setSub] (projectOpts repoNN termNN courseNN groupNN projectNN) [scriptName]
 where f cmd = failure cmd studentRole &&
               failure cmd correctorRole &&
               success cmd teacherRole && 
               success cmd adminRole     
                            
prop_projectValidateScriptUnset repoNN termNN courseNN groupNN projectNN =
 validOpts [repoNN,termNN,courseNN,groupNN,projectNN] ==>
 all f $ makeCmd [projectSub,validateSub,scriptSub,unsetSub] (projectOpts repoNN termNN courseNN groupNN projectNN) noArgs
 where f cmd = failure cmd studentRole &&
               failure cmd correctorRole &&
               success cmd teacherRole &&
               success cmd adminRole  
                             
prop_projectValidateScriptList repoNN termNN courseNN groupNN projectNN =
 validOpts [repoNN,termNN,courseNN,groupNN,projectNN] ==>
 all f $ makeCmd [projectSub,validateSub,scriptSub,listSub] (projectOpts repoNN termNN courseNN groupNN projectNN) noArgs 
 where f cmd = failure cmd studentRole &&
               success cmd correctorRole &&
               success cmd teacherRole &&
               success cmd adminRole     
               
prop_projectValidateScriptExtract repoNN termNN courseNN groupNN projectNN d = 
 validOpts [repoNN,termNN,courseNN,groupNN,projectNN] ==>
 let ds = validArgs [d]; [extractDir] = ds in ds /= [] ==>
 all f $ makeCmd [projectSub,validateSub,scriptSub,extractSub] (projectOpts repoNN termNN courseNN groupNN projectNN) [extractDir]
 where f cmd = failure cmd studentRole &&
               success cmd correctorRole &&
               success cmd teacherRole && 
               success cmd adminRole     
               
prop_projectWorktrainScriptSet repoNN termNN courseNN groupNN projectNN n = 
 validOpts [repoNN,termNN,courseNN,groupNN,projectNN] ==>
 let ns = validArgs [n]; [scriptName] = ns in ns /= [] ==>
 all f $ makeCmd [projectSub,worktrainSub,scriptSub,setSub] (projectOpts repoNN termNN courseNN groupNN projectNN) [scriptName]
 where f cmd = failure cmd studentRole &&
               failure cmd correctorRole &&
               success cmd teacherRole && 
               success cmd adminRole     
                            
prop_projectWorktrainScriptUnset repoNN termNN courseNN groupNN projectNN =
 validOpts [repoNN,termNN,courseNN,groupNN,projectNN] ==>
 all f $ makeCmd [projectSub,worktrainSub,scriptSub,unsetSub] (projectOpts repoNN termNN courseNN groupNN projectNN) noArgs
 where f cmd = failure cmd studentRole &&
               failure cmd correctorRole &&
               success cmd teacherRole &&
               success cmd adminRole  
                             
prop_projectWorktrainScriptList repoNN termNN courseNN groupNN projectNN =
 validOpts [repoNN,termNN,courseNN,groupNN,projectNN] ==>
 all f $ makeCmd [projectSub,worktrainSub,scriptSub,listSub] (projectOpts repoNN termNN courseNN groupNN projectNN) noArgs 
 where f cmd = failure cmd studentRole &&
               success cmd correctorRole &&
               success cmd teacherRole &&
               success cmd adminRole     
               
prop_projectWorktrainScriptExtract repoNN termNN courseNN groupNN projectNN d = 
 validOpts [repoNN,termNN,courseNN,groupNN,projectNN] ==>
 let ds = validArgs [d]; [extractDir] = ds in ds /= [] ==>
 all f $ makeCmd [projectSub,worktrainSub,scriptSub,extractSub] (projectOpts repoNN termNN courseNN groupNN projectNN) [extractDir]
 where f cmd = failure cmd studentRole &&
               success cmd correctorRole &&
               success cmd teacherRole && 
               success cmd adminRole     
                        
prop_projectWorktrainFileAdd repoNN termNN courseNN groupNN projectNN fs =
 validOpts [repoNN,termNN,courseNN,groupNN,projectNN] ==>
 let files = validArgs fs in files /= [] ==>
 all f $ makeCmd [projectSub,worktrainSub,fileSub,addSub] (projectOpts repoNN termNN courseNN groupNN projectNN) files
 where f cmd = failure cmd studentRole &&
               failure cmd correctorRole &&
               success cmd teacherRole &&
               success cmd adminRole     
                            
prop_projectWorktrainFileRemove repoNN termNN courseNN groupNN projectNN fs =
 validOpts [repoNN,termNN,courseNN,groupNN,projectNN] ==>
 let files = validArgs fs in files /= [] ==>
 all f $ makeCmd [projectSub,worktrainSub,fileSub,removeSub] (projectOpts repoNN termNN courseNN groupNN projectNN) files
 where f cmd = failure cmd studentRole &&
               failure cmd correctorRole &&
               success cmd teacherRole &&
               success cmd adminRole  
                             
prop_projectWorktrainFileList repoNN termNN courseNN groupNN projectNN =
 validOpts [repoNN,termNN,courseNN,groupNN,projectNN] ==>
 all f $ makeCmd [projectSub,worktrainSub,fileSub,listSub] (projectOpts repoNN termNN courseNN groupNN projectNN) noArgs 
 where f cmd = failure cmd studentRole &&
               success cmd correctorRole &&
               success cmd teacherRole &&
               success cmd adminRole     
                     
prop_projectWorktrainFileExtract repoNN termNN courseNN groupNN projectNN d fs =
 validOpts [repoNN,termNN,courseNN,groupNN,projectNN] ==>
 let args = validArgs (d:fs) in args /= [] ==>
 all f $ makeCmd [projectSub,worktrainSub,fileSub,extractSub] (projectOpts repoNN termNN courseNN groupNN projectNN) args 
 where f cmd = failure cmd studentRole &&
               success cmd correctorRole &&
               success cmd teacherRole &&
               success cmd adminRole     
               
prop_projectWorktrainTimeLimitSet repoNN termNN courseNN groupNN projectNN v =
 validOpts [repoNN,termNN,courseNN,groupNN,projectNN] ==>
 let val = noLeadingHyphens v in
 all f $ makeCmd [projectSub,worktrainSub,timeLimitSub,setSub] (projectOpts repoNN termNN courseNN groupNN projectNN) [val]
 where f cmd = failure cmd studentRole &&
               failure cmd correctorRole &&
               success cmd teacherRole &&
               success cmd adminRole     
               
prop_projectWorktrainTimeLimitList repoNN termNN courseNN groupNN projectNN =
 validOpts [repoNN,termNN,courseNN,groupNN,projectNN] ==>
 all f $ makeCmd [projectSub,worktrainSub,timeLimitSub,listSub] (projectOpts repoNN termNN courseNN groupNN projectNN) noArgs
 where f cmd = failure cmd studentRole &&
               success cmd correctorRole &&
               success cmd teacherRole &&
               success cmd adminRole     
               
prop_projectWorktrainSpaceLimitSet repoNN termNN courseNN groupNN projectNN v =
 validOpts [repoNN,termNN,courseNN,groupNN,projectNN] ==>
 let val = noLeadingHyphens v in
 all f $ makeCmd [projectSub,worktrainSub,spaceLimitSub,setSub] (projectOpts repoNN termNN courseNN groupNN projectNN) [val]
 where f cmd = failure cmd studentRole &&
               failure cmd correctorRole &&
               success cmd teacherRole &&
               success cmd adminRole     
               
prop_projectWorktrainSpaceLimitList repoNN termNN courseNN groupNN projectNN =
 validOpts [repoNN,termNN,courseNN,groupNN,projectNN] ==>
 all f $ makeCmd [projectSub,worktrainSub,spaceLimitSub,listSub] (projectOpts repoNN termNN courseNN groupNN projectNN) noArgs
 where f cmd = failure cmd studentRole &&
               success cmd correctorRole &&
               success cmd teacherRole &&
               success cmd adminRole     
                            
prop_projectSubmitList repoNN termNN courseNN groupNN projectNN =
 validOpts [repoNN,termNN,courseNN,groupNN,projectNN] ==>
 all f $ makeCmd [projectSub,submitSub,listSub] (projectOpts repoNN termNN courseNN groupNN projectNN) noArgs
 where f cmd = failure cmd studentRole &&
               success cmd correctorRole &&
               success cmd teacherRole &&
               success cmd adminRole  
                            
prop_projectSubmiLate repoNN termNN courseNN groupNN projectNN =
 validOpts [repoNN,termNN,courseNN,groupNN,projectNN] ==>
 all f $ makeCmd [projectSub,submitSub,lateSub] (projectOpts repoNN termNN courseNN groupNN projectNN) noArgs
 where f cmd = failure cmd studentRole &&
               success cmd correctorRole &&
               success cmd teacherRole &&
               success cmd adminRole  
                            
prop_projectSubmiInspect repoNN termNN courseNN groupNN projectNN ks =
 validOpts [repoNN,termNN,courseNN,groupNN,projectNN] ==>
 let keys = validArgs ks in keys /= [] ==>
 all f $ makeCmd [projectSub,submitSub,inspectSub] (projectOpts repoNN termNN courseNN groupNN projectNN) keys
 where f cmd = failure cmd studentRole &&
               success cmd correctorRole &&
               success cmd teacherRole &&
               success cmd adminRole  
                            
prop_projectSubmiExtract repoNN termNN courseNN groupNN projectNN d ks =
 validOpts [repoNN,termNN,courseNN,groupNN,projectNN] ==>
 let args = validArgs (d:ks) in args /= [] ==>
 all f $ makeCmd [projectSub,submitSub,extractSub] (projectOpts repoNN termNN courseNN groupNN projectNN) args
 where f cmd = failure cmd studentRole &&
               success cmd correctorRole &&
               success cmd teacherRole &&
               success cmd adminRole  
                            
prop_submit repoNN termNN courseNN groupNN projectNN fs =
 validOpts [repoNN,termNN,courseNN,groupNN,projectNN] ==>
 let files = validArgs fs in files /= [] ==>
 all f $ makeCmd [submitSub] (projectOpts repoNN termNN courseNN groupNN projectNN) files
 where f cmd = success cmd studentRole &&
               success cmd correctorRole &&
               success cmd teacherRole &&
               success cmd adminRole  
                            
prop_inspect repoNN termNN courseNN groupNN projectNN =
 validOpts [repoNN,termNN,courseNN,groupNN,projectNN] ==>
 all f $ makeCmd [inspectSub] (projectOpts repoNN termNN courseNN groupNN projectNN) noArgs
 where f cmd = success cmd studentRole &&
               success cmd correctorRole &&
               success cmd teacherRole &&
               success cmd adminRole  
                            
prop_extract repoNN termNN courseNN groupNN projectNN d =
 validOpts [repoNN,termNN,courseNN,groupNN,projectNN] ==>
 let dir = noLeadingHyphens d in
 all f $ makeCmd [extractSub] (projectOpts repoNN termNN courseNN groupNN projectNN) [dir]
 where f cmd = success cmd studentRole &&
               success cmd correctorRole &&
               success cmd teacherRole &&
               success cmd adminRole  
                            
prop_worktrainRun repoNN termNN courseNN groupNN projectNN ks =
 validOpts [repoNN,termNN,courseNN,groupNN,projectNN] ==>
 let keys = validArgs ks in keys /= [] ==>
 all f $ makeCmd [worktrainSub,runSub] (projectOpts repoNN termNN courseNN groupNN projectNN) keys
 where f cmd = failure cmd studentRole &&
               success cmd correctorRole &&
               success cmd teacherRole &&
               success cmd adminRole  
               
prop_worktrainDate repoNN termNN courseNN groupNN projectNN =
 validOpts [repoNN,termNN,courseNN,groupNN,projectNN] ==>
 all f $ makeCmd [worktrainSub,dateSub] (projectOpts repoNN termNN courseNN groupNN projectNN) noArgs
 where f cmd = failure cmd studentRole &&
               success cmd correctorRole &&
               success cmd teacherRole &&
               success cmd adminRole  
               
prop_worktrainList repoNN termNN courseNN groupNN projectNN trainRunNN =
 validOpts [repoNN,termNN,courseNN,groupNN,projectNN,trainRunNN] ==>
 all f $ makeCmd [worktrainSub,listSub] (trainRunOpts repoNN termNN courseNN groupNN projectNN trainRunNN) noArgs
 where f cmd = failure cmd studentRole &&
               success cmd correctorRole &&
               success cmd teacherRole &&
               success cmd adminRole  
               
prop_worktrainExtract repoNN termNN courseNN groupNN projectNN trainRunNN d ks =
 validOpts [repoNN,termNN,courseNN,groupNN,projectNN,trainRunNN] ==>
 let args = validArgs (d:ks) in args /= [] ==>
 all f $ makeCmd [worktrainSub,extractSub] (trainRunOpts repoNN termNN courseNN groupNN projectNN trainRunNN) args
 where f cmd = failure cmd studentRole &&
               success cmd correctorRole &&
               success cmd teacherRole &&
               success cmd adminRole  
               
prop_worktrainOutput repoNN termNN courseNN groupNN projectNN trainRunNN d ks m =
 validOpts [repoNN,termNN,courseNN,groupNN,projectNN,trainRunNN] ==>
 let args = validArgs (d:ks) in args /= [] ==>
 all f $ makeCmd [worktrainSub,outputSub] (trainRunOutputOpts repoNN termNN courseNN groupNN projectNN trainRunNN m mergeFlag) args
 where f cmd = failure cmd studentRole &&
               success cmd correctorRole &&
               success cmd teacherRole &&
               success cmd adminRole  
                              
failure cmd = isNothing . testCmd cmd 
success cmd = isJust . testCmd cmd
testCmd cmd role = execParserMaybe (globalInfo role) cmd