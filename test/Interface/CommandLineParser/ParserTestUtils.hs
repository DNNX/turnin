{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Interface.CommandLineParser.ParserTestUtils where

import Test.Framework
import Options.Applicative
import Data.List
import Data.Maybe
import Interface.Lexicon
import Security.SecurityManager

import Interface.CommandLineParser

{-# ANN module "HLint: ignore Use camelCase" #-}
sampleSize = 7 -- Keep this many random option permutations, tests runtime is exponential in this number

o = Nothing
allRoles = [adminRole, teacherRole, correctorRole, studentRole]

data Opt = O Char String | B Char String
type Opts = ([Opt],[Maybe String])

noArgsToGet = ()
noOpts = ([],[])
noArgs = []

repoOpts r = (map z [repoNodeOpt], [r])
termOpts r t = (map z [repoNodeOpt, termNodeOpt], [r, t])
courseOpts r t c = (map z [repoNodeOpt, termNodeOpt, courseNodeOpt], [r, t, c])
groupOpts r t c g = (map z [repoNodeOpt, termNodeOpt, courseNodeOpt, groupNodeOpt], [r, t, c, g])
projectOpts r t c g p = (map z [repoNodeOpt, termNodeOpt, courseNodeOpt, groupNodeOpt, projectNodeOpt], [r, t, c, g, p])
trainRunOpts r t c g p tr = (map z [repoNodeOpt, termNodeOpt, courseNodeOpt, groupNodeOpt, projectNodeOpt, trainRunNodeOpt], [r, t, c, g, p,tr])
trainRunOutputOpts r t c g p tr m v = let l@(xs, ys) = trainRunOpts r t c g p tr
                                      in  if m then (xs ++ [z' v], ys ++ [Just ""]) else l

configThresholdOpts cu ch = (map z [configThresholdSetCurrentOpt, configThresholdSetChooseOpt], [cu, ch])
configTermDateOpts t1 t2 t3 = (map z [configTermDateSetTerm1Opt, configTermDateSetTerm2Opt, configTermDateSetTerm3Opt], [t1,t2,t3])
configProjectDateOpts end late = (map z [configProjectDateSetEndOpt, configProjectDateSetLateOpt], [end, late])

z = uncurry O
z' = uncurry B

termDateOpts r t s e = let (xs,ys) = termOpts r t
                       in  (xs ++ map z [termDateSetStartOpt, termDateSetEndOpt], ys ++ [s,e])

projectAddOpts r t c g s e l = let (xs,ys) = groupOpts r t c g
                               in  (xs ++ map z [projectAddStartOpt, projectAddEndOpt, projectAddLateOpt], ys ++ [s,e,l])
projectDateOpts r t c g p s e l = let (xs,ys) = projectOpts r t c g p
                                  in  (xs ++ map z [projectDateSetStartOpt, projectDateSetEndOpt, projectDateSetLateOpt], ys ++ [s,e,l])

validOpts :: [Maybe String] -> Bool
validOpts = notElem (Just "")

validArgs :: [String] -> [String]
validArgs = filter (not.null) . map noLeadingHyphens

noLeadingHyphens ('-':s) = noLeadingHyphens s
noLeadingHyphens s       = s

testSuccess :: (Eq a) => Int -> a -> (Global -> a) -> [String] -> Opts -> [String] -> Bool
testSuccess securityLevel expected f cmd opts args = all func $ makeCmd cmd opts args
 where func cmd' = expected == g cmd' && securityTests securityLevel cmd'
       g = f . h . execParserMaybe (globalInfo adminRole)
       h (Just x) = x
       h _ = error "Unexpectedly Nothing in testSuccess"

makeCmd :: [String] -> Opts -> [String] -> [[String]]
makeCmd cmd opts vars = [cmd ++ opt ++ vars | opt <- opts']
 where opts' = let result = uncurry makeOpts opts
               in  if null result then [[]] else result

makeOpts :: [Opt] -> [Maybe String] -> [[String]]
makeOpts os = take sampleSize.buildOpts.permutations.map f.filter (isJust.snd).zip os
 where f (a, Just b) = (a,b)
       f _ = error "Unexpectedly Nothing in makeOpts"

buildOpts :: [[(Opt,String)]] -> [[String]]
buildOpts = concatMap (filter (not.null).map concat.f)
 where f :: [(Opt, String)] -> [[[String]]]
       f [] = [[]]
       f (opt:os) = [s:ss  | s <- buildOpt opt, ss <- f os]

buildOpt :: (Opt, String) -> [[String]]
buildOpt (O s l, v) = [['-':s:v],
                       ['-':[s],v],
                       ["--" ++ l, v]]
buildOpt (B s l, _)   = [['-':[s]], ["--" ++ l]]

securityTests level cmd = all f $ zip [1..] [adminRole, teacherRole, correctorRole, studentRole]
 where f (n,role) = (if n <= level then security_success else security_failure) cmd role

security_failure cmd = isNothing . security_case cmd
security_success cmd = isJust . security_case cmd
security_case cmd role = execParserMaybe (globalInfo role) cmd



