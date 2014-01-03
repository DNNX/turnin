{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Interface.CommandLineParserTest.Utils where

import Test.Framework
import Options.Applicative
import Data.List
import Data.Maybe
import Interface.Lexicon

import Interface.CommandLineParser

{-# ANN module "HLint: ignore Use camelCase" #-}

sampleSize = 7 -- Keep this many random option permutations, tests runtime is exponential in this number

type Opt = (Char, String)
type Opts = ([Opt],[Maybe String])
 
noArgsToGet = ()
noOpts = ([],[])
noArgs = []

repoOpts r = ([repoNodeOpt], [r])
termOpts r t = ([repoNodeOpt, termNodeOpt], [r, t])
courseOpts r t c = ([repoNodeOpt, termNodeOpt, courseNodeOpt], [r, t, c])
groupOpts r t c g = ([repoNodeOpt, termNodeOpt, courseNodeOpt, groupNodeOpt], [r, t, c, g])
projectOpts r t c g p = ([repoNodeOpt, termNodeOpt, courseNodeOpt, groupNodeOpt, projectNodeOpt], [r, t, c, g, p])

thresholdOpts cu ch = ([configThresholdSetCurrentOpt, configThresholdSetChooseOpt], [cu, ch])
configTermDateOpts t1 t2 t3 = ([configTermDateSetTerm1Opt, configTermDateSetTerm2Opt, configTermDateSetTerm3Opt], [t1,t2,t3])
configProjectDateOpts end late = ([configProjectDateSetEndOpt, configProjectDateSetLateOpt], [end, late])

termDateOpts r t s e = let (xs,ys) = termOpts r t
                       in  (xs ++ [termDateSetStartOpt, termDateSetEndOpt], ys ++ [s,e]) 

projectAddOpts r t c g s e l = let (xs,ys) = groupOpts r t c g
                               in  (xs ++ [projectAddStartOpt, projectAddEndOpt, projectAddLateOpt], ys ++ [s,e,l])
projectDateOpts r t c g p s e l = let (xs,ys) = projectOpts r t c g p
                                  in  (xs ++ [projectDateSetStartOpt, projectDateSetEndOpt, projectDateSetLateOpt], ys ++ [s,e,l])

validOpts :: [Maybe String] -> Bool
validOpts = notElem (Just "")

validArgs :: [String] -> [String]
validArgs = filter (not.null) . map noLeadingHyphens

noLeadingHyphens ('-':s) = noLeadingHyphens s
noLeadingHyphens s       = s

testSuccess :: (Eq a) => a -> (Global -> a) -> [String] -> Opts -> [String] -> Bool
testSuccess expected f cmd opts args = all ((expected ==).g) $ makeCmd cmd (uncurry makeOpts opts) args
 where g = f . h . execParserMaybe globalInfo
       h (Just x) = x
       h _ = error "Unexpectedly Nothing in testSuccess"

makeCmd :: [String] -> [[String]] -> [String] -> [[String]]
makeCmd cmd opts vars = [cmd ++ o ++ vars | o <- opts]

makeOpts :: [Opt] -> [Maybe String] -> [[String]]
makeOpts os = take sampleSize.buildOpts.permutations.map f.filter (isJust.snd).zip os
 where f (a, Just b) = (a,b)
       f _ = error "Unexpectedly Nothing in makeOpts"

buildOpts :: [[(Opt,String)]] -> [[String]]
buildOpts = concatMap (map concat.f)
 where f :: [(Opt, String)] -> [[[String]]]
       f [] = [[]]
       f (o:os) = [s:ss  | s <- buildOpt o, ss <- f os]

buildOpt :: (Opt, String) -> [[String]]
buildOpt ((s, l), v) = [['-':s:v],
                       ['-':[s],v],
                       ["--" ++ l, v]]

