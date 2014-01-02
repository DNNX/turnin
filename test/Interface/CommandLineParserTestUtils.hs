{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Interface.CommandLineParserTestUtils where

import Test.Framework
import Options.Applicative
import Data.List
import Data.Maybe
 
import Interface.CommandLineParser
import Interface.CommandLineLexicon

{-# ANN module "HLint: ignore Use camelCase" #-}

type Opts = ([Opt],[Maybe String])

noOptsToGet = ()
noOpts = ([],[])
noVariadics = []

validArgs :: [Maybe String] -> Bool
validArgs = notElem (Just "") 

noLeadingHyphens ('-':s) = noLeadingHyphens s
noLeadingHyphens s       = s

testSuccess :: (Eq a) => a -> (Global -> a) -> [String] -> Opts -> [String] -> Bool
testSuccess expected f cmd opts variadics = all ((expected ==).g) $ makeCmd cmd (uncurry makeOpts opts) variadics
 where g = f . h . execParserMaybe globalInfo
       h (Just x) = x
       h _ = error "Unexpectedly Nothing in testSuccess"

makeCmd :: [String] -> [[String]] -> [String] -> [[String]]
makeCmd cmd opts vars = [cmd ++ o ++ vars | o <- opts]

makeOpts :: [Opt] -> [Maybe String] -> [[String]]
makeOpts os = buildOpts . permutations . map f . filter (isJust.snd) . zip os
 where f (a, Just b) = (a,b)
       f _ = error "Unexpectedly Nothing in makeOpts"

buildOpts :: [[(Opt,String)]] -> [[String]]
buildOpts = concatMap (map concat.f)
 where f :: [(Opt, String)] -> [[[String]]]
       f [] = [[]]
       f (o:os) = [s:ss  | s <- buildOpt o, ss <- f os]

buildOpt :: (Opt, String) -> [[String]]
buildOpt (O s l, v) = [['-':s:v],
                       ['-':[s],v],
                       ["--" ++ l, v]]

                       
