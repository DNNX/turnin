{-# LANGUAGE FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns #-}
module Infrastructure.Finder.FinderCompileTest where

import Test.Framework

import Infrastructure.Node
import Domain.Root
import Domain.Project
import Domain.ProjectRepo

import Infrastructure.Finder
import Prelude hiding ((+))

{-# ANN module "HLint: ignore Use camelCase" #-}
{-# ANN module "HLint: ignore Reduce duplication" #-}
{-# ANN module "HLint: ignore Evaluate" #-}
{-# ANN module "HLint: ignore Use ++" #-}

class Add a b c | a b -> c where (+) :: a -> b -> c
instance Add Z b b where Z + b = b
instance Add a b c => Add (S a) b (S c) where (S x a) + b = S x (a + b)

rootN = "root"
rN = "repo"
tN = "term"
cN = "course"
gN = "group"
pN = "project"
trN = "trainRun"

sRN = getName emptySubmitRepo
tfRN = getName emptyTrainFileRepo
trRN = getName emptyTrainRunRepo

tr = make trN
trr = addChild emptyTrainRunRepo tr
p = setTrainRunRepo (make pN) trr
g = addChild (make gN) p
c = addChild (make cN) g
t = addChild (make tN) c
r = addChild (make rN) t
root = addChild (make rootN :: Root) r

sr = emptySubmitRepo
tfr = emptyTrainFileRepo

prs = [(Z, makeProjectSubmitRepo sr), (Z, makeProjectTrainFileRepo tfr), (Z, makeProjectTrainRunRepo trr)]

zero = Z
one = S Nothing zero
two = one + one
three = two + one
four = two + two
five = three + two
six = three + three
seven = four + three
eight = four + four
nine = five + four

rootK = K rootN
rK    = K rN
tK    = K tN
cK    = K cN
gK    = K gN
pK    = K pN
trrK  = K trRN

mapK f = map (h f) where h func (k,v) = (func k,v)

test_findZeroRoot          = assertEqual [(Z, root)] $ find zero root
test_findZeroRepo          = assertEqual [(Z, r)]    $ find zero r   
test_findZeroTerm          = assertEqual [(Z, t)]    $ find zero t   
test_findZeroCourse        = assertEqual [(Z, c)]    $ find zero c   
test_findZeroGroup         = assertEqual [(Z, g)]    $ find zero g   
test_findZeroProject       = assertEqual [(Z, p)]    $ find zero p   
test_findZeroSubmitRepo    = assertEqual [(Z,sr)]    $ find zero sr  
test_findZeroTrainFileRepo = assertEqual [(Z,tfr)]   $ find zero tfr 
test_findZeroTrainRunRepo  = assertEqual [(Z,trr)]   $ find zero trr 
test_findZeroTrainRun      = assertEqual [(Z, tr)]   $ find zero tr  

test_findOneRoot         = assertEqual [(rootK Z, r)] $ find one root
test_findOneRepo         = assertEqual [(rK Z, t)]    $ find one r   
test_findOneTerm         = assertEqual [(tK Z, c)]    $ find one t   
test_findOneCourse       = assertEqual [(cK Z, g)]    $ find one c   
test_findOneGroup        = assertEqual [(gK Z, p)]    $ find one g   
test_findOneProject      = assertEqual (mapK pK prs)  $ find one p   
test_findOneTrainRunRepo = assertEqual [(trrK Z, tr)] $ find one trr 

test_findTwoRoot    = assertEqual [((rootK.rK)Z, t)] $ find two root
test_findTwoRepo    = assertEqual [((rK.tK)Z, c)]    $ find two r   
test_findTwoTerm    = assertEqual [((tK.cK)Z, g)]    $ find two t   
test_findTwoCourse  = assertEqual [((cK.gK)Z, p)]    $ find two c   
test_findTwoGroup   = assertEqual (mapK (gK.pK) prs) $ find two g   
test_findTwoProject = assertEqual [((pK.trrK)Z,tr)]  $ find two p   

test_findThreeRoot   = assertEqual [((rootK.rK.tK)Z, c)] $ find three root
test_findThreeRepo   = assertEqual [((rK.tK.cK)Z, g)]    $ find three r   
test_findThreeTerm   = assertEqual [((tK.cK.gK)Z, p)]    $ find three t   
test_findThreeCourse = assertEqual (mapK (cK.gK.pK) prs) $ find three c      
test_findThreeGroup  = assertEqual [((gK.pK.trrK)Z,tr)]  $ find three g   

test_findFourRoot   = assertEqual [((rootK.rK.tK.cK)Z, g)] $ find four root   
test_findFourRepo   = assertEqual [((rK.tK.cK.gK)Z, p)]    $ find four r   
test_findFourTerm   = assertEqual (mapK (tK.cK.gK.pK) prs) $ find four t     
test_findFourCourse = assertEqual [((cK.gK.pK.trrK)Z,tr)]  $ find four c    

test_findFiveRoot = assertEqual [((rootK.rK.tK.cK.gK)Z, p)] $ find five root       
test_findFiveRepo = assertEqual (mapK (rK.tK.cK.gK.pK) prs) $ find five r         
test_findFiveTerm = assertEqual [((tK.cK.gK.pK.trrK)Z,tr)]  $ find five t        

test_findSixRoot = assertEqual (mapK (rootK.rK.tK.cK.gK.pK) prs) $ find six root 
test_findSixRepo = assertEqual [((rK.tK.cK.gK.pK.trrK)Z,tr)]     $ find six r   

test_findSevenRoot = assertEqual [((rootK.rK.tK.cK.gK.pK.trrK)Z,tr)] $ find seven root 

