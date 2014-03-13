{-# LANGUAGE FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}
module Infrastructure.FinderCompileTest where

import Infrastructure.Node
import Domain.Root
import Domain.Project
import Domain.ProjectRepo

import Infrastructure.Finder
import Prelude hiding ((+))

class Add a b c | a b -> c where (+) :: a -> b -> c
instance Add Z b b where Z + b = b
instance Add a b c => Add (S a) b (S c) where (S x a) + b = S x (a + b)

tr = make "trainRun"
trr = addChild emptyTrainRunRepo tr
p = setTrainRunRepo (make "project") trr
g = addChild (make "group") p
c = addChild (make "course") g
t = addChild (make "term") c
r = addChild (make "repo") t
root = addChild (make "root" :: Root) r

ptrr = makeProjectTrainRunRepo trr

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

--prop_findNestedOneTrainRun     = findNested one tr     == [[()]]
--prop_findNestedTwoTrainRunRepo = findNested two trr    == [[[()]]]
--prop_findNestedThreeProject    = findNested three p    == [[[[()]]]]
--prop_findNestedFourGroup       = findNested four g     == [[[[[()]]]]]
--prop_findNestedFiveCourse      = findNested five c     == [[[[[[()]]]]]]
--prop_findNestedSixTerm         = findNested six t      == [[[[[[[()]]]]]]]
--prop_findNestedSevenRepo       = findNested seven r    == [[[[[[[[()]]]]]]]]
--prop_findNestedEightRoot       = findNested eight root == [[[[[[[[[()]]]]]]]]]

prop_findNestedZeroRoot         = findNested zero root == [root]
prop_findNestedZeroRepo         = findNested zero r    == [r]
prop_findNestedZeroTerm         = findNested zero t    == [t]
prop_findNestedZeroCourse       = findNested zero c    == [c]
prop_findNestedZeroGroup        = findNested zero g    == [g]
prop_findNestedZeroProject      = findNested zero p    == [p]
prop_findNestedZeroTrainRunRepo = findNested zero trr  == [trr]
prop_findNestedZeroTrainRun     = findNested zero tr   == [tr]

prop_findNestedOneRoot         = findNested one root == [[r]]
prop_findNestedOneRepo         = findNested one r    == [[t]]
prop_findNestedOneTerm         = findNested one t    == [[c]]
prop_findNestedOneCourse       = findNested one c    == [[g]]
prop_findNestedOneGroup        = findNested one g    == [[p]]
prop_findNestedOneProject      = findNested one p    == [[ptrr]]
prop_findNestedOneTrainRunRepo = findNested one trr  == [[tr]]

prop_findNestedTwoRoot    = findNested two root == [[[t]]]
prop_findNestedTwoRepo    = findNested two r    == [[[c]]]
prop_findNestedTwoTerm    = findNested two t    == [[[g]]]
prop_findNestedTwoCourse  = findNested two c    == [[[p]]]
prop_findNestedTwoGroup   = findNested two g    == [[[ptrr]]]
prop_findNestedTwoProject = findNested two p    == [[[tr]]]

prop_findNestedThreeRoot   = findNested three root == [[[[c]]]]
prop_findNestedThreeRepo   = findNested three r    == [[[[g]]]]
prop_findNestedThreeTerm   = findNested three t    == [[[[p]]]]
prop_findNestedThreeCourse = findNested three c    == [[[[ptrr]]]]
prop_findNestedThreeGroup  = findNested three g    == [[[[tr]]]]

prop_findNestedFourRoot   = findNested four root == [[[[[g]]]]]
prop_findNestedFourRepo   = findNested four r    == [[[[[p]]]]]
prop_findNestedFourTerm   = findNested four t    == [[[[[ptrr]]]]]
prop_findNestedFourCourse = findNested four c    == [[[[[tr]]]]]

prop_findNestedFiveRoot = findNested five root == [[[[[[p]]]]]]
prop_findNestedFiveRepo = findNested five r    == [[[[[[ptrr]]]]]]
prop_findNestedFiveTerm = findNested five t    == [[[[[[tr]]]]]]

prop_findNestedSixRoot = findNested six root == [[[[[[[ptrr]]]]]]]
prop_findNestedSixRepo = findNested six r    == [[[[[[[tr]]]]]]]

prop_findNestedSevenRoot = findNested seven root == [[[[[[[[tr]]]]]]]]

--prop_flattenZeroZeroRoot         = flatten zero root == root
--prop_flattenZeroZeroRepo         = flatten zero r    == r
--prop_flattenZeroZeroTerm         = flatten zero t    == t
--prop_flattenZeroZeroCourse       = flatten zero c    == c
--prop_flattenZeroZeroGroup        = flatten zero g    == g
--prop_flattenZeroZeroProject      = flatten zero p    == p
--prop_flattenZeroZeroTrainRunRepo = flatten zero trr  == trr
--prop_flattenZeroZeroTrainRun     = flatten zero tr   == tr
--
--prop_flattenOneOneRoot         = flatten one [root] == root
--prop_flattenOneOneRepo         = flatten one [r]    == r
--prop_flattenOneOneTerm         = flatten one [t]    == t
--prop_flattenOneOneCourse       = flatten one [c]    == c
--prop_flattenOneOneGroup        = flatten one [g]    == g
--prop_flattenOneOneProject      = flatten one [p]    == p
--prop_flattenOneOneTrainRunRepo = flatten one [trr]  == trr
--prop_flattenOneOneTrainRun     = flatten one [tr]   == tr

prop_flattenZeroOneRoot         = flatten zero [root] == [root]
prop_flattenZeroOneRepo         = flatten zero [r]    == [r]
prop_flattenZeroOneTerm         = flatten zero [t]    == [t]
prop_flattenZeroOneCourse       = flatten zero [c]    == [c]
prop_flattenZeroOneGroup        = flatten zero [g]    == [g]
prop_flattenZeroOneProject      = flatten zero [p]    == [p]
prop_flattenZeroOneTrainRunRepo = flatten zero [trr]  == [trr]
prop_flattenZeroOneTrainRun     = flatten zero [tr]   == [tr]

prop_flattenOneTwoRoot         = flatten one [[root]] == [root]
prop_flattenOneTwoRepo         = flatten one [[r]]    == [r]
prop_flattenOneTwoTerm         = flatten one [[t]]    == [t]
prop_flattenOneTwoCourse       = flatten one [[c]]    == [c]
prop_flattenOneTwoGroup        = flatten one [[g]]    == [g]
prop_flattenOneTwoProject      = flatten one [[p]]    == [p]
prop_flattenOneTwoTrainRunRepo = flatten one [[trr]]  == [trr]
prop_flattenOneTwoTrainRun     = flatten one [[tr]]   == [tr]

prop_flattenOneThreeRoot         = flatten one [[[root]]] == [[root]]
prop_flattenOneThreeRepo         = flatten one [[[r]]]    == [[r]]
prop_flattenOneThreeTerm         = flatten one [[[t]]]    == [[t]]
prop_flattenOneThreeCourse       = flatten one [[[c]]]    == [[c]]
prop_flattenOneThreeGroup        = flatten one [[[g]]]    == [[g]]
prop_flattenOneThreeProject      = flatten one [[[p]]]    == [[p]]
prop_flattenOneThreeTrainRunRepo = flatten one [[[trr]]]  == [[trr]]
prop_flattenOneThreeTrainRun     = flatten one [[[tr]]]   == [[tr]]

prop_flattenTwoThreeRoot         = flatten two [[[root]]] == [root]
prop_flattenTwoThreeRepo         = flatten two [[[r]]]    == [r]
prop_flattenTwoThreeTerm         = flatten two [[[t]]]    == [t]
prop_flattenTwoThreeCourse       = flatten two [[[c]]]    == [c]
prop_flattenTwoThreeGroup        = flatten two [[[g]]]    == [g]
prop_flattenTwoThreeProject      = flatten two [[[p]]]    == [p]
prop_flattenTwoThreeTrainRunRepo = flatten two [[[trr]]]  == [trr]
prop_flattenTwoThreeTrainRun     = flatten two [[[tr]]]   == [tr]

--prop_findEightRoot       = find eight root == [()]
--prop_findSevenRepo       = find seven r    == [()]
--prop_findSixTerm         = find six t      == [()]
--prop_findFiveCourse      = find five c     == [()]
--prop_findFourGroup       = find four g     == [()]
--prop_findThreeProject    = find three p    == [()]
--prop_findTwoTrainRunRepo = find two trr    == [()]
--prop_findOneTrainRun     = find one tr     == [()]

prop_findZeroRoot         = find zero root == [root]
prop_findZeroRepo         = find zero r    == [r]
prop_findZeroTerm         = find zero t    == [t]
prop_findZeroCourse       = find zero c    == [c]
prop_findZeroGroup        = find zero g    == [g]
prop_findZeroProject      = find zero p    == [p]
prop_findZeroTrainRunRepo = find zero trr  == [trr]
prop_findZeroTrainRun     = find zero tr   == [tr]

prop_findOneRoot         = find one root == [r]
prop_findOneRepo         = find one r    == [t]
prop_findOneTerm         = find one t    == [c]
prop_findOneCourse       = find one c    == [g]
prop_findOneGroup        = find one g    == [p]
prop_findOneProject      = find one p    == [ptrr]
prop_findOneTrainRunRepo = find one trr  == [tr]

prop_findTwoRoot    = find two root == [t]
prop_findTwoRepo    = find two r    == [c]
prop_findTwoTerm    = find two t    == [g]
prop_findTwoCourse  = find two c    == [p]
prop_findTwoGroup   = find two g    == [ptrr]
prop_findTwoProject = find two p    == [tr]

prop_findThreeRoot   = find three root == [c]
prop_findThreeRepo   = find three r    == [g]
prop_findThreeTerm   = find three t    == [p]
prop_findThreeCourse = find three c    == [ptrr]
prop_findThreeGroup  = find three g    == [tr]

prop_findFourRoot   = find four root == [g]
prop_findFourRepo   = find four r    == [p]
prop_findFourTerm   = find four t    == [ptrr]
prop_findFourCourse = find four c    == [tr]

prop_findFiveRoot = find five root == [p]
prop_findFiveRepo = find five r    == [ptrr]
prop_findFiveTerm = find five t    == [tr]

prop_findSixRoot = find six root == [ptrr]
prop_findSixRepo = find six r    == [tr]

prop_findSevenRoot = find seven root == [tr]











