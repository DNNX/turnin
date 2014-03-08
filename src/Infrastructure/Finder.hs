{-# LANGUAGE FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}
module Infrastructure.Finder where

import Control.Applicative
 
import Infrastructure.Node
{- Tests-Examples
import Domain.Root
import Domain.Repo
import Domain.Term
import Domain.Course
import Domain.Group
import Domain.Project
import Domain.ProjectRepo
import Domain.TrainRunRepo
import Domain.TrainRun
import Prelude hiding ((+))
-}

data Z = Z     deriving Show
data S s = S s deriving Show

class Size s where
instance Size Z where
instance Size s => Size (S s) where

class HasNode a => FindNested s a b | s a -> b where findNested :: s -> a -> b
instance HasNode a => FindNested Z a [a] where findNested Z = pure
instance (HasNode a, FindNested s (ChildType a) b) => FindNested (S s) a [b] where findNested (S s) = map (s `findNested`) . getChildren

class Flatten s a b | s a -> b where flatten :: s -> a -> b
instance Flatten Z [a] [a] where flatten Z = id
instance Flatten s [a] b => Flatten (S s) [[a]] b where flatten (S s) = flatten s . concat

find s = flatten s . findNested s

{- Tests-Examples
tr = make "trainRun"
trr = addChild emptyTrainRunRepo tr
p = setTrainRunRepo (make "project") trr
g = addChild (make "group") p
c = addChild (make "course") g
t = addChild (make "term") c
r = addChild (make "repo") t
root = addChild (make "root" :: Root) r

class Add a b c | a b -> c where (+) :: a -> b -> c
instance Add Z b b where Z + b = b
instance Add a b c => Add (S a) b (S c) where (S a) + b = S (a + b)

zero = Z
one = S zero
two = one + one
three = two + one
four = two + two
five = three + two
six = three + three
seven = four + three
eight = four + four

findNestedZeroRoot         = findNested zero root :: [Root]
findNestedZeroRepo         = findNested zero r :: [Repo]
findNestedZeroTerm         = findNested zero t :: [Term]
findNestedZeroCourse       = findNested zero c :: [Course]
findNestedZeroGroup        = findNested zero g :: [Group]
findNestedZeroProject      = findNested zero p :: [Project]
findNestedZeroTrainRunRepo = findNested zero trr :: [TrainRunRepo]
findNestedZeroTrainRun     = findNested zero tr :: [TrainRun]

findNestedOneRoot        = findNested one root :: [[Repo]]
findNestedOneRepo        = findNested one r :: [[Term]]
findNestedOneTerm        = findNested one t :: [[Course]]
findNestedOneCourse      = findNested one c :: [[Group]]
findNestedOneGroup       = findNested one g :: [[Project]]
findNestedOneProject     = findNested one p :: [[ProjectRepo]]
findNestedOneProjectRepo = findNested one trr :: [[TrainRun]]
--findNestedOneTrainRun    = findNested one tr :: [[()]] -- No instance for (HasNode ())

findNestedSevenRoot      = findNested seven root :: [[[[[[[[TrainRun]]]]]]]]
--findNestedSevenRepo      = findNested seven r :: [[[[[[[[()]]]]]]]] -- No instance for (HasNode ())

-- Cannot flatten non-lists
--flattenZeroRoot         = flatten zero root :: [Root]
--flattenZeroRepo         = flatten zero r :: [Repo]
--flattenZeroTerm         = flatten zero t :: [Term]
--flattenZeroCourse       = flatten zero c :: [Course]
--flattenZeroGroup        = flatten zero g :: [Group]
--flattenZeroProject      = flatten zero p :: [Project]
--flattenZeroTrainRunRepo = flatten zero trr :: [TrainRunRepo]
--flattenZeroTrainRun     = flatten zero tr :: [TrainRun]

flattenZeroOneRoot         = flatten zero [root] :: [Root]
flattenZeroOneRepo         = flatten zero [r] :: [Repo]
flattenZeroOneTerm         = flatten zero [t] :: [Term]
flattenZeroOneCourse       = flatten zero [c] :: [Course]
flattenZeroOneGroup        = flatten zero [g] :: [Group]
flattenZeroOneProject      = flatten zero [p] :: [Project]
flattenZeroOneTrainRunRepo = flatten zero [trr] :: [TrainRunRepo]
flattenZeroOneTrainRun     = flatten zero [tr] :: [TrainRun]

--flattenOneOneRoot         = flatten one [root] :: [Root] -- Can't flatten away the list

flattenZeroTwoRoot         = flatten zero [[root]] :: [[Root]]
flattenZeroTwoRepo         = flatten zero [[r]] :: [[Repo]]
flattenZeroTwoTerm         = flatten zero [[t]] :: [[Term]]
flattenZeroTwoCourse       = flatten zero [[c]] :: [[Course]]
flattenZeroTwoGroup        = flatten zero [[g]] :: [[Group]]
flattenZeroTwoProject      = flatten zero [[p]] :: [[Project]]
flattenZeroTwoTrainRunRepo = flatten zero [[trr]] :: [[TrainRunRepo]]
flattenZeroTwoTrainRun     = flatten zero [[tr]] :: [[TrainRun]]

flattenOneTwoRoot         = flatten one [[root]] :: [Root]
flattenOneTwoRepo         = flatten one [[r]] :: [Repo]
flattenOneTwoTerm         = flatten one [[t]] :: [Term]
flattenOneTwoCourse       = flatten one [[c]] :: [Course]
flattenOneTwoGroup        = flatten one [[g]] :: [Group]
flattenOneTwoProject      = flatten one [[p]] :: [Project]
flattenOneTwoTrainRunRepo = flatten one [[trr]] :: [TrainRunRepo]
flattenOneTwoTrainRun     = flatten one [[tr]] :: [TrainRun]

--flattenTwoTwoRoot         = flatten two [[root]] :: [Root] -- Can't flatten away the list

findZeroRoot         = find zero root :: [Root]
findZeroRepo         = find zero r :: [Repo]
findZeroTerm         = find zero t :: [Term]
findZeroCourse       = find zero c :: [Course]
findZeroGroup        = find zero g :: [Group]
findZeroProject      = find zero p :: [Project]
findZeroTrainRunRepo = find zero trr :: [TrainRunRepo]
findZeroTrainRun     = find zero tr :: [TrainRun]

findOneRoot         = find one root :: [Repo]
findOneRepo         = find one r :: [Term]
findOneTerm         = find one t :: [Course]
findOneCourse       = find one c :: [Group]
findOneGroup        = find one g :: [Project]
findOneProject      = find one p :: [ProjectRepo]
findOneTrainRunRepo = find one trr :: [TrainRun]
--findOneTrainRun     = find one tr :: [()] -- No instance for (HasNode ())

findSevenRoot       = find seven root :: [TrainRun]
--findEightRoot       = find eight root :: [()] -- No instance for (HasNode ())

-}