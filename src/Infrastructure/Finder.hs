{-# LANGUAGE FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}
module Infrastructure.Finder
( findNested
, flatten
, find
, Z(Z)
, S(S)
) where

import Control.Applicative
import Infrastructure.Node

data Z = Z     deriving Show
data S s = S (Maybe String) s deriving Show

class Size s where
instance Size Z where
instance Size s => Size (S s) where

class HasNode a => FindNested s a b | s a -> b where findNested :: s -> a -> b
instance HasNode a => FindNested Z a [a] where findNested Z = pure
instance (HasNode a, FindNested s (ChildType a) b) => FindNested (S s) a [b] where findNested (S x s) = map (s `findNested`) . filter (matchesCriteria x). getChildren

class Flatten s a b | s a -> b where flatten :: s -> a -> b
instance Flatten Z [a] [a] where flatten Z = id
instance Flatten s [a] b => Flatten (S s) [[a]] b where flatten (S _ s) = flatten s . concat

find s = flatten s . findNested s

matchesCriteria Nothing _  = True
matchesCriteria (Just s) n = s == getName n