module TestUtils where

import Data.List

uniqueNonEmpty = nub . filter (not.null)
uniqueNonEmptyNoComma = uniqueNonEmpty . map (filter (/=','))

applyGets x = nub . map ($ x)

applySets x = nub . map (foldl apply x) . permutations
 where apply y (g,d) = g y d
 
areEqual xs = [head xs] == nub xs
sameElements xs ys = null(xs \\ ys) && null (ys \\ xs)