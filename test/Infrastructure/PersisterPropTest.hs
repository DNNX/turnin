{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Infrastructure.PersisterPropTest where

import Test.Framework
import IOTestUtils
import Data.Maybe
import Control.Monad

import Infrastructure.Node
import Infrastructure.Persister

{-# ANN module "HLint: ignore Use camelCase" #-}

test_saveLoad = repeatTest $ inTmpDir $ do
  rootName <- randString 1 100
  nbChildren <- randInt 0 4
  maxDepth <- randInt 0 4
  
  n0 <- randNodeTree rootName nbChildren maxDepth
  n1 <- modifyTree n0 nbChildren maxDepth
  
  rootKey <- getRootKey
  notFound <- load rootKey rootName
  assertEqual Nothing notFound
  
  s <- save ["/" | null rootKey] n0
  assertBool $ not s
                      
  success <- save rootKey n0
  assertBool success
    
  n <- load rootKey rootName
  assertNodeEqualLocalRecursive rootKey n0 $ fromJust n
  
  success' <- save rootKey n1
  assertBool success'
  
  n' <- load rootKey rootName
  assertNodeEqualLocalRecursive rootKey n1 $ fromJust n'
  
assertNodeEqualLocalRecursive key expected actual = do
  assertEqual (getName expected) (getName actual)
  assertEqual (getConfigPairs expected) (getConfigPairs actual)
  assertEqual (getCachePairs expected) (getCachePairs actual)
  assertEqual (map getName $ getChildren expected) (map getName $ getChildren actual) -- Children are not loaded
  forM_ (getChildren actual) $ \child -> do
    assertEqual [] $ getConfigPairs child
    assertEqual [] $ getCachePairs child
    assertEqual [] $ getChildren child  
  forM_ (getChildren expected) $ \expectedChild -> let key' = key ++ [getName expected] in do                                                 
    actualChild <- load key' $ getName expectedChild
    assertNodeEqualLocalRecursive key' expectedChild $ fromJust actualChild
                      