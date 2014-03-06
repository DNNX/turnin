{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Infrastructure.PersisterUnitTest where

import Test.Framework
import IOTestUtils

import Infrastructure.Node
import Infrastructure.Persister

{-# ANN module "HLint: ignore Use camelCase" #-}

test_saveLoad = inTmpDir $ let rootName = "root" in do
  let root0 = setConfig (setCache (addChild (make rootName) (addChild (make "child") (make "grandChild"))) "cacheKey" "cacheValue") "configKey" "configValue" 
      root1 = setConfig (setCache (addChild (make rootName)           (make "child")                     ) "cacheKey" "cacheValue") "configKey" "configValue"
      root2 = setConfig (setCache (addChild (make rootName)           (make "child")                     ) "cacheKey" "cacheValue1") "configKey" "configValue1"
  rootKey <- getRootKey  
  notFound <- load rootKey rootName
  assertEqual Nothing notFound
                    
  s <- save ["/" | null rootKey] root0
  assertBool $ not s
                    
  success <- save rootKey root0
  assertBool success
  
  n <- load rootKey rootName
  assertEqual (Just root1) n
  
  success' <- save rootKey root2
  assertBool success'
  
  n' <- load rootKey rootName
  assertEqual (Just root2) n'
