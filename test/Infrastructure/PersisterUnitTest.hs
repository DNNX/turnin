{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Infrastructure.PersisterUnitTest where

import Test.Framework
import IOTestUtils

import Infrastructure.Node
import Infrastructure.Persister

{-# ANN module "HLint: ignore Use camelCase" #-}

test_saveLoad = inTmpDir $ do
  let root0 = setConfig (setCache (addChild (make "root") (addChild (make "child") (make "grandChild"))) "cacheKey" "cacheValue") "configKey" "configValue" 
      root1 = setConfig (setCache (addChild (make "root")           (make "child")                     ) "cacheKey" "cacheValue") "configKey" "configValue"
      root2 = setConfig (setCache (addChild (make "root")           (make "child")                     ) "cacheKey" "cacheValue1") "configKey" "configValue1"
  rootKey <- getRootKey  
  notFound <- load rootKey root0
  assertEqual Nothing notFound
                    
  s <- save ["/" | null rootKey] root0
  assertBool $ not s
                    
  success <- save rootKey root0
  assertBool success
  
  n <- load rootKey $ make "root"
  assertEqual (Just root1) n
  
  success' <- save rootKey root2
  assertBool success'
  
  n' <- load rootKey $ make "root"
  assertEqual (Just root2) n'
