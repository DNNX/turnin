{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Infrastructure.PersisterUnitTest where

import Test.Framework
import IOTestUtils

import Infrastructure.Persister

{-# ANN module "HLint: ignore Use camelCase" #-}

test_saveAndLoadingEmpty = inTmpDir $ do
  c0 <- load []
  assertEqual Nothing c0

  save [] "content"
  c1 <- load []
  assertEqual Nothing c1

  delete []
  c2 <- load []
  assertEqual Nothing c2

test_saveAndLoading = inTmpDir $ do
  c0 <- load ["file"]
  assertEqual Nothing c0

  save ["file"] "content"
  c1 <- load ["file"]
  assertEqual (Just "content") c1
  
  c2 <- load ["dir", "file"]
  assertEqual Nothing c2

  save ["dir", "file"] "sub content"
  c3 <- load ["dir", "file"]
  assertEqual (Just "sub content") c3
  
  delete ["dir"]
  c4 <- load ["dir", "file"]
  assertEqual Nothing c4
  
  delete ["file"]
  c5 <- load ["file"]
  assertEqual Nothing c5