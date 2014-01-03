{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where
import Test.Framework

import {-@ HTF_TESTS @-} Interface.CommandLineParser.ConfigParserTest
import {-@ HTF_TESTS @-} Interface.CommandLineParser.RepoTermParserTest
import {-@ HTF_TESTS @-} Interface.CommandLineParser.CourseGroupParserTest
import {-@ HTF_TESTS @-} Interface.CommandLineParser.ProjectValidateParserTest

main :: IO ()
main = htfMain htf_importedTests
