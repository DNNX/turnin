{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where
import Test.Framework

import {-@ HTF_TESTS @-} Interface.CommandLineParser.ConfigTest
import {-@ HTF_TESTS @-} Interface.CommandLineParser.RepoTermTest
import {-@ HTF_TESTS @-} Interface.CommandLineParser.CourseGroupTest
import {-@ HTF_TESTS @-} Interface.CommandLineParser.ProjectValidateTest
import {-@ HTF_TESTS @-} Interface.CommandLineParser.ProjectWorktrainSubmitTest
import {-@ HTF_TESTS @-} Interface.CommandLineParser.SubmitWorktrainTest
import {-@ HTF_TESTS @-} Infrastructure.NodeTest

main :: IO ()
main = htfMain htf_importedTests
