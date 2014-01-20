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
import {-@ HTF_TESTS @-} Infrastructure.CsvNodeTest
import {-@ HTF_TESTS @-} Infrastructure.DateTest

import {-@ HTF_TESTS @-} Domain.HierarchyTest
import {-@ HTF_TESTS @-} Domain.RootTest
import {-@ HTF_TESTS @-} Domain.TermTest
import {-@ HTF_TESTS @-} Domain.GroupTest
import {-@ HTF_TESTS @-} Domain.ProjectTest

import {-@ HTF_TESTS @-} Service.AutomaticTermNamesTest


main :: IO ()
main = htfMain htf_importedTests
