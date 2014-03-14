{-# OPTIONS_GHC -F -pgmF htfpp #-}
module Main where
import Test.Framework

import {-@ HTF_TESTS @-} Interface.CommandLineParser.ConfigUnitTest
import {-@ HTF_TESTS @-} Interface.CommandLineParser.RepoTermUnitTest
import {-@ HTF_TESTS @-} Interface.CommandLineParser.CourseGroupUnitTest
import {-@ HTF_TESTS @-} Interface.CommandLineParser.ProjectValidateUnitTest
import {-@ HTF_TESTS @-} Interface.CommandLineParser.ProjectWorktrainSubmitUnitTest
import {-@ HTF_TESTS @-} Interface.CommandLineParser.SubmitWorktrainUnitTest

import {-@ HTF_TESTS @-} Infrastructure.NodeUnitTest
import {-@ HTF_TESTS @-} Infrastructure.CsvNodeUnitTest
import {-@ HTF_TESTS @-} Infrastructure.DateUnitTest
import {-@ HTF_TESTS @-} Infrastructure.PersisterPropTest
import {-@ HTF_TESTS @-} Infrastructure.Finder.FinderPropFindingTest
import {-@ HTF_TESTS @-} Infrastructure.Finder.FinderPropNoFindingTest

import {-@ HTF_TESTS @-} Domain.HierarchyUnitTest
import {-@ HTF_TESTS @-} Domain.RootUnitTest
import {-@ HTF_TESTS @-} Domain.TermUnitTest
import {-@ HTF_TESTS @-} Domain.GroupUnitTest
import {-@ HTF_TESTS @-} Domain.ProjectUnitTest
import {-@ HTF_TESTS @-} Domain.ProjectHierarchyUnitTest

import {-@ HTF_TESTS @-} Service.AutomaticTermNamesUnitTest
import {-@ HTF_TESTS @-} Service.WorktrainOutputFormatterUnitTest


import {-@ HTF_TESTS @-} Interface.CommandLineParser.ConfigPropTest
import {-@ HTF_TESTS @-} Interface.CommandLineParser.RepoTermPropTest
import {-@ HTF_TESTS @-} Interface.CommandLineParser.CourseGroupPropTest
import {-@ HTF_TESTS @-} Interface.CommandLineParser.ProjectValidatePropTest
import {-@ HTF_TESTS @-} Interface.CommandLineParser.ProjectWorktrainSubmitPropTest
import {-@ HTF_TESTS @-} Interface.CommandLineParser.SubmitWorktrainPropTest

import {-@ HTF_TESTS @-} Infrastructure.NodePropTest
import {-@ HTF_TESTS @-} Infrastructure.CsvNodePropTest
import {-@ HTF_TESTS @-} Infrastructure.DatePropTest
import {-@ HTF_TESTS @-} Infrastructure.PersisterUnitTest
import {-@ HTF_TESTS @-} Infrastructure.Finder.FinderCompileTest
import {-@ HTF_TESTS @-} Infrastructure.Finder.FinderUnitFindingTest
import {-@ HTF_TESTS @-} Infrastructure.Finder.FinderUnitNoFindingTest

import {-@ HTF_TESTS @-} Domain.HierarchyPropTest
import {-@ HTF_TESTS @-} Domain.RootPropTest
import {-@ HTF_TESTS @-} Domain.TermPropTest
import {-@ HTF_TESTS @-} Domain.GroupPropTest
import {-@ HTF_TESTS @-} Domain.ProjectPropTest
import {-@ HTF_TESTS @-} Domain.ProjectHierarchyPropTest

import {-@ HTF_TESTS @-} Service.AutomaticTermNamesPropTest
import {-@ HTF_TESTS @-} Service.WorktrainOutputFormatterPropTest

main :: IO ()
main = htfMain htf_importedTests
