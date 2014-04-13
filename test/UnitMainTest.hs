{-# OPTIONS_GHC -F -pgmF htfpp #-}
module UnitMainTest where
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
import {-@ HTF_TESTS @-} Infrastructure.PersisterUnitTest
import {-@ HTF_TESTS @-} Infrastructure.Finder.FinderUnitFindingTest
import {-@ HTF_TESTS @-} Infrastructure.Finder.FinderUnitNoFindingTest
import {-@ HTF_TESTS @-} Infrastructure.Finder.DisambiguationUnitTest

import {-@ HTF_TESTS @-} Domain.HierarchyUnitTest
import {-@ HTF_TESTS @-} Domain.RootUnitTest
import {-@ HTF_TESTS @-} Domain.TermUnitTest
import {-@ HTF_TESTS @-} Domain.GroupUnitTest
import {-@ HTF_TESTS @-} Domain.ProjectUnitTest
import {-@ HTF_TESTS @-} Domain.ProjectHierarchyUnitTest

import {-@ HTF_TESTS @-} Service.AutomaticTermNamesUnitTest
import {-@ HTF_TESTS @-} Service.WorktrainOutputFormatterUnitTest

main :: IO ()
main = htfMain htf_importedTests
