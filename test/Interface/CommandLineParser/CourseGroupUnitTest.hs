{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Interface.CommandLineParser.CourseGroupUnitTest where

import Test.Framework
import Interface.CommandLineParser.ParserTestUtils
import Control.Monad
import Security.SecurityManager
import Options.Applicative

import Interface.Lexicon
import Interface.CommandLineParser
import Interface.CommandLineParser.Course
import Interface.CommandLineParser.Group

{-# ANN module "HLint: ignore Use camelCase" #-}

-- Course
test_course = forM_ allRoles $ \role -> assertEqual Nothing $ execParserMaybe (globalInfo role) [courseSub]
test_courseAdd = do
  forM_ [adminRole, teacherRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [courseSub, addSub]
    assertEqual (Just(Global(Course(CourseOpts(CourseAdd(CourseAddOpts o o "name")))))) $ execParserMaybe (globalInfo role) [courseSub, addSub, "name"]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [courseSub, addSub, "name", "v"]
  forM_ [correctorRole, studentRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [courseSub, addSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [courseSub, addSub, "name"]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [courseSub, addSub, "name", "v"]

test_courseRemove = do
  forM_ [adminRole, teacherRole] $ \role -> do
    assertEqual (Just(Global(Course(CourseOpts(CourseRemove(CourseRemoveOpts o o o)))))) $ execParserMaybe (globalInfo role) [courseSub, removeSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [courseSub, removeSub, "v"]
  forM_ [correctorRole, studentRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [courseSub, removeSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [courseSub, removeSub, "v"]

test_courseList = do
  forM_ [adminRole, teacherRole, correctorRole] $ \role -> do
    assertEqual (Just(Global(Course(CourseOpts(CourseList(CourseListOpts o o)))))) $ execParserMaybe (globalInfo role) [courseSub, listSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [courseSub, listSub, "v"]
  assertEqual Nothing $ execParserMaybe (globalInfo studentRole) [courseSub, listSub]
  assertEqual Nothing $ execParserMaybe (globalInfo studentRole) [courseSub, listSub, "v"]

test_courseTeacher = forM_ allRoles $ \role -> assertEqual Nothing $ execParserMaybe (globalInfo role) [courseSub, teacherSub]
test_courseTeacherAdd = do
  forM_ [adminRole, teacherRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [courseSub, teacherSub, addSub]
    assertEqual (Just(Global(Course(CourseOpts(CourseTeacher(CourseTeacherOpts(CourseTeacherAdd(CourseTeacherAddOpts o o o ["name.."])))))))) $
                                                                                                        execParserMaybe (globalInfo role) [courseSub, teacherSub, addSub, "name.."]
  forM_ [correctorRole, studentRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [courseSub, teacherSub, addSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [courseSub, teacherSub, addSub, "name.."]

test_courseTeacherRemove = do
  forM_ [adminRole, teacherRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [courseSub, teacherSub, addSub]
    assertEqual (Just(Global(Course(CourseOpts(CourseTeacher(CourseTeacherOpts(CourseTeacherRemove(CourseTeacherRemoveOpts o o o ["name.."])))))))) $
                                                                                                        execParserMaybe (globalInfo role) [courseSub, teacherSub, removeSub, "name.."]
  forM_ [correctorRole, studentRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [courseSub, teacherSub, removeSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [courseSub, teacherSub, removeSub, "name.."]


test_courseTeacherList = do
  forM_ [adminRole, teacherRole, correctorRole] $ \role -> do
    assertEqual (Just(Global(Course(CourseOpts(CourseTeacher(CourseTeacherOpts(CourseTeacherList(CourseTeacherListOpts o o o)))))))) $ execParserMaybe (globalInfo role) [courseSub, teacherSub, listSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [courseSub, teacherSub, listSub, "v"]
  assertEqual Nothing $ execParserMaybe (globalInfo studentRole) [courseSub, teacherSub, listSub]
  assertEqual Nothing $ execParserMaybe (globalInfo studentRole) [courseSub, teacherSub, listSub, "v"]

test_courseCorrector = forM_ allRoles $ \role -> assertEqual Nothing $ execParserMaybe (globalInfo role) [courseSub, teacherSub]
test_courseCorrectorAdd = do
  forM_ [adminRole, teacherRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [courseSub, correctorSub, addSub]
    assertEqual (Just(Global(Course(CourseOpts(CourseCorrector(CourseCorrectorOpts(CourseCorrectorAdd(CourseCorrectorAddOpts o o o ["name.."])))))))) $
                                                                                                execParserMaybe (globalInfo role) [courseSub, correctorSub, addSub, "name.."]
  forM_ [correctorRole, studentRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [courseSub, correctorSub, addSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [courseSub, correctorSub, addSub, "name.."]

test_courseCorrectorRemove = do
  forM_ [adminRole, teacherRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [courseSub, teacherSub, addSub]
    assertEqual (Just(Global(Course(CourseOpts(CourseCorrector(CourseCorrectorOpts(CourseCorrectorRemove(CourseCorrectorRemoveOpts o o o ["name.."])))))))) $
                                                                                                execParserMaybe (globalInfo role) [courseSub, correctorSub, removeSub, "name.."]
  forM_ [correctorRole, studentRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [courseSub, correctorSub, removeSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [courseSub, correctorSub, removeSub, "name.."]

test_courseCorrectorList = do
  forM_ [adminRole, teacherRole, correctorRole] $ \role -> do
    assertEqual (Just(Global(Course(CourseOpts(CourseCorrector(CourseCorrectorOpts(CourseCorrectorList(CourseCorrectorListOpts o o o)))))))) $
                                                                                                execParserMaybe (globalInfo role) [courseSub, correctorSub, listSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [courseSub, correctorSub, listSub, "v"]
  assertEqual Nothing $ execParserMaybe (globalInfo studentRole) [courseSub, correctorSub, listSub]
  assertEqual Nothing $ execParserMaybe (globalInfo studentRole) [courseSub, correctorSub, listSub, "v"]

-- Group
test_group = forM_ allRoles $ \role -> assertEqual Nothing $ execParserMaybe (globalInfo role) [groupSub]
test_groupAdd = do
  forM_ [adminRole, teacherRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [groupSub, addSub]
    assertEqual (Just(Global(Group(GroupOpts(GroupAdd(GroupAddOpts o o o "name")))))) $ execParserMaybe (globalInfo role) [groupSub, addSub, "name"]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [groupSub, addSub, "name", "v"]
  forM_ [correctorRole, studentRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [groupSub, addSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [groupSub, addSub, "name"]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [groupSub, addSub, "name", "v"]

test_groupRemove = do
  forM_ [adminRole, teacherRole] $ \role -> do
    assertEqual (Just(Global(Group(GroupOpts(GroupRemove(GroupRemoveOpts o o o o)))))) $ execParserMaybe (globalInfo role) [groupSub, removeSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [groupSub, removeSub, "v"]
  forM_ [correctorRole, studentRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [groupSub, removeSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [groupSub, removeSub, "v"]

test_groupList = do
  forM_ [adminRole, teacherRole, correctorRole] $ \role -> do
    assertEqual (Just(Global(Group(GroupOpts(GroupList(GroupListOpts o o o)))))) $ execParserMaybe (globalInfo role) [groupSub, listSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [groupSub, listSub, "v"]
  assertEqual Nothing $ execParserMaybe (globalInfo studentRole) [groupSub, listSub]
  assertEqual Nothing $ execParserMaybe (globalInfo studentRole) [groupSub, listSub, "v"]


test_groupTeacher = forM_ allRoles $ \role -> assertEqual Nothing $ execParserMaybe (globalInfo role) [groupSub, teacherSub]
test_groupTeacherAdd = do
  forM_ [adminRole, teacherRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [groupSub, teacherSub, addSub]
    assertEqual (Just(Global(Group(GroupOpts(GroupTeacher(GroupTeacherOpts(GroupTeacherAdd(GroupTeacherAddOpts o o o o ["name.."])))))))) $
                                                                                                execParserMaybe (globalInfo role) [groupSub, teacherSub, addSub, "name.."]
  forM_ [correctorRole, studentRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [groupSub, teacherSub, addSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [groupSub, teacherSub, addSub, "name.."]

test_groupTeacherRemove = do
  forM_ [adminRole, teacherRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [groupSub, teacherSub, addSub]
    assertEqual (Just(Global(Group(GroupOpts(GroupTeacher(GroupTeacherOpts(GroupTeacherRemove(GroupTeacherRemoveOpts o o o o ["name.."])))))))) $
                                                                                                execParserMaybe (globalInfo role) [groupSub, teacherSub, removeSub, "name.."]
  forM_ [correctorRole, studentRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [groupSub, teacherSub, removeSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [groupSub, teacherSub, removeSub, "name.."]


test_groupTeacherList = do
  forM_ [adminRole, teacherRole, correctorRole] $ \role -> do
    assertEqual (Just(Global(Group(GroupOpts(GroupTeacher(GroupTeacherOpts(GroupTeacherList(GroupTeacherListOpts o o o o)))))))) $
                                                                                                execParserMaybe (globalInfo role) [groupSub, teacherSub, listSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [groupSub, teacherSub, listSub, "v"]
  assertEqual Nothing $ execParserMaybe (globalInfo studentRole) [groupSub, teacherSub, listSub]
  assertEqual Nothing $ execParserMaybe (globalInfo studentRole) [groupSub, teacherSub, listSub, "v"]


test_groupCorrector = forM_ allRoles $ \role -> assertEqual Nothing $ execParserMaybe (globalInfo role) [groupSub, teacherSub]
test_groupCorrectorAdd = do
  forM_ [adminRole, teacherRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [groupSub, correctorSub, addSub]
    assertEqual (Just(Global(Group(GroupOpts(GroupCorrector(GroupCorrectorOpts(GroupCorrectorAdd(GroupCorrectorAddOpts o o o o ["name.."])))))))) $
                                                                                                execParserMaybe (globalInfo role) [groupSub, correctorSub, addSub, "name.."]
  forM_ [correctorRole, studentRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [groupSub, correctorSub, addSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [groupSub, correctorSub, addSub, "name.."]

test_groupCorrectorRemove = do
  forM_ [adminRole, teacherRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [groupSub, teacherSub, addSub]
    assertEqual (Just(Global(Group(GroupOpts(GroupCorrector(GroupCorrectorOpts(GroupCorrectorRemove(GroupCorrectorRemoveOpts o o o o ["name.."])))))))) $
                                                                                                execParserMaybe (globalInfo role) [groupSub, correctorSub, removeSub, "name.."]
  forM_ [correctorRole, studentRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [groupSub, correctorSub, removeSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [groupSub, correctorSub, removeSub, "name.."]


test_groupCorrectorList = do
  forM_ [adminRole, teacherRole, correctorRole] $ \role -> do
    assertEqual (Just(Global(Group(GroupOpts(GroupCorrector(GroupCorrectorOpts(GroupCorrectorList(GroupCorrectorListOpts o o o o)))))))) $
                                                                                                execParserMaybe (globalInfo role) [groupSub, correctorSub, listSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [groupSub, correctorSub, listSub, "v"]
  assertEqual Nothing $ execParserMaybe (globalInfo studentRole) [groupSub, correctorSub, listSub]
  assertEqual Nothing $ execParserMaybe (globalInfo studentRole) [groupSub, correctorSub, listSub, "v"]



          