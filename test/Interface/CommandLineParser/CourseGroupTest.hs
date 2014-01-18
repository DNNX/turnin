{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Interface.CommandLineParser.CourseGroupTest where

import Test.Framework
import Interface.CommandLineParser.ParserTestUtils

import Interface.Lexicon
import Interface.CommandLineParser
import Interface.CommandLineParser.Course
import Interface.CommandLineParser.Group

{-# ANN module "HLint: ignore Use camelCase" #-}

-- Course
prop_courseAddSuccess repoNN termNN n = let name = noLeadingHyphens n in
 validOpts [repoNN, termNN] ==>
  testSuccess (repoNN, termNN, name) x [courseSub, addSub] (termOpts repoNN termNN) [name]
   where x (Global(
            Course(CourseOpts(
             CourseAdd(CourseAddOpts a b c))))) = (a,b,c)

prop_courseRemoveSuccess repoNN termNN courseNN =
 validOpts [repoNN, termNN, courseNN] ==>
  testSuccess (repoNN, termNN, courseNN) x [courseSub, removeSub] (courseOpts repoNN termNN courseNN) noArgs
   where x (Global(
            Course(CourseOpts(
             CourseRemove(CourseRemoveOpts a b c))))) = (a,b,c)

prop_courseListSuccess repoNN termNN =
 validOpts [repoNN, termNN] ==>
  testSuccess (repoNN, termNN) x [courseSub, listSub] (termOpts repoNN termNN) noArgs
   where x (Global(
            Course(CourseOpts(
             CourseList(CourseListOpts a b))))) = (a,b)

prop_courseTeacherAddSuccess repoNN termNN courseNN ns = let names = validArgs ns in names /= [] ==>
 validOpts [repoNN, termNN, courseNN] ==>
  testSuccess (repoNN, termNN, courseNN, names) x [courseSub, teacherSub, addSub]
   (courseOpts repoNN termNN courseNN) names
       where x (Global(
                Course(CourseOpts(
                 CourseTeacher(CourseTeacherOpts(
                  CourseTeacherAdd(CourseTeacherAddOpts a b c d))))))) = (a,b,c,d)

prop_courseTeacherRemoveSuccess repoNN termNN courseNN ns = let names = validArgs ns in names /= [] ==>
 validOpts [repoNN, termNN, courseNN] ==>
  testSuccess (repoNN, termNN, courseNN, names) x [courseSub, teacherSub, removeSub]
   (courseOpts repoNN termNN courseNN) names
       where x (Global(
                Course(CourseOpts(
                 CourseTeacher(CourseTeacherOpts(
                  CourseTeacherRemove(CourseTeacherRemoveOpts a b c d))))))) = (a,b,c,d)

prop_courseTeacherListSuccess repoNN termNN courseNN =
 validOpts [repoNN, termNN, courseNN] ==>
  testSuccess (repoNN, termNN, courseNN) x [courseSub, teacherSub, listSub]
   (courseOpts repoNN termNN courseNN) noArgs
       where x (Global(
                Course(CourseOpts(
                 CourseTeacher(CourseTeacherOpts(
                  CourseTeacherList(CourseTeacherListOpts a b c))))))) = (a,b,c)


prop_courseCorrectorAddSuccess repoNN termNN courseNN ns = let names = validArgs ns in names /= [] ==>
 validOpts [repoNN, termNN, courseNN] ==>
  testSuccess (repoNN, termNN, courseNN, names) x [courseSub, correctorSub, addSub]
   (courseOpts repoNN termNN courseNN) names
       where x (Global(
                Course(CourseOpts(
                 CourseCorrector(CourseCorrectorOpts(
                  CourseCorrectorAdd(CourseCorrectorAddOpts a b c d))))))) = (a,b,c,d)

prop_courseCorrectorRemoveSuccess repoNN termNN courseNN ns = let names = validArgs ns in names /= [] ==>
 validOpts [repoNN, termNN, courseNN] ==>
  testSuccess (repoNN, termNN, courseNN, names) x [courseSub, correctorSub, removeSub]
   (courseOpts repoNN termNN courseNN) names
       where x (Global(
                Course(CourseOpts(
                 CourseCorrector(CourseCorrectorOpts(
                  CourseCorrectorRemove(CourseCorrectorRemoveOpts a b c d))))))) = (a,b,c,d)

prop_courseCorrectorListSuccess repoNN termNN courseNN =
 validOpts [repoNN, termNN, courseNN] ==>
  testSuccess (repoNN, termNN, courseNN) x [courseSub, correctorSub, listSub]
   (courseOpts repoNN termNN courseNN) noArgs
       where x (Global(
                Course(CourseOpts(
                 CourseCorrector(CourseCorrectorOpts(
                  CourseCorrectorList(CourseCorrectorListOpts a b c))))))) = (a,b,c)

-- Group
prop_groupAddSuccess repoNN termNN courseNN n = let name = noLeadingHyphens n in
 validOpts [repoNN, termNN, courseNN] ==>
  testSuccess (repoNN, termNN, courseNN, name) x [groupSub, addSub]
   (courseOpts repoNN termNN courseNN) [name]
    where x (Global(
             Group(GroupOpts(
              GroupAdd(GroupAddOpts a b c d))))) = (a,b,c,d)

prop_groupRemoveSuccess repoNN termNN courseNN groupNN =
 validOpts [repoNN, termNN, courseNN, groupNN] ==>
  testSuccess (repoNN, termNN, courseNN, groupNN) x [groupSub, removeSub]
   (groupOpts repoNN termNN courseNN groupNN) noArgs
    where x (Global(
             Group(GroupOpts(
              GroupRemove(GroupRemoveOpts a b c d))))) = (a,b,c,d)

prop_groupListSuccess repoNN termNN courseNN =
 validOpts [repoNN, termNN, courseNN] ==>
  testSuccess (repoNN, termNN, courseNN) x [groupSub, listSub] (courseOpts repoNN termNN courseNN) noArgs
   where x (Global(
            Group(GroupOpts(
             GroupList(GroupListOpts a b c))))) = (a,b,c)

prop_groupTeacherAddSuccess repoNN termNN courseNN groupNN ns = let names = validArgs ns in names /= [] ==>
 validOpts [repoNN, termNN, courseNN, groupNN] ==>
  testSuccess (repoNN, termNN, courseNN, groupNN, names) x
   [groupSub, teacherSub, addSub] (groupOpts repoNN termNN courseNN groupNN) names
       where x (Global(
                Group(GroupOpts(
                 GroupTeacher(GroupTeacherOpts(
                  GroupTeacherAdd(GroupTeacherAddOpts a b c d e))))))) = (a,b,c,d, e)

prop_groupTeacherRemoveSuccess repoNN termNN courseNN groupNN ns = let names = validArgs ns in names /= [] ==>
 validOpts [repoNN, termNN, courseNN, groupNN] ==>
  testSuccess (repoNN, termNN, courseNN, groupNN, names) x [groupSub, teacherSub, removeSub]
   (groupOpts repoNN termNN courseNN groupNN) names
       where x (Global(
                Group(GroupOpts(
                 GroupTeacher(GroupTeacherOpts(
                  GroupTeacherRemove(GroupTeacherRemoveOpts a b c d e))))))) = (a,b,c,d,e)

prop_groupTeacherListSuccess repoNN termNN courseNN groupNN =
 validOpts [repoNN, termNN, courseNN, groupNN] ==>
  testSuccess (repoNN, termNN, courseNN, groupNN) x [groupSub, teacherSub, listSub]
   (groupOpts repoNN termNN courseNN groupNN) noArgs
    where x (Global(
             Group(GroupOpts(
              GroupTeacher(GroupTeacherOpts(
               GroupTeacherList(GroupTeacherListOpts a b c d))))))) = (a,b,c,d)


prop_groupCorrectorAddSuccess repoNN termNN courseNN groupNN ns = let names = validArgs ns in names /= [] ==>
 validOpts [repoNN, termNN, courseNN, groupNN] ==>
  testSuccess (repoNN, termNN, courseNN, groupNN, names) x [groupSub, correctorSub, addSub]
   (groupOpts repoNN termNN courseNN groupNN) names
       where x (Global(
                Group(GroupOpts(
                 GroupCorrector(GroupCorrectorOpts(
                  GroupCorrectorAdd(GroupCorrectorAddOpts a b c d e))))))) = (a,b,c,d,e)

prop_groupCorrectorRemoveSuccess repoNN termNN courseNN groupNN ns = let names = validArgs ns in names /= [] ==>
 validOpts [repoNN, termNN, courseNN, groupNN] ==>
  testSuccess (repoNN, termNN, courseNN, groupNN, names) x [groupSub, correctorSub, removeSub]
   (groupOpts repoNN termNN courseNN groupNN) names
       where x (Global(
                Group(GroupOpts(
                 GroupCorrector(GroupCorrectorOpts(
                  GroupCorrectorRemove(GroupCorrectorRemoveOpts a b c d e))))))) = (a,b,c,d, e)

prop_groupCorrectorListSuccess repoNN termNN courseNN groupNN =
 validOpts [repoNN, termNN, courseNN, groupNN] ==>
  testSuccess (repoNN, termNN, courseNN, groupNN) x [groupSub, correctorSub, listSub]
   (groupOpts repoNN termNN courseNN groupNN) noArgs
       where x (Global(
                Group(GroupOpts(
                 GroupCorrector(GroupCorrectorOpts(
                  GroupCorrectorList(GroupCorrectorListOpts a b c d))))))) = (a,b,c,d)
