{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Interface.CommandLineParser.CourseGroupUnitTest where

import Test.Framework
import Interface.CommandLineParser.ParserTestUtils

import Interface.Lexicon
import Interface.CommandLineParser
import Interface.CommandLineParser.Course
import Interface.CommandLineParser.Group

{-# ANN module "HLint: ignore Use camelCase" #-}
{-
-- Course
prop_courseAddSuccess repoNN termNN n = let name = noLeadingHyphens n in
 validOpts [repoNN, termNN] ==>
  testSuccess 2 (repoNN, termNN, name) courseAddF [courseSub, addSub] (termOpts repoNN termNN) [name]

prop_courseRemoveSuccess repoNN termNN courseNN =
 validOpts [repoNN, termNN, courseNN] ==>
  testSuccess 2 (repoNN, termNN, courseNN) courseRemoveF [courseSub, removeSub] (courseOpts repoNN termNN courseNN) noArgs

prop_courseListSuccess repoNN termNN =
 validOpts [repoNN, termNN] ==>
  testSuccess 3 (repoNN, termNN) courseListF [courseSub, listSub] (termOpts repoNN termNN) noArgs

prop_courseTeacherAddSuccess repoNN termNN courseNN ns = let names = validArgs ns in names /= [] ==>
 validOpts [repoNN, termNN, courseNN] ==>
  testSuccess 2 (repoNN, termNN, courseNN, names) courseTeacherAddF [courseSub, teacherSub, addSub]
   (courseOpts repoNN termNN courseNN) names

prop_courseTeacherRemoveSuccess repoNN termNN courseNN ns = let names = validArgs ns in names /= [] ==>
 validOpts [repoNN, termNN, courseNN] ==>
  testSuccess 2 (repoNN, termNN, courseNN, names) courseTeacherRemoveF [courseSub, teacherSub, removeSub]
   (courseOpts repoNN termNN courseNN) names

prop_courseTeacherListSuccess repoNN termNN courseNN =
 validOpts [repoNN, termNN, courseNN] ==>
  testSuccess 3 (repoNN, termNN, courseNN) courseTeacherListF [courseSub, teacherSub, listSub]
   (courseOpts repoNN termNN courseNN) noArgs

prop_courseCorrectorAddSuccess repoNN termNN courseNN ns = let names = validArgs ns in names /= [] ==>
 validOpts [repoNN, termNN, courseNN] ==>
  testSuccess 2 (repoNN, termNN, courseNN, names) courseCorrectorAddF [courseSub, correctorSub, addSub]
   (courseOpts repoNN termNN courseNN) names

prop_courseCorrectorRemoveSuccess repoNN termNN courseNN ns = let names = validArgs ns in names /= [] ==>
 validOpts [repoNN, termNN, courseNN] ==>
  testSuccess 2 (repoNN, termNN, courseNN, names) courseCorrectorRemoveF [courseSub, correctorSub, removeSub]
   (courseOpts repoNN termNN courseNN) names

prop_courseCorrectorListSuccess repoNN termNN courseNN =
 validOpts [repoNN, termNN, courseNN] ==>
  testSuccess 3 (repoNN, termNN, courseNN) courseCorrectorListF [courseSub, correctorSub, listSub]
   (courseOpts repoNN termNN courseNN) noArgs

-- Group
prop_groupAddSuccess repoNN termNN courseNN n = let name = noLeadingHyphens n in
 validOpts [repoNN, termNN, courseNN] ==>
  testSuccess 2 (repoNN, termNN, courseNN, name) groupAddF [groupSub, addSub]
   (courseOpts repoNN termNN courseNN) [name]

prop_groupRemoveSuccess repoNN termNN courseNN groupNN =
 validOpts [repoNN, termNN, courseNN, groupNN] ==>
  testSuccess 2 (repoNN, termNN, courseNN, groupNN) groupRemoveF [groupSub, removeSub]
   (groupOpts repoNN termNN courseNN groupNN) noArgs

prop_groupListSuccess repoNN termNN courseNN =
 validOpts [repoNN, termNN, courseNN] ==>
  testSuccess 3 (repoNN, termNN, courseNN) groupListF [groupSub, listSub] (courseOpts repoNN termNN courseNN) noArgs

prop_groupTeacherAddSuccess repoNN termNN courseNN groupNN ns = let names = validArgs ns in names /= [] ==>
 validOpts [repoNN, termNN, courseNN, groupNN] ==>
  testSuccess 2 (repoNN, termNN, courseNN, groupNN, names) groupTeacherAddF
   [groupSub, teacherSub, addSub] (groupOpts repoNN termNN courseNN groupNN) names

prop_groupTeacherRemoveSuccess repoNN termNN courseNN groupNN ns = let names = validArgs ns in names /= [] ==>
 validOpts [repoNN, termNN, courseNN, groupNN] ==>
  testSuccess 2 (repoNN, termNN, courseNN, groupNN, names) groupTeacherRemoveF [groupSub, teacherSub, removeSub]
   (groupOpts repoNN termNN courseNN groupNN) names

prop_groupTeacherListSuccess repoNN termNN courseNN groupNN =
 validOpts [repoNN, termNN, courseNN, groupNN] ==>
  testSuccess 3 (repoNN, termNN, courseNN, groupNN) groupTeacherListF [groupSub, teacherSub, listSub]
   (groupOpts repoNN termNN courseNN groupNN) noArgs

prop_groupCorrectorAddSuccess repoNN termNN courseNN groupNN ns = let names = validArgs ns in names /= [] ==>
 validOpts [repoNN, termNN, courseNN, groupNN] ==>
  testSuccess 2 (repoNN, termNN, courseNN, groupNN, names) groupCorrectorAddF [groupSub, correctorSub, addSub]
   (groupOpts repoNN termNN courseNN groupNN) names

prop_groupCorrectorRemoveSuccess repoNN termNN courseNN groupNN ns = let names = validArgs ns in names /= [] ==>
 validOpts [repoNN, termNN, courseNN, groupNN] ==>
  testSuccess 2 (repoNN, termNN, courseNN, groupNN, names) groupCorrectorRemoveF [groupSub, correctorSub, removeSub]
   (groupOpts repoNN termNN courseNN groupNN) names

prop_groupCorrectorListSuccess repoNN termNN courseNN groupNN =
 validOpts [repoNN, termNN, courseNN, groupNN] ==>
  testSuccess 3 (repoNN, termNN, courseNN, groupNN) groupCorrectorListF [groupSub, correctorSub, listSub]
   (groupOpts repoNN termNN courseNN groupNN) noArgs

courseAddF (Global(Course(CourseOpts(CourseAdd(CourseAddOpts a b c))))) = (a,b,c)
courseRemoveF (Global(Course(CourseOpts(CourseRemove(CourseRemoveOpts a b c))))) = (a,b,c)
courseListF (Global(Course(CourseOpts(CourseList(CourseListOpts a b))))) = (a,b)
courseTeacherAddF (Global(Course(CourseOpts(CourseTeacher(CourseTeacherOpts(CourseTeacherAdd(CourseTeacherAddOpts a b c d))))))) = (a,b,c,d)
courseTeacherRemoveF (Global(Course(CourseOpts(CourseTeacher(CourseTeacherOpts(CourseTeacherRemove(CourseTeacherRemoveOpts a b c d))))))) = (a,b,c,d)
courseTeacherListF (Global(Course(CourseOpts(CourseTeacher(CourseTeacherOpts(CourseTeacherList(CourseTeacherListOpts a b c))))))) = (a,b,c)
courseCorrectorAddF (Global(Course(CourseOpts(CourseCorrector(CourseCorrectorOpts(CourseCorrectorAdd(CourseCorrectorAddOpts a b c d))))))) = (a,b,c,d)
courseCorrectorRemoveF (Global(Course(CourseOpts(CourseCorrector(CourseCorrectorOpts(CourseCorrectorRemove(CourseCorrectorRemoveOpts a b c d))))))) = (a,b,c,d)
courseCorrectorListF (Global(Course(CourseOpts(CourseCorrector(CourseCorrectorOpts(CourseCorrectorList(CourseCorrectorListOpts a b c))))))) = (a,b,c)
groupAddF (Global(Group(GroupOpts(GroupAdd(GroupAddOpts a b c d))))) = (a,b,c,d)
groupRemoveF (Global(Group(GroupOpts(GroupRemove(GroupRemoveOpts a b c d))))) = (a,b,c,d)
groupListF (Global(Group(GroupOpts(GroupList(GroupListOpts a b c))))) = (a,b,c)
groupTeacherAddF (Global(Group(GroupOpts(GroupTeacher(GroupTeacherOpts(GroupTeacherAdd(GroupTeacherAddOpts a b c d e))))))) = (a,b,c,d, e)
groupTeacherRemoveF (Global(Group(GroupOpts(GroupTeacher(GroupTeacherOpts(GroupTeacherRemove(GroupTeacherRemoveOpts a b c d e))))))) = (a,b,c,d,e)
groupTeacherListF (Global(Group(GroupOpts(GroupTeacher(GroupTeacherOpts(GroupTeacherList(GroupTeacherListOpts a b c d))))))) = (a,b,c,d)
groupCorrectorAddF (Global(Group(GroupOpts(GroupCorrector(GroupCorrectorOpts(GroupCorrectorAdd(GroupCorrectorAddOpts a b c d e))))))) = (a,b,c,d,e)
groupCorrectorRemoveF (Global(Group(GroupOpts(GroupCorrector(GroupCorrectorOpts(GroupCorrectorRemove(GroupCorrectorRemoveOpts a b c d e))))))) = (a,b,c,d, e)
groupCorrectorListF (Global(Group(GroupOpts(GroupCorrector(GroupCorrectorOpts(GroupCorrectorList(GroupCorrectorListOpts a b c d))))))) = (a,b,c,d)
-}