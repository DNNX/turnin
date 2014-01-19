{-# LANGUAGE NoMonomorphismRestriction #-}
module Security.SecurityManager
( Role()
, hasConfigRights
, hasConfigWriteRights
, hasConfigReadRights
, hasRepoRights
, hasRepoWriteRights
, hasRepoReadRights
, hasTermRights
, hasTermWriteRights
, hasTermReadRights
, hasCourseRights
, hasCourseWriteRights
, hasCourseReadRights
, hasGroupRights
, hasGroupWriteRights
, hasGroupReadRights
, hasProjectRights
, hasProjectWriteRights
, hasProjectReadRights
, hasSubmitRights
, hasInspectRights
, hasExtractRights
, hasWorktrainRights
, adminRole
, teacherRole
, correctorRole
, studentRole
) where

import Data.Monoid

data Role = StudentRole | CorrectorRole | TeacherRole | AdminRole deriving (Show, Eq)

hasConfigRights      = (`mustBe` adminTeacher)
hasConfigWriteRights = (`mustBe` admin)
hasConfigReadRights  = hasConfigRights

hasRepoRights      = (`mustBe` adminTeacherCorr)
hasRepoWriteRights = (`mustBe` admin)
hasRepoReadRights  = hasRepoRights

hasTermRights      = (`mustBe` adminTeacherCorr)
hasTermWriteRights = (`mustBe` admin)
hasTermReadRights  = hasTermRights

hasCourseRights      = (`mustBe` adminTeacherCorr)
hasCourseWriteRights = (`mustBe` adminTeacher)
hasCourseReadRights  = hasCourseRights

hasGroupRights      = (`mustBe` adminTeacherCorr)
hasGroupWriteRights = (`mustBe` adminTeacher)
hasGroupReadRights  = hasGroupRights

hasProjectRights      = (`mustBe` adminTeacherCorr)
hasProjectWriteRights = (`mustBe` adminTeacher)
hasProjectReadRights  = hasProjectRights

hasSubmitRights    = (`mustBe` anyone)
hasInspectRights   = (`mustBe` anyone)
hasExtractRights   = (`mustBe` anyone)
hasWorktrainRights = (`mustBe` adminTeacherCorr)

admin            = [adminRole]
adminTeacher     = teacherRole:admin
adminTeacherCorr = correctorRole:adminTeacher
anyone           = studentRole:adminTeacherCorr

mustBe v vs x = if v `elem` vs then x else mempty

adminRole     = AdminRole
teacherRole   = TeacherRole
correctorRole = CorrectorRole
studentRole   = StudentRole
