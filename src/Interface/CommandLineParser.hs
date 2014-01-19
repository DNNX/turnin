module Interface.CommandLineParser where

import Options.Applicative
import Interface.Lexicon
import Interface.CommandLineParser.Utils
import Interface.CommandLineParser.Config
import Interface.CommandLineParser.Repo
import Interface.CommandLineParser.Term
import Interface.CommandLineParser.Course
import Interface.CommandLineParser.Group
import Interface.CommandLineParser.Project
import Interface.CommandLineParser.SubmitWorktrain

import Security.SecurityManager

data Global = Global Cmd     deriving (Show, Eq)
data Cmd = Config    ConfigOpts
         | Repo      RepoOpts
         | Term      TermOpts 
         | Course    CourseOpts 
         | Group     GroupOpts
         | Project   ProjectOpts 
         | Submit    SubmitOpts
         | Inspect   InspectOpts
         | Extract   ExtractOpts
         | Worktrain WorktrainOpts deriving (Show, Eq)

globalInfo role = info (myHelper <*> global role) (progDesc globalDesc <> header globalHeader)
 
global role = Global <$> subparser (
 hasConfigRights role    (command configSub    (Config <$> configInfo role)) <>
 hasRepoRights role      (command repoSub      (Repo   <$> repoInfo role)) <> 
 hasTermRights role      (command termSub      (Term   <$> termInfo role)) <> 
 hasCourseRights role    (command courseSub    (Course <$> courseInfo role)) <>
 hasGroupRights role     (command groupSub     (Group  <$> groupInfo role)) <>  
 hasProjectRights role   (command projectSub   (Project <$> projectInfo role)) <>
 hasSubmitRights role    (command submitSub    (Submit <$> submitInfo)) <>
 hasInspectRights role   (command inspectSub   (Inspect <$> inspectInfo)) <>
 hasExtractRights role   (command extractSub   (Extract <$> extractInfo)) <> 
 hasWorktrainRights role (command worktrainSub (Worktrain <$> worktrainInfo)))
 