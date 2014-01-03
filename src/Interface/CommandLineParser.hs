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

data Global = Global Cmd     deriving (Show, Eq)
data Cmd = Config  ConfigOpts 
         | Repo    RepoOpts
         | Term    TermOpts   
         | Course  CourseOpts
         | Group   GroupOpts  
         | Project ProjectOpts deriving (Show, Eq)
 
globalInfo = info (myHelper <*> global) (progDesc globalDesc <> header globalHeader)
 
global = Global <$> subparser (
 command configSub  (Config <$> configInfo) <>
 command repoSub    (Repo   <$> repoInfo) <>
 command termSub    (Term   <$> termInfo) <>
 command courseSub  (Course <$> courseInfo) <>
 command groupSub   (Group  <$> groupInfo) <>
 command projectSub (Project <$> projectInfo))
  