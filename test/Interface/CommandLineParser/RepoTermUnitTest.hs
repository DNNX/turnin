{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Interface.CommandLineParser.RepoTermUnitTest where

import Test.Framework
import Interface.CommandLineParser.ParserTestUtils
import Options.Applicative
import Control.Monad
import Security.SecurityManager

import Interface.Lexicon
import Interface.CommandLineParser
import Interface.CommandLineParser.Repo
import Interface.CommandLineParser.Term

{-# ANN module "HLint: ignore Use camelCase" #-}

-- Repo
test_repo = forM_ allRoles $ \role -> assertEqual Nothing $ execParserMaybe (globalInfo role) [repoSub]

test_repoAdd = do
  forM_ allRoles $ \role -> assertEqual Nothing $ execParserMaybe (globalInfo role) [repoSub, addSub]
  assertEqual (Just(Global(Repo(RepoOpts(RepoAdd(RepoAddOpts "name")))))) $ execParserMaybe (globalInfo adminRole) [repoSub, addSub, "name"]
  assertEqual Nothing $ execParserMaybe (globalInfo adminRole) [repoSub, addSub, "name", "v"]
  forM_ [teacherRole, correctorRole, studentRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [repoSub, addSub, "name"]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [repoSub, addSub, "name", "v"]
 
test_repoRemove = do
  assertEqual (Just(Global(Repo(RepoOpts(RepoRemove(RepoRemoveOpts o)))))) $ execParserMaybe (globalInfo adminRole) [repoSub, removeSub]
  assertEqual Nothing $ execParserMaybe (globalInfo adminRole) [repoSub, removeSub, "v"]
  forM_ [teacherRole, correctorRole, studentRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [repoSub, removeSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [repoSub, removeSub, "v"]
 
test_repoList = do
  forM_ [adminRole, teacherRole, correctorRole] $ \role -> do
    assertEqual (Just(Global(Repo(RepoOpts(RepoList RepoListOpts))))) $ execParserMaybe (globalInfo role) [repoSub, listSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [repoSub, listSub, "v"]
  assertEqual Nothing $ execParserMaybe (globalInfo studentRole) [repoSub, listSub]
  assertEqual Nothing $ execParserMaybe (globalInfo studentRole) [repoSub, listSub, "v"]
 
-- Term
test_term = forM_ allRoles $ \role -> assertEqual Nothing $ execParserMaybe (globalInfo role) [termSub]

test_termAdd = do
  assertEqual Nothing $ execParserMaybe (globalInfo adminRole) [termSub, addSub]
  assertEqual Nothing $ execParserMaybe (globalInfo adminRole) [termSub, addSub, "name"]
  assertEqual Nothing $ execParserMaybe (globalInfo adminRole) [termSub, addSub, "name", "start"]
  assertEqual (Just(Global(Term(TermOpts(TermAdd(TermAddOpts o "name" "start" "end")))))) $ execParserMaybe (globalInfo adminRole) [termSub, addSub, "name", "start", "end"]
  assertEqual Nothing $ execParserMaybe (globalInfo adminRole) [termSub, addSub, "name", "start", "end", "v"]
  forM_ [teacherRole, correctorRole, studentRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [termSub, addSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [termSub, addSub, "name"]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [termSub, addSub, "name", "start"]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [termSub, addSub, "name", "start", "end"]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [termSub, addSub, "name", "start", "end", "v"]
 
test_termRemove = do
  assertEqual (Just(Global(Term(TermOpts(TermRemove(TermRemoveOpts o o)))))) $ execParserMaybe (globalInfo adminRole) [termSub, removeSub]
  assertEqual Nothing $ execParserMaybe (globalInfo adminRole) [termSub, removeSub, "v"]
  forM_ [teacherRole, correctorRole, studentRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [termSub, removeSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [termSub, removeSub, "v"]
 
test_termList = do
  forM_ [adminRole, teacherRole, correctorRole] $ \role -> do
    assertEqual (Just(Global(Term(TermOpts(TermList(TermListOpts o)))))) $ execParserMaybe (globalInfo role) [termSub, listSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [termSub, listSub, "v"]
  assertEqual Nothing $ execParserMaybe (globalInfo studentRole) [termSub, listSub]
  assertEqual Nothing $ execParserMaybe (globalInfo studentRole) [termSub, listSub, "v"]

test_termDate = forM_ allRoles $ \role -> assertEqual Nothing $ execParserMaybe (globalInfo role) [termSub, dateSub]

test_termDateSet = do
  assertEqual (Just(Global(Term(TermOpts(TermDate(TermDateOpts(TermDateSet(TermDateSetOpts o o o o)))))))) $ execParserMaybe (globalInfo adminRole) [termSub, dateSub, setSub]
  assertEqual Nothing $ execParserMaybe (globalInfo adminRole) [termSub, dateSub, setSub, "v"]
  forM_ [teacherRole, correctorRole, studentRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) [termSub, dateSub, setSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [termSub, dateSub, setSub, "v"]

test_termDateList = do
  forM_ [adminRole, teacherRole, correctorRole] $ \role -> do
    assertEqual (Just(Global(Term(TermOpts(TermDate(TermDateOpts(TermDateList(TermDateListOpts o o)))))))) $ execParserMaybe (globalInfo role) [termSub, dateSub, listSub]
    assertEqual Nothing $ execParserMaybe (globalInfo role) [termSub, dateSub, listSub, "v"]
  assertEqual Nothing $ execParserMaybe (globalInfo studentRole) [termSub, dateSub, listSub]
  assertEqual Nothing $ execParserMaybe (globalInfo studentRole) [termSub, dateSub, listSub, "v"]

repoAddF (Global(Repo(RepoOpts(RepoAdd(RepoAddOpts a))))) = a
repoRemoveF (Global(Repo(RepoOpts(RepoRemove(RepoRemoveOpts a))))) = a
repoListF (Global(Repo(RepoOpts(RepoList RepoListOpts)))) = noArgsToGet
termAddF (Global(Term(TermOpts(TermAdd(TermAddOpts a b c d))))) = (a,b,c,d)
termRemoveF (Global(Term(TermOpts(TermRemove(TermRemoveOpts a b))))) = (a,b)
termListF (Global(Term(TermOpts(TermList(TermListOpts a))))) = a
termDateSetF (Global(Term(TermOpts(TermDate(TermDateOpts(TermDateSet(TermDateSetOpts a b c d))))))) = (a,b,c,d)
termDateListF (Global(Term(TermOpts(TermDate(TermDateOpts(TermDateList(TermDateListOpts a b))))))) = (a,b)