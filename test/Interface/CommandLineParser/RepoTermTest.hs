{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Interface.CommandLineParser.RepoTermTest where

import Test.Framework
import Interface.CommandLineParser.ParserTestUtils
import Options.Applicative
import Control.Monad (forM_)
import Security.SecurityManager (adminRole, teacherRole, correctorRole, studentRole)

import Interface.Lexicon
import Interface.CommandLineParser
import Interface.CommandLineParser.Repo
import Interface.CommandLineParser.Term

{-# ANN module "HLint: ignore Use camelCase" #-}

-- Repo
test_repo = forM_ allRoles $ \role -> assertEqual Nothing $ execParserMaybe (globalInfo role) ["repo"]

test_repoAdd = do
  forM_ allRoles $ \role -> assertEqual Nothing $ execParserMaybe (globalInfo role) ["repo", "add"]
  assertEqual (Just(Global(Repo(RepoOpts(RepoAdd(RepoAddOpts "name")))))) $ execParserMaybe (globalInfo adminRole) ["repo", "add", "name"]
  assertEqual Nothing $ execParserMaybe (globalInfo adminRole) ["repo", "add", "name", "v"]
  forM_ [teacherRole, correctorRole, studentRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) ["repo", "add", "name"]
    assertEqual Nothing $ execParserMaybe (globalInfo role) ["repo", "add", "name", "v"]
 
prop_repoAddSuccess n = let name = noLeadingHyphens n in testSuccess 1 name repoAddF [repoSub, addSub] noOpts [name]

test_repoRemove = do
  assertEqual (Just(Global(Repo(RepoOpts(RepoRemove(RepoRemoveOpts o)))))) $ execParserMaybe (globalInfo adminRole) ["repo", "remove"]
  assertEqual Nothing $ execParserMaybe (globalInfo adminRole) ["repo", "remove", "v"]
  forM_ [teacherRole, correctorRole, studentRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) ["repo", "remove"]
    assertEqual Nothing $ execParserMaybe (globalInfo role) ["repo", "remove", "v"]
 
prop_repoRemoveSuccess repoNN = validOpts [repoNN] ==> testSuccess 1 repoNN repoRemoveF [repoSub, removeSub] (repoOpts repoNN) noArgs

test_repoList = do
  forM_ [adminRole, teacherRole, correctorRole] $ \role -> do
    assertEqual (Just(Global(Repo(RepoOpts(RepoList RepoListOpts))))) $ execParserMaybe (globalInfo role) ["repo", "list"]
    assertEqual Nothing $ execParserMaybe (globalInfo role) ["repo", "list", "v"]
  assertEqual Nothing $ execParserMaybe (globalInfo studentRole) ["repo", "list"]
  assertEqual Nothing $ execParserMaybe (globalInfo studentRole) ["repo", "list", "v"]
 
prop_repoListSuccess = testSuccess 3 noArgsToGet repoListF [repoSub, listSub] noOpts noArgs

-- Term
test_term = forM_ allRoles $ \role -> assertEqual Nothing $ execParserMaybe (globalInfo role) ["term"]

test_termAdd = do
  assertEqual Nothing $ execParserMaybe (globalInfo adminRole) ["term", "add"]
  assertEqual Nothing $ execParserMaybe (globalInfo adminRole) ["term", "add", "name"]
  assertEqual Nothing $ execParserMaybe (globalInfo adminRole) ["term", "add", "name", "start"]
  assertEqual (Just(Global(Term(TermOpts(TermAdd(TermAddOpts o "name" "start" "end")))))) $ execParserMaybe (globalInfo adminRole) ["term", "add", "name", "start", "end"]
  assertEqual Nothing $ execParserMaybe (globalInfo adminRole) ["term", "add", "name", "start", "end", "v"]
  forM_ [teacherRole, correctorRole, studentRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) ["term", "add"]
    assertEqual Nothing $ execParserMaybe (globalInfo role) ["term", "add", "name"]
    assertEqual Nothing $ execParserMaybe (globalInfo role) ["term", "add", "name", "start"]
    assertEqual Nothing $ execParserMaybe (globalInfo role) ["term", "add", "name", "start", "end"]
    assertEqual Nothing $ execParserMaybe (globalInfo role) ["term", "add", "name", "start", "end", "v"]
 
prop_termAddSuccess repoNN n s e = let args = validArgs [n, s, e] in length args == 3 ==>
  validOpts [repoNN] ==> let [name, start, end] = args in
   testSuccess 1 (repoNN, name, start, end) termAddF [termSub, addSub] (repoOpts repoNN) args

test_termRemove = do
  assertEqual (Just(Global(Term(TermOpts(TermRemove(TermRemoveOpts o o)))))) $ execParserMaybe (globalInfo adminRole) ["term", "remove"]
  assertEqual Nothing $ execParserMaybe (globalInfo adminRole) ["term", "remove", "v"]
  forM_ [teacherRole, correctorRole, studentRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) ["term", "remove"]
    assertEqual Nothing $ execParserMaybe (globalInfo role) ["term", "remove", "v"]
 
prop_termRemoveSuccess repoNN termNN =
 validOpts [repoNN, termNN] ==>
  testSuccess 1 (repoNN,termNN) termRemoveF [termSub, removeSub] (termOpts repoNN termNN) noArgs

test_termList = do
  forM_ [adminRole, teacherRole, correctorRole] $ \role -> do
    assertEqual (Just(Global(Term(TermOpts(TermList(TermListOpts o)))))) $ execParserMaybe (globalInfo role) ["term", "list"]
    assertEqual Nothing $ execParserMaybe (globalInfo role) ["term", "list", "v"]
  assertEqual Nothing $ execParserMaybe (globalInfo studentRole) ["term", "list"]
  assertEqual Nothing $ execParserMaybe (globalInfo studentRole) ["term", "list", "v"]

prop_termListSuccess repoNN =
 validOpts [repoNN] ==>
  testSuccess 3 repoNN termListF [termSub, listSub] (repoOpts repoNN) noArgs

test_termDate = forM_ allRoles $ \role -> assertEqual Nothing $ execParserMaybe (globalInfo role) ["term", "date"]

test_termDateSet = do
  assertEqual (Just(Global(Term(TermOpts(TermDate(TermDateOpts(TermDateSet(TermDateSetOpts o o o o)))))))) $ execParserMaybe (globalInfo adminRole) ["term", "date", "set"]
  assertEqual Nothing $ execParserMaybe (globalInfo adminRole) ["term", "date", "set", "v"]
  forM_ [teacherRole, correctorRole, studentRole] $ \role -> do
    assertEqual Nothing $ execParserMaybe (globalInfo role) ["term", "date", "set"]
    assertEqual Nothing $ execParserMaybe (globalInfo role) ["term", "date", "set", "v"]

prop_termDateSetSuccess repoNN termNN start end =
 validOpts [repoNN, termNN, start, end] ==>
  testSuccess 1 (repoNN, termNN, start, end) termDateSetF [termSub, dateSub, setSub] (termDateOpts repoNN termNN start end) noArgs

test_termDateList = do
  forM_ [adminRole, teacherRole, correctorRole] $ \role -> do
    assertEqual (Just(Global(Term(TermOpts(TermDate(TermDateOpts(TermDateList(TermDateListOpts o o)))))))) $ execParserMaybe (globalInfo role) ["term", "date", "list"]
    assertEqual Nothing $ execParserMaybe (globalInfo role) ["term", "date", "list", "v"]
  assertEqual Nothing $ execParserMaybe (globalInfo studentRole) ["term", "date", "list"]
  assertEqual Nothing $ execParserMaybe (globalInfo studentRole) ["term", "date", "list", "v"]

prop_termDateListSuccess repoNN termNN =
 validOpts [repoNN, termNN] ==>
  testSuccess 3 (repoNN, termNN) termDateListF [termSub, dateSub, listSub] (termOpts repoNN termNN) noArgs

repoAddF (Global(Repo(RepoOpts(RepoAdd(RepoAddOpts a))))) = a
repoRemoveF (Global(Repo(RepoOpts(RepoRemove(RepoRemoveOpts a))))) = a
repoListF (Global(Repo(RepoOpts(RepoList RepoListOpts)))) = noArgsToGet
termAddF (Global(Term(TermOpts(TermAdd(TermAddOpts a b c d))))) = (a,b,c,d)
termRemoveF (Global(Term(TermOpts(TermRemove(TermRemoveOpts a b))))) = (a,b)
termListF (Global(Term(TermOpts(TermList(TermListOpts a))))) = a
termDateSetF (Global(Term(TermOpts(TermDate(TermDateOpts(TermDateSet(TermDateSetOpts a b c d))))))) = (a,b,c,d)
termDateListF (Global(Term(TermOpts(TermDate(TermDateOpts(TermDateList(TermDateListOpts a b))))))) = (a,b)