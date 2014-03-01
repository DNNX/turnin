{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Interface.CommandLineParser.RepoTermPropTest where

import Test.Framework
import Interface.CommandLineParser.ParserTestUtils

import Interface.Lexicon
import Interface.CommandLineParser
import Interface.CommandLineParser.Repo
import Interface.CommandLineParser.Term

{-# ANN module "HLint: ignore Use camelCase" #-}

-- Repo
prop_repoAddSuccess n = let name = noLeadingHyphens n in testSuccess 1 name repoAddF [repoSub, addSub] noOpts [name]
prop_repoRemoveSuccess repoNN = validOpts [repoNN] ==> testSuccess 1 repoNN repoRemoveF [repoSub, removeSub] (repoOpts repoNN) noArgs
prop_repoListSuccess = testSuccess 3 noArgsToGet repoListF [repoSub, listSub] noOpts noArgs

-- Term
prop_termAddSuccess repoNN n s e = let args = validArgs [n, s, e] in length args == 3 ==>
  validOpts [repoNN] ==> let [name, start, end] = args in
   testSuccess 1 (repoNN, name, start, end) termAddF [termSub, addSub] (repoOpts repoNN) args

prop_termRemoveSuccess repoNN termNN =
 validOpts [repoNN, termNN] ==>
  testSuccess 1 (repoNN,termNN) termRemoveF [termSub, removeSub] (termOpts repoNN termNN) noArgs

prop_termListSuccess repoNN =
 validOpts [repoNN] ==>
  testSuccess 3 repoNN termListF [termSub, listSub] (repoOpts repoNN) noArgs

prop_termDateSetSuccess repoNN termNN start end =
 validOpts [repoNN, termNN, start, end] ==>
  testSuccess 1 (repoNN, termNN, start, end) termDateSetF [termSub, dateSub, setSub] (termDateOpts repoNN termNN start end) noArgs

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