{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Interface.CommandLineParser.RepoTermTest where
import Test.Framework
import Interface.CommandLineParser.ParserTestUtils

import Interface.Lexicon
import Interface.CommandLineParser
import Interface.CommandLineParser.Repo
import Interface.CommandLineParser.Term

{-# ANN module "HLint: ignore Use camelCase" #-}

-- Repo
prop_repoAddSuccess n = let name = noLeadingHyphens n in
 testSuccess name x [repoSub, addSub] noOpts [name]
  where x (Global(
           Repo(RepoOpts(
            RepoAdd(RepoAddOpts a))))) = a

prop_repoRemoveSuccess repoNN =
 validOpts [repoNN] ==>
  testSuccess repoNN x [repoSub, removeSub] (repoOpts repoNN) noArgs
   where x (Global(
            Repo(RepoOpts(
             RepoRemove(RepoRemoveOpts a))))) = a

prop_repoListSuccess =
 testSuccess noArgsToGet x [repoSub, listSub] noOpts noArgs
  where x (Global(
           Repo(RepoOpts(
            RepoList RepoListOpts)))) = noArgsToGet

-- Term
prop_termAddSuccess repoNN n s e = let args = validArgs [n, s, e] in length args == 3 ==>
  validOpts [repoNN] ==> let [name, start, end] = args in
   testSuccess (repoNN, name, start, end) x [termSub, addSub] (repoOpts repoNN) args
    where x (Global(
             Term(TermOpts(
              TermAdd(TermAddOpts a b c d))))) = (a,b,c,d)

prop_termRemoveSuccess repoNN termNN =
 validOpts [repoNN, termNN] ==>
  testSuccess (repoNN,termNN) x [termSub, removeSub] (termOpts repoNN termNN) noArgs
   where x (Global(
            Term(TermOpts(
             TermRemove(TermRemoveOpts a b))))) = (a,b)

prop_termListSuccess repoNN =
 validOpts [repoNN] ==>
  testSuccess repoNN x [termSub, listSub] (repoOpts repoNN) noArgs
   where x (Global(
            Term(TermOpts(
             TermList(TermListOpts a))))) = a

prop_termDateSetSuccess repoNN termNN start end =
 validOpts [repoNN, termNN, start, end] ==>
  testSuccess (repoNN, termNN, start, end) x [termSub, dateSub, setSub] (termDateOpts repoNN termNN start end) noArgs
   where x (Global(
            Term(TermOpts(
             TermDate(TermDateOpts(
              TermDateSet(TermDateSetOpts a b c d))))))) = (a,b,c,d)

prop_termDateListSuccess repoNN termNN =
 validOpts [repoNN, termNN] ==>
  testSuccess (repoNN, termNN) x [termSub, dateSub, listSub] (termOpts repoNN termNN) noArgs
   where x (Global(
            Term(TermOpts(
             TermDate(TermDateOpts(
              TermDateList(TermDateListOpts a b))))))) = (a,b)
