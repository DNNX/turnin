{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Service.WorktrainOutputFormatterUnitTest where

import Test.Framework

import Service.WorktrainOutputFormatter 

{-# ANN module "HLint: ignore Use camelCase" #-}

test_stripAccents = do
 assertEqual "" $ stripAccents ""
 assertEqual "SZszYAAAAAACEEEEIIIINOOOOOUUUUYaaaaaaceeeeiiiiooooouuuuy" $ stripAccents "ŠŽšžŸÀÁÂÃÄÅÇÈÉÊËÌÍÎÏÑÒÓÔÕÖÙÚÛÜÝàáâãäåçèéêëìíîïòóôõöùúûüý"
     
test_lineEnd = do
 assertEqual "" $ formatLineEnds ""
 assertEqual "ab\nc\n" $ formatLineEnds "a\rb\r\nc\n"
  
test_maxLineWidth = do
 assertEqual "" $ wrapLines "" 10
 assertEqual "123456789\n" $ wrapLines "123456789" 10
 assertEqual "123456789 \n" $ wrapLines "123456789 " 10
 assertEqual "123456789 \n1\n" $ wrapLines "123456789 1" 10
  
test_headerAndFooter = let
 empty = "\n\n\n\n\n\n\n\n\n ============ output begin ============ \n" ++ 
         "\n" ++ 
         " ============  output end  ============ \n"
 lettersLow = "    @    @@@@@@   @@@@@  @@@@@@  @@@@@@@ @@@@@@@  @@@@@  @     @   @@@         @ @    @  @       @     @ @     @ @@@@@@@ @@@@@@   @@@@@  @@@@@@   @@@@@  @@@@@@@ @     @ @     @ @     @ @     @ @     @ @@@@@@@\n" ++
              "   @ @   @     @ @     @ @     @ @       @       @     @ @     @    @          @ @   @   @       @@   @@ @@    @ @     @ @     @ @     @ @     @ @     @    @    @     @ @     @ @  @  @  @   @   @   @       @ \n" ++
              "  @   @  @     @ @       @     @ @       @       @       @     @    @          @ @  @    @       @ @ @ @ @ @   @ @     @ @     @ @     @ @     @ @          @    @     @ @     @ @  @  @   @ @     @ @       @  \n" ++
              " @     @ @@@@@@  @       @     @ @@@@@   @@@@@   @  @@@@ @@@@@@@    @          @ @@@     @       @  @  @ @  @  @ @     @ @@@@@@  @     @ @@@@@@   @@@@@     @    @     @ @     @ @  @  @    @       @       @   \n" ++
              " @@@@@@@ @     @ @       @     @ @       @       @     @ @     @    @    @     @ @  @    @       @     @ @   @ @ @     @ @       @   @ @ @   @         @    @    @     @  @   @  @  @  @   @ @      @      @    \n" ++
              " @     @ @     @ @     @ @     @ @       @       @     @ @     @    @    @     @ @   @   @       @     @ @    @@ @     @ @       @    @  @    @  @     @    @    @     @   @ @   @  @  @  @   @     @     @     \n" ++
              " @     @ @@@@@@   @@@@@  @@@@@@  @@@@@@@ @        @@@@@  @     @   @@@    @@@@@  @    @  @@@@@@@ @     @ @     @ @@@@@@@ @        @@@@ @ @     @  @@@@@     @     @@@@@     @     @@ @@  @     @    @    @@@@@@@\n" ++
              "\n" ++
              "abcdefghijklmnopqrstuvwxyz" ++
              "\n" ++
              " ============ output begin ============ \n" ++
              "content\n" ++
              " ============  output end  ============ \n"
 lettersUp =  "    @    @@@@@@   @@@@@  @@@@@@  @@@@@@@ @@@@@@@  @@@@@  @     @   @@@         @ @    @  @       @     @ @     @ @@@@@@@ @@@@@@   @@@@@  @@@@@@   @@@@@  @@@@@@@ @     @ @     @ @     @ @     @ @     @ @@@@@@@\n" ++
              "   @ @   @     @ @     @ @     @ @       @       @     @ @     @    @          @ @   @   @       @@   @@ @@    @ @     @ @     @ @     @ @     @ @     @    @    @     @ @     @ @  @  @  @   @   @   @       @ \n" ++
              "  @   @  @     @ @       @     @ @       @       @       @     @    @          @ @  @    @       @ @ @ @ @ @   @ @     @ @     @ @     @ @     @ @          @    @     @ @     @ @  @  @   @ @     @ @       @  \n" ++
              " @     @ @@@@@@  @       @     @ @@@@@   @@@@@   @  @@@@ @@@@@@@    @          @ @@@     @       @  @  @ @  @  @ @     @ @@@@@@  @     @ @@@@@@   @@@@@     @    @     @ @     @ @  @  @    @       @       @   \n" ++
              " @@@@@@@ @     @ @       @     @ @       @       @     @ @     @    @    @     @ @  @    @       @     @ @   @ @ @     @ @       @   @ @ @   @         @    @    @     @  @   @  @  @  @   @ @      @      @    \n" ++
              " @     @ @     @ @     @ @     @ @       @       @     @ @     @    @    @     @ @   @   @       @     @ @    @@ @     @ @       @    @  @    @  @     @    @    @     @   @ @   @  @  @  @   @     @     @     \n" ++
              " @     @ @@@@@@   @@@@@  @@@@@@  @@@@@@@ @        @@@@@  @     @   @@@    @@@@@  @    @  @@@@@@@ @     @ @     @ @@@@@@@ @        @@@@ @ @     @  @@@@@     @     @@@@@     @     @@ @@  @     @    @    @@@@@@@\n" ++
              "\n" ++
              "ABCDEFGHIJKLMNOPQRSTUVWXYZ" ++
              "\n" ++
              " ============ output begin ============ \n" ++
              "content\n" ++
              " ============  output end  ============ \n"              
 others = "   @@@      @     @@@@@   @@@@@  @       @@@@@@@  @@@@@  @@@@@@@  @@@@@   @@@@@         \n" ++
          "  @   @    @@    @     @ @     @ @    @  @       @     @ @    @  @     @ @     @        \n" ++
          " @     @  @ @          @       @ @    @  @       @           @   @     @ @     @        \n" ++
          " @     @    @     @@@@@   @@@@@  @    @  @@@@@@  @@@@@@     @     @@@@@   @@@@@@        \n" ++
          " @     @    @    @             @ @@@@@@@       @ @     @   @     @     @       @        \n" ++
          "  @   @     @    @       @     @      @  @     @ @     @   @     @     @ @     @        \n" ++
          "   @@@    @@@@@  @@@@@@@  @@@@@       @   @@@@@   @@@@@    @      @@@@@   @@@@@  @@@@@@@\n" ++
          "\n" ++
          "0123456789_\n" ++
          " ============ output begin ============ \n" ++
          "content\n" ++
          " ============  output end  ============ \n"
 in do
 assertEqual empty $ addHeaderAndFooter "" ""
 assertEqual lettersLow $ addHeaderAndFooter "content" ['a'..'z']
 assertEqual lettersUp $ addHeaderAndFooter "content" ['A'..'Z']     
 assertEqual others $ addHeaderAndFooter "content" "0123456789_"         
 
test_merge = assertEqual "output1\noutput2\noutput3\n" $ mergeOutputs ["output1", "output2", "output3"]

test_repeats = 
 let toFormat = "a\nb\na\nb\na\nb\nc\na\nb\na\nb\na\nb\nc\na\nb\na\nb\na\nb\nc\n"
     expected = "a\nb\na\nb\n[The previous 2 lines repeat 1 time]\nc\na\nb\na\nb\n[The previous 2 lines repeat 1 time]\nc\n[The previous 6 lines repeat 1 time]\n"
 in do
 assertEqual "[The previous 0 line repeat 1 time]\n" $ formatRepetitions "" 0
 assertEqual "" $ formatRepetitions "" 1
 assertEqual expected $ formatRepetitions toFormat 2 

test_repeatTree = mapM_ f testValues
 where f (toMakeWith,expected) = assertEqual expected (makeRepeatTree toMakeWith)
  
  