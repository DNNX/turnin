module Main where

import Options.Applicative
import Interface.CommandLineParser

main :: IO ()
--main = print "Hello World!"
main = execParser globalInfo >>= print