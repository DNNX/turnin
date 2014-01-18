module Main where

import Options.Applicative
import Interface.CommandLineParser
import Security.SecurityManager

main :: IO ()
--main = print "Hello World!"
main = execParser (globalInfo adminRole) >>= print