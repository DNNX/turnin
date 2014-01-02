module Interface.CommandLineParser.Utils where

import Options.Applicative
import Interface.Lexicon

toMod (s, l) = short s <> long l

myHelper ::  Parser (a -> a)
myHelper = abortOption ShowHelpText $ toMod helpOpt <> help helpHelp