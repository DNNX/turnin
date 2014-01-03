module Interface.CommandLineParser.Term where

import Options.Applicative
import Interface.Lexicon
import Interface.CommandLineParser.Utils

data TermOpts = TermOpts   TermCmd                deriving (Show, Eq)
data TermCmd  = TermAdd    TermAddOpts            
              | TermRemove TermRemoveOpts         
              | TermList   TermListOpts           
              | TermDate   TermDateOpts           deriving (Show, Eq)
                                                  
data TermAddOpts = TermAddOpts                    
 { termAddRepoNN :: Maybe String                    
 , termAddName   :: String 
 , termAddStart  :: String
 , termAddEnd    :: String }                      deriving (Show, Eq)
                                                  
data TermRemoveOpts = TermRemoveOpts              
 { termRemoveRepoNN :: Maybe String                               
 , termRemoveTermNN :: Maybe String }             deriving (Show, Eq)
                                                  
data TermListOpts = TermListOpts
 { termListRepoNN :: Maybe String }               deriving (Show, Eq)
                                                  
data TermDateOpts = TermDateOpts TermDateCmd      deriving (Show, Eq)
data TermDateCmd  = TermDateSet  TermDateSetOpts 
                  | TermDateList TermDateListOpts deriving (Show, Eq)
                  
data TermDateSetOpts = TermDateSetOpts
 { termDateSetRepoNN :: Maybe String
 , termDateSetTermNN :: Maybe String
 , termDateSetStart  :: Maybe String
 , termDateSetEnd    :: Maybe String }            deriving (Show, Eq)    
 
data TermDateListOpts = TermDateListOpts
 { termDateListRepoNN :: Maybe String
 , termDateListTermNN :: Maybe String }           deriving (Show, Eq)        
 
termInfo =         info (myHelper <*> term)         (progDesc termDesc)
termAddInfo =      info (myHelper <*> termAdd)      (progDesc termAddDesc)
termRemoveInfo =   info (myHelper <*> termRemove)   (progDesc termRemoveDesc)  
termListInfo =     info (myHelper <*> termList)     (progDesc termListDesc)
termDateInfo =     info (myHelper <*> termDate)     (progDesc termDateDesc)
termDateSetInfo =  info (myHelper <*> termDateSet)  (progDesc termDateSetDesc)
termDateListInfo = info (myHelper <*> termDateList) (progDesc termDateListDesc)
 
term = TermOpts <$> subparser (
 command addSub    termAddInfo <>
 command removeSub termRemoveInfo <>
 command listSub   termListInfo <>
 command dateSub   termDateInfo)

termAdd = TermAdd <$> (TermAddOpts
 <$> optional (strOption $ toMod repoNodeOpt <> metavar repoNodeMeta <> help repoNodeHelp)
 <*> argument str (metavar termAddNameMeta <> help termAddNameHelp)
 <*> argument str (metavar termAddStartMeta <> help termAddStartHelp)
 <*> argument str (metavar termAddEndMeta <> help termAddEndHelp))
 
termRemove = TermRemove <$> (TermRemoveOpts
 <$> optional (strOption $ toMod repoNodeOpt <> metavar repoNodeMeta <> help repoNodeHelp)
 <*> optional (strOption $ toMod termNodeOpt <> metavar termNodeMeta <> help termNodeHelp))
 
termList = TermList <$> (TermListOpts
 <$> optional (strOption $ toMod repoNodeOpt <> metavar repoNodeMeta <> help repoNodeHelp))
  
termDate = TermDate <$> TermDateOpts <$> subparser (
 command setSub  termDateSetInfo <>
 command listSub termDateListInfo) 
 
termDateSet = TermDateSet <$> (TermDateSetOpts
 <$> optional (strOption $ toMod repoNodeOpt <> metavar repoNodeMeta <> help repoNodeHelp)
 <*> optional (strOption $ toMod termNodeOpt <> metavar termNodeMeta <> help termNodeHelp)
 <*> optional (strOption $ toMod termDateSetStartOpt <> metavar termDateSetStartMeta <> help termDateSetStartHelp)
 <*> optional (strOption $ toMod termDateSetEndOpt   <> metavar termDateSetEndMeta   <> help termDateSetEndHelp))

termDateList = TermDateList <$> (TermDateListOpts
 <$> optional (strOption $ toMod repoNodeOpt <> metavar repoNodeMeta <> help repoNodeHelp)
 <*> optional (strOption $ toMod termNodeOpt <> metavar termNodeMeta <> help termNodeHelp))
 
 