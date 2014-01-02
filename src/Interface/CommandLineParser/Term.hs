module Interface.CommandLineParser.Term where

import Options.Applicative
import Interface.Lexicon
import Interface.CommandLineParser.Utils

data TermOpts = TermOpts   TermCmd                deriving (Show)
data TermCmd  = TermAdd    TermAddOpts            
              | TermRemove TermRemoveOpts         
              | TermList   TermListOpts           
              | TermDate   TermDateOpts           deriving (Show)
                                                  
data TermAddOpts = TermAddOpts                    
 { termAddRepoNN :: Maybe String                    
 , termAddName   :: String 
 , termAddStart  :: String
 , termAddEnd    :: String }                      deriving (Show)
                                                  
data TermRemoveOpts = TermRemoveOpts              
 { termRemoveRepoNN :: Maybe String                               
 , termRemoveTermNN :: Maybe String }             deriving (Show)
                                                  
data TermListOpts = TermListOpts
 { termListRepoNN :: Maybe String }               deriving (Show)
                                                  
data TermDateOpts = TermDateOpts TermDateCmd      deriving (Show)
data TermDateCmd  = TermDateSet  TermDateSetOpts 
                  | TermDateList TermDateListOpts deriving (Show)
                  
data TermDateSetOpts = TermDateSetOpts
 { dateSetRepoNN :: Maybe String
 , dateSetTermNN :: Maybe String
 , dateSetStart  :: Maybe String
 , dateSetEnd    :: Maybe String }                deriving (Show)    
 
data TermDateListOpts = TermDateListOpts
 { dateListRepoNN :: Maybe String
 , dateListTermNN :: Maybe String }               deriving (Show)        
 
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
 <*> argument str (metavar termAddStartMeta <> help termAddNameHelp)
 <*> argument str (metavar termAddEndMeta <> help termAddNameHelp))
 
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
  
 
 