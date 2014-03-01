module Interface.CommandLineParser.SubmitWorktrain where

import Options.Applicative
import Interface.Lexicon
import Interface.CommandLineParser.Utils

data SubmitOpts = SubmitOpts
 { submitRepoNN    :: Maybe String
 , submitTermNN    :: Maybe String
 , submitCourseNN  :: Maybe String
 , submitGroupNN   :: Maybe String
 , submitProjectNN :: Maybe String
 , submitFiles     :: [String]     }                       deriving (Show, Eq)

data InspectOpts = InspectOpts
 { inspectRepoNN    :: Maybe String
 , inspectTermNN    :: Maybe String
 , inspectCourseNN  :: Maybe String
 , inspectGroupNN   :: Maybe String
 , inspectProjectNN :: Maybe String }                      deriving (Show, Eq)

data ExtractOpts = ExtractOpts
 { extractRepoNN    :: Maybe String
 , extractTermNN    :: Maybe String
 , extractCourseNN  :: Maybe String
 , extractGroupNN   :: Maybe String
 , extractProjectNN :: Maybe String
 , extractDir       :: String       }                      deriving (Show, Eq)

data WorktrainOpts = WorktrainOpts    WorktrainCmd         deriving (Show, Eq)
data WorktrainCmd  = WorktrainRun     WorktrainRunOpts
                   | WorktrainDate    WorktrainDateOpts
                   | WorktrainList    WorktrainListOpts
                   | WorktrainExtract WorktrainExtractOpts
                   | WorktrainOutput  WorktrainOutputOpts  deriving (Show, Eq)

data WorktrainRunOpts = WorktrainRunOpts
 { worktrainRunRepoNN    :: Maybe String
 , worktrainRunTermNN    :: Maybe String
 , worktrainRunCourseNN  :: Maybe String
 , worktrainRunGroupNN   :: Maybe String
 , worktrainRunProjectNN :: Maybe String
 , worktrainRunKeys      :: [String]     }                 deriving (Show, Eq)

data WorktrainDateOpts = WorktrainDateOpts
 { worktrainDateRepoNN    :: Maybe String
 , worktrainDateTermNN    :: Maybe String
 , worktrainDateCourseNN  :: Maybe String
 , worktrainDateGroupNN   :: Maybe String
 , worktrainDateProjectNN :: Maybe String }                deriving (Show, Eq)

data WorktrainListOpts = WorktrainListOpts
 { worktrainListRepoNN     :: Maybe String
 , worktrainListTermNN     :: Maybe String
 , worktrainListCourseNN   :: Maybe String
 , worktrainListGroupNN    :: Maybe String
 , worktrainListProjectNN  :: Maybe String
 , worktrainListTrainRunNN :: Maybe String }                deriving (Show, Eq)

data WorktrainExtractOpts = WorktrainExtractOpts
 { worktrainExtractRepoNN     :: Maybe String
 , worktrainExtractTermNN     :: Maybe String
 , worktrainExtractCourseNN   :: Maybe String
 , worktrainExtractGroupNN    :: Maybe String
 , worktrainExtractProjectNN  :: Maybe String
 , worktrainExtractTrainRunNN :: Maybe String
 , worktrainExtractDir        :: String
 , worktrainExtractDirKeys    :: [String]     }             deriving (Show, Eq)

data WorktrainOutputOpts = WorktrainOutputOpts
 { worktrainOutputRepoNN     :: Maybe String
 , worktrainOutputTermNN     :: Maybe String
 , worktrainOutputCourseNN   :: Maybe String
 , worktrainOutputGroupNN    :: Maybe String
 , worktrainOutputProjectNN  :: Maybe String
 , worktrainOutputTrainRunNN :: Maybe String
 , worktrainOutputDir        :: String
 , worktrainOutputKeys       :: [String]
 , worktrainOutputMerge      :: Bool          }              deriving (Show, Eq)


submitInfo =           info (myHelper <*> submit)           (progDesc submitDesc)
inspectInfo =          info (myHelper <*> inspect)          (progDesc inspectDesc)
extractInfo =          info (myHelper <*> extract)          (progDesc extractDesc)
worktrainInfo =        info (myHelper <*> worktrain)        (progDesc worktrainDesc)
worktrainRunInfo =     info (myHelper <*> worktrainRun)     (progDesc worktrainRunDesc)
worktrainDateInfo =    info (myHelper <*> worktrainDate)    (progDesc worktrainDateDesc)
worktrainListInfo =    info (myHelper <*> worktrainList)    (progDesc worktrainListDesc)
worktrainExtractInfo = info (myHelper <*> worktrainExtract) (progDesc worktrainExtractDesc)
worktrainOutputInfo =  info (myHelper <*> worktrainOutput)  (progDesc worktrainOutputDesc)

submit = SubmitOpts
 <$> optional (strOption $ toMod repoNodeOpt <> metavar repoNodeMeta <> help repoNodeHelp)
 <*> optional (strOption $ toMod termNodeOpt <> metavar termNodeMeta <> help termNodeHelp)
 <*> optional (strOption $ toMod courseNodeOpt <> metavar courseNodeMeta <> help courseNodeHelp)
 <*> optional (strOption $ toMod groupNodeOpt <> metavar groupNodeMeta <> help groupNodeHelp)
 <*> optional (strOption $ toMod projectNodeOpt <> metavar projectNodeMeta <> help projectNodeHelp)
 <*> some (argument str (metavar submitMeta <> help submitHelp))

inspect = InspectOpts
 <$> optional (strOption $ toMod repoNodeOpt <> metavar repoNodeMeta <> help repoNodeHelp)
 <*> optional (strOption $ toMod termNodeOpt <> metavar termNodeMeta <> help termNodeHelp)
 <*> optional (strOption $ toMod courseNodeOpt <> metavar courseNodeMeta <> help courseNodeHelp)
 <*> optional (strOption $ toMod groupNodeOpt <> metavar groupNodeMeta <> help groupNodeHelp)
 <*> optional (strOption $ toMod projectNodeOpt <> metavar projectNodeMeta <> help projectNodeHelp)

extract = ExtractOpts
 <$> optional (strOption $ toMod repoNodeOpt <> metavar repoNodeMeta <> help repoNodeHelp)
 <*> optional (strOption $ toMod termNodeOpt <> metavar termNodeMeta <> help termNodeHelp)
 <*> optional (strOption $ toMod courseNodeOpt <> metavar courseNodeMeta <> help courseNodeHelp)
 <*> optional (strOption $ toMod groupNodeOpt <> metavar groupNodeMeta <> help groupNodeHelp)
 <*> optional (strOption $ toMod projectNodeOpt <> metavar projectNodeMeta <> help projectNodeHelp)
 <*> argument str (metavar extractMeta <> help extractHelp)

worktrain = WorktrainOpts <$> subparser (
 command runSub     worktrainRunInfo <>
 command dateSub    worktrainDateInfo <>
 command listSub    worktrainListInfo <>
 command extractSub worktrainExtractInfo <>
 command outputSub  worktrainOutputInfo)

worktrainRun = WorktrainRun <$> (WorktrainRunOpts
 <$> optional (strOption $ toMod repoNodeOpt <> metavar repoNodeMeta <> help repoNodeHelp)
 <*> optional (strOption $ toMod termNodeOpt <> metavar termNodeMeta <> help termNodeHelp)
 <*> optional (strOption $ toMod courseNodeOpt <> metavar courseNodeMeta <> help courseNodeHelp)
 <*> optional (strOption $ toMod groupNodeOpt <> metavar groupNodeMeta <> help groupNodeHelp)
 <*> optional (strOption $ toMod projectNodeOpt <> metavar projectNodeMeta <> help projectNodeHelp)
 <*> some (argument str (metavar worktrainRunMeta <> help worktrainRunHelp)))

worktrainDate = WorktrainDate <$> (WorktrainDateOpts
 <$> optional (strOption $ toMod repoNodeOpt <> metavar repoNodeMeta <> help repoNodeHelp)
 <*> optional (strOption $ toMod termNodeOpt <> metavar termNodeMeta <> help termNodeHelp)
 <*> optional (strOption $ toMod courseNodeOpt <> metavar courseNodeMeta <> help courseNodeHelp)
 <*> optional (strOption $ toMod groupNodeOpt <> metavar groupNodeMeta <> help groupNodeHelp)
 <*> optional (strOption $ toMod projectNodeOpt <> metavar projectNodeMeta <> help projectNodeHelp))

worktrainList = WorktrainList <$> (WorktrainListOpts
 <$> optional (strOption $ toMod repoNodeOpt <> metavar repoNodeMeta <> help repoNodeHelp)
 <*> optional (strOption $ toMod termNodeOpt <> metavar termNodeMeta <> help termNodeHelp)
 <*> optional (strOption $ toMod courseNodeOpt <> metavar courseNodeMeta <> help courseNodeHelp)
 <*> optional (strOption $ toMod groupNodeOpt <> metavar groupNodeMeta <> help groupNodeHelp)
 <*> optional (strOption $ toMod projectNodeOpt <> metavar projectNodeMeta <> help projectNodeHelp)
 <*> optional (strOption $ toMod trainRunNodeOpt <> metavar trainRunNodeMeta <> help trainRunNodeHelp))

worktrainExtract = WorktrainExtract <$> (WorktrainExtractOpts
 <$> optional (strOption $ toMod repoNodeOpt <> metavar repoNodeMeta <> help repoNodeHelp)
 <*> optional (strOption $ toMod termNodeOpt <> metavar termNodeMeta <> help termNodeHelp)
 <*> optional (strOption $ toMod courseNodeOpt <> metavar courseNodeMeta <> help courseNodeHelp)
 <*> optional (strOption $ toMod groupNodeOpt <> metavar groupNodeMeta <> help groupNodeHelp)
 <*> optional (strOption $ toMod projectNodeOpt <> metavar projectNodeMeta <> help projectNodeHelp)
 <*> optional (strOption $ toMod trainRunNodeOpt <> metavar trainRunNodeMeta <> help trainRunNodeHelp)
 <*> argument str (metavar worktrainExtractDirMeta <> metavar worktrainExtractDirHelp)
 <*> some (argument str (metavar worktrainExtractKeysMeta <> metavar worktrainExtractKeysHelp)))

worktrainOutput = WorktrainOutput <$> (WorktrainOutputOpts
 <$> optional (strOption $ toMod repoNodeOpt <> metavar repoNodeMeta <> help repoNodeHelp)
 <*> optional (strOption $ toMod termNodeOpt <> metavar termNodeMeta <> help termNodeHelp)
 <*> optional (strOption $ toMod courseNodeOpt <> metavar courseNodeMeta <> help courseNodeHelp)
 <*> optional (strOption $ toMod groupNodeOpt <> metavar groupNodeMeta <> help groupNodeHelp)
 <*> optional (strOption $ toMod projectNodeOpt <> metavar projectNodeMeta <> help projectNodeHelp)
 <*> optional (strOption $ toMod trainRunNodeOpt <> metavar trainRunNodeMeta <> help trainRunNodeHelp)
 <*> argument str (metavar worktrainOutputDirMeta <> help worktrainOutputDirHelp)
 <*> some (argument str (metavar worktrainOutputKeysMeta <> help worktrainOutputKeysHelp))
 <*> switch (toMod mergeFlag <> metavar worktrainOutputMergeMeta <> help worktrainOutputMergeHelp))


 