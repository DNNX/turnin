{-# OPTIONS_GHC -F -pgmF htfpp -fno-warn-incomplete-patterns#-}
module Infrastructure.Finder.FinderUnitFindingTest where

import Test.Framework
import Control.Arrow (first)

import Infrastructure.Node
import Domain.Root
import Domain.Project
import Domain.ProjectRepo

import Infrastructure.Finder

{-# ANN module "HLint: ignore Use camelCase" #-}
{-# ANN module "HLint: ignore Reduce duplication" #-}
{-# ANN module "HLint: ignore Evaluate" #-}
{-# ANN module "HLint: ignore Use ++" #-}

test_findNoHints = do
  let tr = make trN
      trr = addChild emptyTrainRunRepo tr
      p = setTrainRunRepo (make pN) trr
      g = addChild (make gN) p
      c = addChild (make cN) g
      t = addChild (make tN) c
      r = addChild (make rN) t
      root = addChild (make rootN :: Root) r

      pSubmitR = makeProjectSubmitRepo emptySubmitRepo
      pTrainFileR = makeProjectTrainFileRepo emptyTrainFileRepo
      pTrainRunR = makeProjectTrainRunRepo trr
      prs = [pSubmitR,pTrainFileR,pTrainRunR]
      prs1 = map (\x -> (K pN Z,x)) prs
      prs2 = map (first (K gN)) prs1
      prs3 = map (first (K cN)) prs2
      prs4 = map (first (K tN)) prs3
      prs5 = map (first (K rN)) prs4
      prs6 = map (first (K rootN)) prs5

      rootN = "root"
      rN = "repo"
      tN = "term"
      cN = "course"
      gN = "group"
      pN = "project"
      trN = "trainRun"

      trrN = getName trr

  assertEqual [(Z,root)] (find zero root)                               >> assertEqual (Just root) (findUnambiguous zero root)                               
  assertEqual [(Z,r)]    (find zero r)                                  >> assertEqual (Just r)    (findUnambiguous zero r)                                  
  assertEqual [(Z,t)]    (find zero t)                                  >> assertEqual (Just t)    (findUnambiguous zero t)                                  
  assertEqual [(Z,c)]    (find zero c)                                  >> assertEqual (Just c)    (findUnambiguous zero c)                                  
  assertEqual [(Z,g)]    (find zero g)                                  >> assertEqual (Just g)    (findUnambiguous zero g)                                  
  assertEqual [(Z,p)]    (find zero p)                                  >> assertEqual (Just p)    (findUnambiguous zero p)                                  
  assertEqual [(Z,trr)]  (find zero trr)                                >> assertEqual (Just trr)  (findUnambiguous zero trr)                                
  assertEqual [(Z,tr)]   (find zero tr)                                 >> assertEqual (Just tr)   (findUnambiguous zero tr)                                 
                                                                                                                                            
  assertEqual [(one rootN,r)] (find one' root)                          >> assertEqual (Just r) (findUnambiguous one' root)                          
  assertEqual [(one rN,t)]    (find one' r)                             >> assertEqual (Just t) (findUnambiguous one' r)                             
  assertEqual [(one tN,c)]    (find one' t)                             >> assertEqual (Just c) (findUnambiguous one' t)                             
  assertEqual [(one cN,g)]    (find one' c)                             >> assertEqual (Just g) (findUnambiguous one' c)                             
  assertEqual [(one gN,p)]    (find one' g)                             >> assertEqual (Just p) (findUnambiguous one' g)                             
  assertEqual prs1            (find one' p)                                                 
  assertEqual [(one trrN,tr)] (find one' trr)                           >> assertEqual (Just tr) (findUnambiguous one' trr)                           
                                                                                                                                           
  assertEqual [(two rootN rN,t)] (find two' root)                       >> assertEqual (Just t) (findUnambiguous two' root)                       
  assertEqual [(two rN tN,c)]    (find two' r)                          >> assertEqual (Just c) (findUnambiguous two' r)                          
  assertEqual [(two tN cN,g)]    (find two' t)                          >> assertEqual (Just g) (findUnambiguous two' t)                          
  assertEqual [(two cN gN,p)]    (find two' c)                          >> assertEqual (Just p) (findUnambiguous two' c)                          
  assertEqual prs2               (find two' g)                                          
  assertEqual [(two pN trrN,tr)] (find two' p)                          >> assertEqual (Just tr) (findUnambiguous two' p)                          
                                                                                                                                            
  assertEqual [(three rootN rN tN,c)] (find three' root)                >> assertEqual (Just c) (findUnambiguous three' root)                
  assertEqual [(three rN tN cN,g)]    (find three' r)                   >> assertEqual (Just g) (findUnambiguous three' r)                   
  assertEqual [(three tN cN gN,p)]    (find three' t)                   >> assertEqual (Just p) (findUnambiguous three' t)                   
   
  assertEqual [(three gN pN trrN,tr)] (find three' g)                   >> assertEqual (Just tr) (findUnambiguous three' g)                   
                                                                                                                                         
  assertEqual [(four rootN rN tN cN,g)]  (find four' root)              >> assertEqual (Just g) (findUnambiguous four' root)              
  assertEqual [(four rN tN cN gN,p)]     (find four' r)                 >> assertEqual (Just p) (findUnambiguous four' r)                 
  
  assertEqual [(four cN gN pN trrN, tr)] (find four' c)                 >> assertEqual (Just tr) (findUnambiguous four' c)                 
                                                                                                                                            
  assertEqual [(five rootN rN tN cN gN,p)] (find five' root)            >> assertEqual (Just p) (findUnambiguous five' root)            
  
  assertEqual [(five tN cN gN pN trrN,tr)] (find five' t)               >> assertEqual (Just tr) (findUnambiguous five' t)               
                                                                                                                                               
  assertEqual prs6                           (find six' root)               
  assertEqual [(six rN tN cN gN pN trrN,tr)] (find six' r)              >> assertEqual (Just tr) (findUnambiguous six' r)              
                                                                                                                                               
  assertEqual [(seven rootN rN tN cN gN pN trrN,tr)] (find seven' root) >> assertEqual (Just tr) (findUnambiguous seven' root) 

test_findAllHints = do
  let tr = make trN
      trr = addChild emptyTrainRunRepo tr
      pSubmitR = makeProjectSubmitRepo emptySubmitRepo
      pTrainFileR = makeProjectTrainFileRepo emptyTrainFileRepo
      pTrainRunR = makeProjectTrainRunRepo trr
      p = setTrainRunRepo (make pN) trr
      g = addChild (make gN) p
      c = addChild (make cN) g
      t = addChild (make tN) c
      r = addChild (make rN) t
      root = addChild (make rootN :: Root) r

      rootN = "root"
      rN = "repo"
      tN = "term"
      cN = "course"
      gN = "group"
      pN = "project"
      trN = "trainRun"
      sRN = getName emptySubmitRepo
      tfRN = getName emptyTrainFileRepo
      trRN = getName emptyTrainRunRepo
  assertEqual [(one rootN,r)]        (find (oneM rN) root)                                          >> assertEqual (Just r)           (findUnambiguous (oneM rN) root)                                         
  assertEqual [(one rN,t)]           (find (oneM tN) r)                                             >> assertEqual (Just t)           (findUnambiguous (oneM tN) r)                                            
  assertEqual [(one tN,c)]           (find (oneM cN) t)                                             >> assertEqual (Just c)           (findUnambiguous (oneM cN) t)                                            
  assertEqual [(one cN,g)]           (find (oneM gN) c)                                             >> assertEqual (Just g)           (findUnambiguous (oneM gN) c)                                            
  assertEqual [(one gN,p)]           (find (oneM pN) g)                                             >> assertEqual (Just p)           (findUnambiguous (oneM pN) g)                                            
  assertEqual [(one pN,pSubmitR)]    (find (oneM sRN) p)                                            >> assertEqual (Just pSubmitR)    (findUnambiguous (oneM sRN) p)                                           
  assertEqual [(one pN,pTrainFileR)] (find (oneM tfRN) p)                                           >> assertEqual (Just pTrainFileR) (findUnambiguous (oneM tfRN) p)                                          
  assertEqual [(one pN,pTrainRunR)]  (find (oneM trRN) p)                                           >> assertEqual (Just pTrainRunR)  (findUnambiguous (oneM trRN) p)                                          
  assertEqual [(one trRN,tr)]        (find (oneM trN) trr)                                          >> assertEqual (Just tr)          (findUnambiguous (oneM trN) trr)                                         
                                                                                                                                                                                             
  assertEqual [(two rootN rN,t)]        (find (twoM rN tN) root)                                    >> assertEqual (Just t)           (findUnambiguous (twoM rN tN) root)                                   
  assertEqual [(two rN tN,c)]           (find (twoM tN cN) r)                                       >> assertEqual (Just c)           (findUnambiguous (twoM tN cN) r)                                      
  assertEqual [(two tN cN,g)]           (find (twoM cN gN) t)                                       >> assertEqual (Just g)           (findUnambiguous (twoM cN gN) t)                                      
  assertEqual [(two cN gN,p)]           (find (twoM gN pN) c)                                       >> assertEqual (Just p)           (findUnambiguous (twoM gN pN) c)                                      
  assertEqual [(two gN pN,pSubmitR)]    (find (twoM pN sRN) g)                                      >> assertEqual (Just pSubmitR)    (findUnambiguous (twoM pN sRN) g)                                     
  assertEqual [(two gN pN,pTrainFileR)] (find (twoM pN tfRN) g)                                     >> assertEqual (Just pTrainFileR) (findUnambiguous (twoM pN tfRN) g)                                    
  assertEqual [(two gN pN,pTrainRunR)]  (find (twoM pN trRN) g)                                     >> assertEqual (Just pTrainRunR)  (findUnambiguous (twoM pN trRN) g)                                    
  assertEqual [(two pN trRN,tr)]        (find (twoM trRN trN) p)                                    >> assertEqual (Just tr)          (findUnambiguous (twoM trRN trN) p)                                   
                                                                                                                                                                                                 
  assertEqual [(three rootN rN tN,c)]        (find (threeM rN tN cN) root)                          >> assertEqual (Just c)           (findUnambiguous (threeM rN tN cN) root)                         
  assertEqual [(three rN tN cN,g)]           (find (threeM tN cN gN) r)                             >> assertEqual (Just g)           (findUnambiguous (threeM tN cN gN) r)                            
  assertEqual [(three tN cN gN,p)]           (find (threeM cN gN pN) t)                             >> assertEqual (Just p)           (findUnambiguous (threeM cN gN pN) t)                            
  assertEqual [(three cN gN pN,pSubmitR)]    (find (threeM gN pN sRN) c)                            >> assertEqual (Just pSubmitR)    (findUnambiguous (threeM gN pN sRN) c)                           
  assertEqual [(three cN gN pN,pTrainFileR)] (find (threeM gN pN tfRN) c)                           >> assertEqual (Just pTrainFileR) (findUnambiguous (threeM gN pN tfRN) c)                          
  assertEqual [(three cN gN pN,pTrainRunR)]  (find (threeM gN pN trRN) c)                           >> assertEqual (Just pTrainRunR)  (findUnambiguous (threeM gN pN trRN) c)                          
  assertEqual [(three gN pN trRN,tr)]        (find (threeM pN trRN trN) g)                          >> assertEqual (Just tr)          (findUnambiguous (threeM pN trRN trN) g)                         
                                                                                                                                                                                              
  assertEqual [(four rootN rN tN cN,g)]        (find (fourM rN tN cN gN) root)                      >> assertEqual (Just g)           (findUnambiguous (fourM rN tN cN gN) root)                     
  assertEqual [(four rN tN cN gN,p)]           (find (fourM tN cN gN pN) r)                         >> assertEqual (Just p)           (findUnambiguous (fourM tN cN gN pN) r)                        
  assertEqual [(four tN cN gN pN,pSubmitR)]    (find (fourM cN gN pN sRN) t)                        >> assertEqual (Just pSubmitR)    (findUnambiguous (fourM cN gN pN sRN) t)                       
  assertEqual [(four tN cN gN pN,pTrainFileR)] (find (fourM cN gN pN tfRN) t)                       >> assertEqual (Just pTrainFileR) (findUnambiguous (fourM cN gN pN tfRN) t)                      
  assertEqual [(four tN cN gN pN,pTrainRunR)]  (find (fourM cN gN pN trRN) t)                       >> assertEqual (Just pTrainRunR)  (findUnambiguous (fourM cN gN pN trRN) t)                      
  assertEqual [(four cN gN pN trRN,tr)]        (find (fourM gN pN trRN trN) c)                      >> assertEqual (Just tr)          (findUnambiguous (fourM gN pN trRN trN) c)                     
                        
  assertEqual [(five rootN rN tN cN gN,p)]        (find (fiveM rN tN cN gN pN) root)                >> assertEqual (Just p)           (findUnambiguous (fiveM rN tN cN gN pN) root)               
  assertEqual [(five rN tN cN gN pN,pSubmitR)]    (find (fiveM tN cN gN pN sRN) r)                  >> assertEqual (Just pSubmitR)    (findUnambiguous (fiveM tN cN gN pN sRN) r)                 
  assertEqual [(five rN tN cN gN pN,pTrainFileR)] (find (fiveM tN cN gN pN tfRN) r)                 >> assertEqual (Just pTrainFileR) (findUnambiguous (fiveM tN cN gN pN tfRN) r)                
  assertEqual [(five rN tN cN gN pN,pTrainRunR)]  (find (fiveM tN cN gN pN trRN) r)                 >> assertEqual (Just pTrainRunR)  (findUnambiguous (fiveM tN cN gN pN trRN) r)                
  assertEqual [(five tN cN gN pN trRN, tr)]       (find (fiveM cN gN pN trRN trN) t)                >> assertEqual (Just tr)          (findUnambiguous (fiveM cN gN pN trRN trN) t)               
                                                                                                                                                                                                 
  assertEqual [(six rootN rN tN cN gN pN,pSubmitR)]    (find (sixM rN tN cN gN pN sRN) root)        >> assertEqual (Just pSubmitR)    (findUnambiguous (sixM rN tN cN gN pN sRN) root)       
  assertEqual [(six rootN rN tN cN gN pN,pTrainFileR)] (find (sixM rN tN cN gN pN tfRN) root)       >> assertEqual (Just pTrainFileR) (findUnambiguous (sixM rN tN cN gN pN tfRN) root)      
  assertEqual [(six rootN rN tN cN gN pN,pTrainRunR)]  (find (sixM rN tN cN gN pN trRN) root)       >> assertEqual (Just pTrainRunR)  (findUnambiguous (sixM rN tN cN gN pN trRN) root)      
  assertEqual [(six rN tN cN gN pN trRN,tr)]           (find (sixM tN cN gN pN trRN trN) r)         >> assertEqual (Just tr)          (findUnambiguous (sixM tN cN gN pN trRN trN) r)        
                                                                                                                                                                                               
  assertEqual [(seven rootN rN tN cN gN pN trRN,tr)]   (find (sevenM rN tN cN gN pN trRN trN) root) >> assertEqual (Just tr)          (findUnambiguous (sevenM rN tN cN gN pN trRN trN) root)

zero   = Z
one'   = S Nothing zero
two'   = S Nothing one'
three' = S Nothing two'
four'  = S Nothing three'
five'  = S Nothing four'
six'   = S Nothing five'
seven' = S Nothing six'

one a               = K a zero
two a b             = K a $ one b
three a b c         = K a $ two b c
four a b c d        = K a $ three b c d
five a b c d e      = K a $ four b c d e
six a b c d e f     = K a $ five b c d e f
seven a b c d e f g = K a $ six b c d e f g

oneM a               = S (Just a) zero
twoM a b             = S (Just a) $ oneM b
threeM a b c         = S (Just a) $ twoM b c
fourM a b c d        = S (Just a) $ threeM b c d
fiveM a b c d e      = S (Just a) $ fourM b c d e
sixM a b c d e f     = S (Just a) $ fiveM b c d e f
sevenM a b c d e f g = S (Just a) $ sixM b c d e f g


