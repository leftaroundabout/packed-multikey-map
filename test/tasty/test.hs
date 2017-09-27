-- |
-- Module      : Main
-- Copyright   : (c) Justus Sagemüller 2017
-- License     : GPL v3
-- 
-- Maintainer  : (@) sagemueller $ geo.uni-koeln.de
-- Stability   : experimental
-- Portability : portable
-- 

module Main where

import Data.Map.Multikey.Packed

import Test.Tasty
import Test.Tasty.HUnit



main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
 [ testGroup "Simple list conversion"
   [ testCase "Flat map"
      $ (toList <$> fromList' [(0,'x'), (1,'y') :: (Int,Char)])
        @?= Just [(0,'x'), (1,'y')]
   , testCase "Degerate flat map"
      $ (toList <$> fromList' [(0,'x'), (0,'y') :: (Int,Char)])
        @?= Just [(0,'x')]
   , testCase "Simple table"
      $ (toList <$> fromList'
           [ ((0,0),'x'), ((0,1),'y'), ((0,2),'p')
           , ((1,0),'a'), ((1,1),'b'), ((1,2),'q') :: ((Int,Int),Char) ])
        @?= Just (zip [(i,j)|i<-[0,1],j<-[0..2]] "xypabq")
   , testCase "Incomplete table"
      $ (toList <$> fromList'
           [ ((0,0),'x'), ((0,1),'y'), ((0,2),'p')
           , ((1,0),'a'),              ((1,2),'q') :: ((Int,Int),Char) ])
        @?= Nothing
   ]
 ]


