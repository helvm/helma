module HelVM.HelCam.Machines.WhiteSpace.EvaluatorTestData where

import HelVM.HelCam.Machines.WhiteSpace.Token
import HelVM.HelCam.Machines.WhiteSpace.Instruction

import HelVM.HelCam.Common.Util

calcTL :: TokenList
calcTL =
  [S,S,S,S,N,S,S,S,T,S,S,S,T,S,T,N,T,T,S,S,S,S,T,N,S,S,S,T,T,S,T,T,T,S,N,T,T,S,S,S,S,T,S,N,S,S,S,T,T,T,S,T,S,S,N,T,T,S,S,S,S,T,T,N,S,S,S,T,T,S,S,T,S,T,N,T,T,S,S,S,S,T,S,S,N,S,S,S,T,T,T,S,S,T,S,N,T,T,S,S,S,S,T,S,T,N,S,S,S,T,S,S,S,S,S,N,T,T,S,S,S,S,T,T,S,N,S,S,S,T,T,T,S,S,T,T,N,T,T,S,S,S,S,T,T,T,N,S,S,S,T,T,S,T,T,T,T,N,T,T,S,S,S,S,T,S,S,S,N,S,S,S,T,T,S,T,T,S,T,N,T,T,S,S,S,S,T,S,S,T,N,S,S,S,T,T,S,S,T,S,T,N,T,T,S,S,S,S,T,S,T,S,N,S,S,S,T,S,S,S,S,S,N,T,T,S,S,S,S,T,S,T,T,N,S,S,S,T,T,S,T,T,T,S,N,T,T,S,S,S,S,T,T,S,S,N,S,S,S,T,T,T,S,T,S,T,N,T,T,S,S,S,S,T,T,S,T,N,S,S,S,T,T,S,T,T,S,T,N,T,T,S,S,S,S,T,T,T,S,N,S,S,S,T,T,S,S,S,T,S,N,T,T,S,S,S,S,T,T,T,T,N,S,S,S,T,T,S,S,T,S,T,N,T,T,S,S,S,S,T,S,S,S,S,N,S,S,S,T,T,T,S,S,T,S,N,T,T,S,S,S,S,T,S,S,S,T,N,S,S,S,T,T,T,S,S,T,T,N,T,T,S,S,S,S,T,S,S,T,S,N,S,S,S,T,S,T,T,S,S,N,T,T,S,S,S,S,T,S,S,T,T,N,S,S,S,T,S,S,S,S,S,N,T,T,S,S,S,S,T,S,T,S,S,N,S,S,S,T,T,T,S,T,S,S,N,T,T,S,S,S,S,T,S,T,S,T,N,S,S,S,T,T,S,T,S,S,S,N,T,T,S,S,S,S,T,S,T,T,S,N,S,S,S,T,T,S,S,T,S,T,N,T,T,S,S,S,S,T,S,T,T,T,N,S,S,S,T,T,S,T,T,T,S,N,T,T,S,S,S,S,T,T,S,S,S,N,S,S,S,T,S,S,S,S,S,N,T,T,S,S,S,S,T,T,S,S,T,N,S,S,S,T,S,T,T,S,T,N,T,T,S,S,S,S,T,T,S,T,S,N,S,S,S,T,T,S,S,S,T,N,T,T,S,S,S,S,T,T,S,T,T,N,S,S,S,T,S,S,S,S,S,N,T,T,S,S,S,S,T,T,T,S,S,N,S,S,S,T,T,T,S,T,S,S,N,T,T,S,S,S,S,T,T,T,S,T,N,S,S,S,T,T,S,T,T,T,T,N,T,T,S,S,S,S,T,T,T,T,S,N,S,S,S,T,S,S,S,S,S,N,T,T,S,S,S,S,T,T,T,T,T,N,S,S,S,T,T,S,S,T,T,S,N,T,T,S,S,S,S,T,S,S,S,S,S,N,S,S,S,T,T,S,T,S,S,T,N,T,T,S,S,S,S,T,S,S,S,S,T,N,S,S,S,T,T,S,T,T,T,S,N,T,T,S,S,S,S,T,S,S,S,T,S,N,S,S,S,T,T,S,T,S,S,T,N,T,T,S,S,S,S,T,S,S,S,T,T,N,S,S,S,T,T,T,S,S,T,T,N,T,T,S,S,S,S,T,S,S,T,S,S,N,S,S,S,T,T,S,T,S,S,S,N,T,T,S,S,S,S,T,S,S,T,S,T,N,S,S,S,S,N,T,T,S,S,S,S,T,S,T,S,T,S,N,S,S,S,T,S,S,T,T,T,S,N,T,T,S,S,S,S,T,S,T,S,T,T,N,S,S,S,T,T,T,S,T,S,T,N,T,T,S,S,S,S,T,S,T,T,S,S,N,S,S,S,T,T,S,T,T,S,T,N,T,T,S,S,S,S,T,S,T,T,S,T,N,S,S,S,T,T,S,S,S,T,S,N,T,T,S,S,S,S,T,S,T,T,T,S,N,S,S,S,T,T,S,S,T,S,T,N,T,T,S,S,S,S,T,S,T,T,T,T,N,S,S,S,T,T,T,S,S,T,S,N,T,T,S,S,S,S,T,T,S,S,S,S,N,S,S,S,T,T,T,S,T,S,N,T,T,S,S,S,S,T,T,S,S,S,T,N,S,S,S,S,N,T,T,S,S,S,S,T,T,T,T,S,S,N,S,S,S,T,S,T,S,T,S,S,N,T,T,S,S,S,S,T,T,T,T,S,T,N,S,S,S,T,T,S,T,T,T,T,N,T,T,S,S,S,S,T,T,T,T,T,S,N,S,S,S,T,T,T,S,T,S,S,N,T,T,S,S,S,S,T,T,T,T,T,T,N,S,S,S,T,T,S,S,S,S,T,N,T,T,S,S,S,S,T,S,S,S,S,S,S,N,S,S,S,T,T,S,T,T,S,S,N,T,T,S,S,S,S,T,S,S,S,S,S,T,N,S,S,S,T,S,S,S,S,S,N,T,T,S,S,S,S,T,S,S,S,S,T,S,N,S,S,S,T,T,S,T,S,S,T,N,T,T,S,S,S,S,T,S,S,S,S,T,T,N,S,S,S,T,T,T,S,S,T,T,N,T,T,S,S,S,S,T,S,S,S,T,S,S,N,S,S,S,T,S,S,S,S,S,N,T,T,S,S,S,S,T,S,S,S,T,S,T,N,S,S,S,S,N,T,T,S,S,S,S,S,N,N,S,T,S,T,T,T,S,T,T,T,S,T,T,T,S,S,T,S,S,T,T,S,T,S,S,T,S,T,T,T,S,T,S,S,S,T,T,S,S,T,S,T,N,N,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,T,S,T,T,T,S,T,T,T,S,T,T,S,T,T,S,S,S,T,T,S,T,S,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,T,N,S,S,S,T,T,S,S,T,S,S,N,S,S,S,S,N,T,T,S,N,S,S,S,T,T,S,T,T,S,S,S,T,T,S,T,T,T,T,S,T,T,S,T,T,T,T,S,T,T,T,S,S,S,S,N,S,S,S,T,S,T,S,T,S,N,N,S,T,S,T,T,T,S,T,T,T,S,T,T,T,S,S,T,S,S,T,T,S,T,S,S,T,S,T,T,T,S,T,S,S,S,T,T,S,S,T,S,T,N,S,S,S,T,T,S,S,T,S,T,N,T,N,T,T,S,S,S,T,T,S,S,T,S,T,N,T,T,T,S,N,S,S,S,T,T,N,T,S,S,T,N,T,S,S,T,T,T,S,S,S,S,S,T,T,T,S,S,T,S,S,T,T,S,T,T,T,T,S,T,T,S,S,T,T,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,S,N,S,S,S,T,T,S,S,T,S,S,N,T,T,T,T,S,S,S,S,S,S,T,T,S,S,T,S,S,N,S,N,T,T,T,S,N,S,N,S,T,T,S,T,T,S,S,S,T,T,S,T,T,T,T,S,T,T,S,T,T,T,T,S,T,T,T,S,S,S,S,N,N,S,S,S,T,T,T,S,S,S,S,S,T,T,T,S,S,T,S,S,T,T,S,T,T,T,T,S,T,T,S,S,T,T,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,S,N,S,N,N,S,S,S,T,T,T,T,S,S,N,N,S,T,S,T,T,T,S,T,T,T,S,T,T,T,S,S,T,S,S,T,T,S,T,S,S,T,S,T,T,T,S,T,S,S,S,T,T,S,S,T,S,T,N,S,S,S,T,T,S,S,T,S,S,N,T,T,T,T,N,S,T,N,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,T,S,T,T,T,S,T,T,T,S,T,T,S,T,T,S,S,S,T,T,S,T,S,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,T,N,N,N,N,N,S,S,S,T,T,S,S,S,S,T,S,T,T,S,S,T,S,S,S,T,T,S,S,T,S,S,N,T,S,S,S,N,T,N,N,S,S,S,T,T,T,S,T,T,T,S,T,T,T,S,S,T,S,S,T,T,S,T,S,S,T,S,T,T,T,S,T,S,S,S,T,T,S,S,T,S,T,N,S,N,S,T,T,T,S,N,S,N,T,S,S,T,T,T,S,T,T,T,S,T,T,T,S,S,T,S,S,T,T,S,T,S,S,T,S,T,T,T,S,T,S,S,S,T,T,S,S,T,S,T,S,T,S,T,T,T,T,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,S,N,T,N,S,S,S,S,S,T,N,T,S,S,S,N,S,N,S,T,T,T,S,T,T,T,S,T,T,T,S,S,T,S,S,T,T,S,T,S,S,T,S,T,T,T,S,T,S,S,S,T,T,S,S,T,S,T,N,N,S,S,S,T,T,T,S,T,T,T,S,T,T,T,S,S,T,S,S,T,T,S,T,S,S,T,S,T,T,T,S,T,S,S,S,T,T,S,S,T,S,T,S,T,S,T,T,T,T,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,S,N,S,N,N,S,N,N,N,T,N,N,S,S,S,T,T,T,S,S,T,S,S,T,T,S,S,T,S,T,S,T,T,S,S,S,S,T,S,T,T,S,S,T,S,S,N,S,N,S,S,N,S,T,N,T,S,T,T,T,S,N,S,S,S,S,T,S,T,S,N,T,S,S,T,N,T,S,S,T,T,T,S,S,T,S,S,T,T,S,S,T,S,T,S,T,T,S,S,S,S,T,S,T,T,S,S,T,S,S,S,T,S,T,T,T,T,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,S,N,S,N,N,S,S,S,T,N,T,S,S,S,N,S,N,S,T,T,T,S,S,T,S,S,T,T,S,S,T,S,T,S,T,T,S,S,S,S,T,S,T,T,S,S,T,S,S,N,N,S,S,S,T,T,T,S,S,T,S,S,T,T,S,S,T,S,T,S,T,T,S,S,S,S,T,S,T,T,S,S,T,S,S,S,T,S,T,T,T,T,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,S,N,S,N,N,S,S,S,T,N,T,S,S,S,S,S,S,S,N,T,T,S,N,T,N,N,S,S,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,T,S,T,T,T,S,T,T,T,S,T,T,S,T,T,S,S,S,T,T,S,T,S,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,T,N,S,S,S,T,S,T,S,N,S,S,S,T,T,S,T,N,T,N,S,S,T,N,S,S,N,T,N]

catTL :: TokenList
catTL =
  [N,S,S,S,N
  ,S,S,S,T,N
  ,T,N,T,S
  ,S,S,S,T,N
  ,T,T,T
  ,T,N,S,S
  ,S,S,S,T,N
  ,N,T,S,T,N
  ,N,S,N,S,N
  ,N,S,S,T,N
  ,N,N,N
  ]


countTL :: TokenList
countTL =
  [S,S,S,T,N,N,S,S,S,T,S,S,S,S,T,T,N,S,N,S,T,N,S,T,S,S,S,T,S,T,S,N,T,N,S,S,S,S,S,T,N,T,S,S,S,S,N,S,S,S,S,T,S,T,T,N,T,S,S,T,N,T,S,S,T,S,S,S,T,S,T,N,N,S,N,S,T,S,S,S,S,T,T,N,N,S,S,S,T,S,S,S,T,S,T,N,S,N,N,N,N,N,N,S,S,S,T,T,S,S,S,S,T,S,T,T,S,S,T,S,S,S,T,T,S,S,T,S,S,N,T,S,S,S,N,T,N,N,S,S,S,T,T,T,S,T,T,T,S,T,T,T,S,S,T,S,S,T,T,S,T,S,S,T,S,T,T,T,S,T,S,S,S,T,T,S,S,T,S,T,N,S,N,S,T,T,T,S,N,S,N,T,S,S,T,T,T,S,T,T,T,S,T,T,T,S,S,T,S,S,T,T,S,T,S,S,T,S,T,T,T,S,T,S,S,S,T,T,S,S,T,S,T,S,T,S,T,T,T,T,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,S,N,T,N,S,S,S,S,S,T,N,T,S,S,S,N,S,N,S,T,T,T,S,T,T,T,S,T,T,T,S,S,T,S,S,T,T,S,T,S,S,T,S,T,T,T,S,T,S,S,S,T,T,S,S,T,S,T,N,N,S,S,S,T,T,T,S,T,T,T,S,T,T,T,S,S,T,S,S,T,T,S,T,S,S,T,S,T,T,T,S,T,S,S,S,T,T,S,S,T,S,T,S,T,S,T,T,T,T,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,S,N,S,N,N,S,N,N,N,T,N,N,S,S,S,T,T,T,S,S,T,S,S,T,T,S,S,T,S,T,S,T,T,S,S,S,S,T,S,T,T,S,S,T,S,S,N,S,N,S,S,N,S,T,N,T,S,T,T,T,S,N,S,S,S,S,T,S,T,S,N,T,S,S,T,N,T,S,S,T,T,T,S,S,T,S,S,T,T,S,S,T,S,T,S,T,T,S,S,S,S,T,S,T,T,S,S,T,S,S,S,T,S,T,T,T,T,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,S,N,S,N,N,S,S,S,T,N,T,S,S,S,N,S,N,S,T,T,T,S,S,T,S,S,T,T,S,S,T,S,T,S,T,T,S,S,S,S,T,S,T,T,S,S,T,S,S,N,N,S,S,S,T,T,T,S,S,T,S,S,T,T,S,S,T,S,T,S,T,T,S,S,S,S,T,S,T,T,S,S,T,S,S,S,T,S,T,T,T,T,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,S,N,S,N,N,S,S,S,T,N,T,S,S,S,S,S,S,S,N,T,T,S,N,T,N,N,S,S,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,T,S,T,T,T,S,T,T,T,S,T,T,S,T,T,S,S,S,T,T,S,T,S,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,T,N,S,S,S,T,S,T,S,N,S,S,S,T,T,S,T,N,T,N,S,S,T,N,S,S,N,T,N]

factTL :: TokenList
factTL =
  [S,S,S,S,N,S,S,S,T,S,S,S,T,S,T,N,T,T,S,S,S,S,T,N,S,S,S,T,T,S,T,T,T,S,N,T,T,S,S,S,S,T,S,N,S,S,S,T,T,T,S,T,S,S,N,T,T,S,S,S,S,T,T,N,S,S,S,T,T,S,S,T,S,T,N,T,T,S,S,S,S,T,S,S,N,S,S,S,T,T,T,S,S,T,S,N,T,T,S,S,S,S,T,S,T,N,S,S,S,T,S,S,S,S,S,N,T,T,S,S,S,S,T,T,S,N,S,S,S,T,T,S,S,S,S,T,N,T,T,S,S,S,S,T,T,T,N,S,S,S,T,S,S,S,S,S,N,T,T,S,S,S,S,T,S,S,S,N,S,S,S,T,T,S,T,T,T,S,N,T,T,S,S,S,S,T,S,S,T,N,S,S,S,T,T,T,S,T,S,T,N,T,T,S,S,S,S,T,S,T,S,N,S,S,S,T,T,S,T,T,S,T,N,T,T,S,S,S,S,T,S,T,T,N,S,S,S,T,T,S,S,S,T,S,N,T,T,S,S,S,S,T,T,S,S,N,S,S,S,T,T,S,S,T,S,T,N,T,T,S,S,S,S,T,T,S,T,N,S,S,S,T,T,T,S,S,T,S,N,T,T,S,S,S,S,T,T,T,S,N,S,S,S,T,T,T,S,T,S,N,T,T,S,S,S,S,T,T,T,T,N,S,S,S,T,S,S,S,S,S,N,T,T,S,S,S,S,T,S,S,S,S,N,S,S,S,S,N,T,T,S,S,S,S,T,S,T,S,S,N,S,S,S,T,S,S,S,S,T,N,T,T,S,S,S,S,T,S,T,S,T,N,S,S,S,T,S,S,S,S,S,N,T,T,S,S,S,S,T,S,T,T,S,N,S,S,S,T,T,T,T,S,T,N,T,T,S,S,S,S,T,S,T,T,T,N,S,S,S,T,S,S,S,S,S,N,T,T,S,S,S,S,T,T,S,S,S,N,S,S,S,S,N,T,T,S,S,S,S,S,N,N,S,T,S,T,T,T,S,T,T,T,S,T,T,T,S,S,T,S,S,T,T,S,T,S,S,T,S,T,T,T,S,T,S,S,S,T,T,S,S,T,S,T,N,S,S,S,T,T,S,S,T,S,S,N,T,N,T,T,S,S,S,T,T,S,S,T,S,S,N,T,T,T,N,S,T,S,T,T,S,S,T,T,S,S,T,T,S,S,S,S,T,S,T,T,S,S,S,T,T,S,T,T,T,S,T,S,S,N,S,S,S,T,T,S,S,T,S,S,N,T,T,T,T,N,S,T,S,S,S,T,S,T,S,S,N,N,S,T,S,T,T,T,S,T,T,T,S,T,T,T,S,S,T,S,S,T,T,S,T,S,S,T,S,T,T,T,S,T,S,S,S,T,T,S,S,T,S,T,N,T,N,S,T,N,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,T,S,T,T,T,S,T,T,T,S,T,T,S,T,T,S,S,S,T,T,S,T,S,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,T,N,N,N,N,N,S,S,S,T,T,S,S,T,T,S,S,T,T,S,S,S,S,T,S,T,T,S,S,S,T,T,S,T,T,T,S,T,S,S,N,S,N,S,S,S,S,T,N,T,S,S,T,N,T,S,S,T,T,S,S,T,T,S,S,T,T,S,S,S,S,T,S,T,T,S,S,S,T,T,S,T,T,T,S,T,S,S,S,T,T,S,S,S,T,S,S,T,T,S,S,S,S,T,S,T,T,T,S,S,T,T,S,T,T,S,S,T,S,T,N,S,N,S,S,S,S,T,N,T,S,S,T,N,S,T,S,T,T,S,S,T,T,S,S,T,T,S,S,S,S,T,S,T,T,S,S,S,T,T,S,T,T,T,S,T,S,S,N,T,S,S,N,N,T,N,N,S,S,S,T,T,S,S,T,T,S,S,T,T,S,S,S,S,T,S,T,T,S,S,S,T,T,S,T,T,T,S,T,S,S,S,T,T,S,S,S,T,S,S,T,T,S,S,S,S,T,S,T,T,T,S,S,T,T,S,T,T,S,S,T,S,T,N,S,S,S,T,N,S,N,N,N,T,N,N,S,S,S,T,T,S,S,S,S,T,S,T,T,S,S,T,S,S,S,T,T,S,S,T,S,S,N,T,S,S,S,N,T,N,N,S,S,S,T,T,T,S,T,T,T,S,T,T,T,S,S,T,S,S,T,T,S,T,S,S,T,S,T,T,T,S,T,S,S,S,T,T,S,S,T,S,T,N,S,N,S,T,T,T,S,N,S,N,T,S,S,T,T,T,S,T,T,T,S,T,T,T,S,S,T,S,S,T,T,S,T,S,S,T,S,T,T,T,S,T,S,S,S,T,T,S,S,T,S,T,S,T,S,T,T,T,T,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,S,N,T,N,S,S,S,S,S,T,N,T,S,S,S,N,S,N,S,T,T,T,S,T,T,T,S,T,T,T,S,S,T,S,S,T,T,S,T,S,S,T,S,T,T,T,S,T,S,S,S,T,T,S,S,T,S,T,N,N,S,S,S,T,T,T,S,T,T,T,S,T,T,T,S,S,T,S,S,T,T,S,T,S,S,T,S,T,T,T,S,T,S,S,S,T,T,S,S,T,S,T,S,T,S,T,T,T,T,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,S,N,S,N,N,S,N,N,N,T,N,N,S,S,S,T,T,T,S,S,T,S,S,T,T,S,S,T,S,T,S,T,T,S,S,S,S,T,S,T,T,S,S,T,S,S,N,S,N,S,S,N,S,T,N,T,S,T,T,T,S,N,S,S,S,S,T,S,T,S,N,T,S,S,T,N,T,S,S,T,T,T,S,S,T,S,S,T,T,S,S,T,S,T,S,T,T,S,S,S,S,T,S,T,T,S,S,T,S,S,S,T,S,T,T,T,T,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,S,N,S,N,N,S,S,S,T,N,T,S,S,S,N,S,N,S,T,T,T,S,S,T,S,S,T,T,S,S,T,S,T,S,T,T,S,S,S,S,T,S,T,T,S,S,T,S,S,N,N,S,S,S,T,T,T,S,S,T,S,S,T,T,S,S,T,S,T,S,T,T,S,S,S,S,T,S,T,T,S,S,T,S,S,S,T,S,T,T,T,T,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,S,N,S,N,N,S,S,S,T,N,T,S,S,S,S,S,S,S,N,T,T,S,N,T,N,N,S,S,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,T,S,T,T,T,S,T,T,T,S,T,T,S,T,T,S,S,S,T,T,S,T,S,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,T,N,S,S,S,T,S,T,S,N,S,S,S,T,T,S,T,N,T,N,S,S,T,N,S,S,N,T,N]

hanoiTL :: TokenList
hanoiTL =
  [S,S,S,S,N,S,S,S,T,S,S,S,T,S,T,N,T,T,S,S,S,S,T,N,S,S,S,T,T,S,T,T,T,S,N,T,T,S,S,S,S,T,S,N,S,S,S,T,T,T,S,T,S,S,N,T,T,S,S,S,S,T,T,N,S,S,S,T,T,S,S,T,S,T,N,T,T,S,S,S,S,T,S,S,N,S,S,S,T,T,T,S,S,T,S,N,T,T,S,S,S,S,T,S,T,N,S,S,S,T,S,S,S,S,S,N,T,T,S,S,S,S,T,T,S,N,S,S,S,T,T,S,S,S,S,T,N,T,T,S,S,S,S,T,T,T,N,S,S,S,T,S,S,S,S,S,N,T,T,S,S,S,S,T,S,S,S,N,S,S,S,T,T,S,T,T,T,S,N,T,T,S,S,S,S,T,S,S,T,N,S,S,S,T,T,T,S,T,S,T,N,T,T,S,S,S,S,T,S,T,S,N,S,S,S,T,T,S,T,T,S,T,N,T,T,S,S,S,S,T,S,T,T,N,S,S,S,T,T,S,S,S,T,S,N,T,T,S,S,S,S,T,T,S,S,N,S,S,S,T,T,S,S,T,S,T,N,T,T,S,S,S,S,T,T,S,T,N,S,S,S,T,T,T,S,S,T,S,N,T,T,S,S,S,S,T,T,T,S,N,S,S,S,T,T,T,S,T,S,N,T,T,S,S,S,S,T,T,T,T,N,S,S,S,T,S,S,S,S,S,N,T,T,S,S,S,S,T,S,S,S,S,N,S,S,S,S,N,T,T,S,S,S,S,T,S,T,S,S,N,S,S,S,T,S,S,S,S,S,N,T,T,S,S,S,S,T,S,T,S,T,N,S,S,S,T,S,T,T,S,T,N,T,T,S,S,S,S,T,S,T,T,S,N,S,S,S,T,T,T,T,T,S,N,T,T,S,S,S,S,T,S,T,T,T,N,S,S,S,T,S,S,S,S,S,N,T,T,S,S,S,S,T,T,S,S,S,N,S,S,S,S,N,T,T,S,S,S,S,S,N,N,S,T,S,T,T,T,S,T,T,T,S,T,T,T,S,S,T,S,S,T,T,S,T,S,S,T,S,T,T,T,S,T,S,S,S,T,T,S,S,T,S,T,N,S,S,S,T,T,S,S,T,S,S,N,T,N,T,T,S,S,S,T,T,S,S,T,S,S,N,T,T,T,S,S,S,T,N,S,S,S,T,T,N,S,S,S,T,S,N,N,S,T,S,T,T,S,T,S,S,S,S,T,T,S,S,S,S,T,S,T,T,S,T,T,T,S,S,T,T,S,T,T,T,T,S,T,T,S,T,S,S,T,N,N,N,N,N,S,S,S,T,T,S,T,S,S,S,S,T,T,S,S,S,S,T,S,T,T,S,T,T,T,S,S,T,T,S,T,T,T,T,S,T,T,S,T,S,S,T,N,S,S,S,T,T,S,S,T,T,T,N,S,N,T,T,T,S,S,S,S,T,T,S,S,T,T,S,N,S,N,T,T,T,S,S,S,S,T,T,S,S,T,S,T,N,S,N,T,T,T,S,S,S,S,T,T,S,S,T,S,S,N,S,N,T,T,T,S,S,S,S,T,T,S,S,T,S,S,N,T,T,T,N,T,S,S,T,T,S,S,T,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,S,S,T,T,S,T,S,S,S,S,T,T,S,S,S,S,T,S,T,T,S,T,T,T,S,S,T,T,S,T,T,T,T,S,T,T,S,T,S,S,T,N,S,S,S,T,T,S,S,T,S,S,N,T,T,T,S,S,S,T,T,S,S,T,S,T,N,T,T,T,S,S,S,T,T,S,S,T,T,S,N,T,T,T,S,S,S,T,T,S,S,T,T,T,N,T,T,T,S,S,S,T,T,S,S,T,S,S,N,T,T,T,S,S,S,T,N,T,S,S,T,S,S,S,T,T,S,S,T,S,T,N,T,T,T,S,S,S,T,T,S,S,T,T,T,N,T,T,T,S,S,S,T,T,S,S,T,T,S,N,T,T,T,N,S,T,S,T,T,S,T,S,S,S,S,T,T,S,S,S,S,T,S,T,T,S,T,T,T,S,S,T,T,S,T,T,T,T,S,T,T,S,T,S,S,T,N,S,S,S,T,T,S,S,T,T,T,N,S,N,T,T,T,S,S,S,S,T,T,S,S,T,T,S,N,S,N,T,T,T,S,S,S,S,T,T,S,S,T,S,T,N,S,N,T,T,T,S,S,S,S,T,T,S,S,T,S,S,N,S,N,T,T,T,S,S,S,S,T,T,S,S,T,S,T,N,T,T,T,T,N,S,T,S,S,S,T,S,T,S,S,N,N,S,T,S,T,T,T,S,T,T,T,S,T,T,T,S,S,T,S,S,T,T,S,T,S,S,T,S,T,T,T,S,T,S,S,S,T,T,S,S,T,S,T,N,S,S,S,T,T,S,S,T,T,S,N,T,T,T,T,N,S,T,N,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,T,S,T,T,T,S,T,T,T,S,T,T,S,T,T,S,S,S,T,T,S,T,S,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,T,N,S,S,S,T,T,S,S,T,S,S,N,T,T,T,S,S,S,T,T,S,S,T,S,T,N,T,T,T,S,S,S,T,T,S,S,T,T,S,N,T,T,T,S,S,S,T,T,S,S,T,T,T,N,T,T,T,S,S,S,T,T,S,S,T,S,S,N,T,T,T,S,S,S,T,N,T,S,S,T,S,S,S,T,T,S,S,T,T,T,N,T,T,T,S,S,S,T,T,S,S,T,T,S,N,T,T,T,S,S,S,T,T,S,S,T,S,T,N,T,T,T,N,S,T,S,T,T,S,T,S,S,S,S,T,T,S,S,S,S,T,S,T,T,S,T,T,T,S,S,T,T,S,T,T,T,T,S,T,T,S,T,S,S,T,N,S,S,S,T,T,S,S,T,T,T,N,S,N,T,T,T,S,S,S,S,T,T,S,S,T,T,S,N,S,N,T,T,T,S,S,S,S,T,T,S,S,T,S,T,N,S,N,T,T,T,S,S,S,S,T,T,S,S,T,S,S,N,S,N,T,T,T,S,N,S,S,S,T,T,S,S,T,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,S,S,T,T,S,T,S,S,S,S,T,T,S,S,S,S,T,S,T,T,S,T,T,T,S,S,T,T,S,T,T,T,T,S,T,T,S,T,S,S,T,N,N,T,N,N,S,S,S,T,T,S,S,S,S,T,S,T,T,S,S,T,S,S,S,T,T,S,S,T,S,S,N,T,S,S,S,N,T,N,N,S,S,S,T,T,T,S,T,T,T,S,T,T,T,S,S,T,S,S,T,T,S,T,S,S,T,S,T,T,T,S,T,S,S,S,T,T,S,S,T,S,T,N,S,N,S,T,T,T,S,N,S,N,T,S,S,T,T,T,S,T,T,T,S,T,T,T,S,S,T,S,S,T,T,S,T,S,S,T,S,T,T,T,S,T,S,S,S,T,T,S,S,T,S,T,S,T,S,T,T,T,T,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,S,N,T,N,S,S,S,S,S,T,N,T,S,S,S,N,S,N,S,T,T,T,S,T,T,T,S,T,T,T,S,S,T,S,S,T,T,S,T,S,S,T,S,T,T,T,S,T,S,S,S,T,T,S,S,T,S,T,N,N,S,S,S,T,T,T,S,T,T,T,S,T,T,T,S,S,T,S,S,T,T,S,T,S,S,T,S,T,T,T,S,T,S,S,S,T,T,S,S,T,S,T,S,T,S,T,T,T,T,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,S,N,S,N,N,S,N,N,N,T,N,N,S,S,S,T,T,T,S,S,T,S,S,T,T,S,S,T,S,T,S,T,T,S,S,S,S,T,S,T,T,S,S,T,S,S,N,S,N,S,S,N,S,T,N,T,S,T,T,T,S,N,S,S,S,S,T,S,T,S,N,T,S,S,T,N,T,S,S,T,T,T,S,S,T,S,S,T,T,S,S,T,S,T,S,T,T,S,S,S,S,T,S,T,T,S,S,T,S,S,S,T,S,T,T,T,T,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,S,N,S,N,N,S,S,S,T,N,T,S,S,S,N,S,N,S,T,T,T,S,S,T,S,S,T,T,S,S,T,S,T,S,T,T,S,S,S,S,T,S,T,T,S,S,T,S,S,N,N,S,S,S,T,T,T,S,S,T,S,S,T,T,S,S,T,S,T,S,T,T,S,S,S,S,T,S,T,T,S,S,T,S,S,S,T,S,T,T,T,T,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,S,N,S,N,N,S,S,S,T,N,T,S,S,S,S,S,S,S,N,T,T,S,N,T,N,N,S,S,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,T,S,T,T,T,S,T,T,T,S,T,T,S,T,T,S,S,S,T,T,S,T,S,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,T,N,S,S,S,T,S,T,S,N,S,S,S,T,T,S,T,N,T,N,S,S,T,N,S,S,N,T,N]

helloWorldTL :: TokenList
helloWorldTL =
  [S,S,S,T,S,S,T,S,S,S,N
  ,T,N,S,S
  ,S,S,S,T,T,S,S,T,S,T,N
  ,T,N,S,S
  ,S,S,S,T,T,S,T,T,S,S,N
  ,T,N,S,S
  ,S,S,S,T,T,S,T,T,S,S,N
  ,T,N,S,S
  ,S,S,S,T,T,S,T,T,T,T,N
  ,T,N,S,S
  ,S,S,S,T,S,T,T,S,S,N
  ,T,N,S,S
  ,S,S,S,T,S,S,S,S,S,N
  ,T,N,S,S
  ,S,S,S,T,T,T,S,T,T,T,N
  ,T,N,S,S
  ,S,S,S,T,T,S,T,T,T,T,N
  ,T,N,S,S
  ,S,S,S,T,T,T,S,S,T,S,N
  ,T,N,S,S
  ,S,S,S,T,T,S,T,T,S,S,N
  ,T,N,S,S
  ,S,S,S,T,T,S,S,T,S,S,N
  ,T,N,S,S
  ,N,N,N
  ]

hWorldTL :: TokenList
hWorldTL =
  [S,S,S,S,N,S,S,S,T,S,S,T,S,S,S,N,T,T,S,S,S,S,T,N,S,S,S,T,T,S,S,T,S,T,N,T,T,S,S,S,S,T,S,N,S,S,S,T,T,S,T,T,S,S,N,T,T,S,S,S,S,T,T,N,S,S,S,T,T,S,T,T,S,S,N,T,T,S,S,S,S,T,S,S,N,S,S,S,T,T,S,T,T,T,T,N,T,T,S,S,S,S,T,S,T,N,S,S,S,T,S,T,T,S,S,N,T,T,S,S,S,S,T,T,S,N,S,S,S,T,S,S,S,S,S,N,T,T,S,S,S,S,T,T,T,N,S,S,S,T,T,T,S,T,T,T,N,T,T,S,S,S,S,T,S,S,S,N,S,S,S,T,T,S,T,T,T,T,N,T,T,S,S,S,S,T,S,S,T,N,S,S,S,T,T,T,S,S,T,S,N,T,T,S,S,S,S,T,S,T,S,N,S,S,S,T,T,S,T,T,S,S,N,T,T,S,S,S,S,T,S,T,T,N,S,S,S,T,T,S,S,T,S,S,N,T,T,S,S,S,S,T,T,S,S,N,S,S,S,T,S,S,S,S,S,N,T,T,S,S,S,S,T,T,S,T,N,S,S,S,T,T,S,T,T,T,T,N,T,T,S,S,S,S,T,T,T,S,N,S,S,S,T,T,S,S,T,T,S,N,T,T,S,S,S,S,T,T,T,T,N,S,S,S,T,S,S,S,S,S,N,T,T,S,S,S,S,T,S,S,S,S,N,S,S,S,T,T,T,S,S,T,T,N,T,T,S,S,S,S,T,S,S,S,T,N,S,S,S,T,T,T,S,S,S,S,N,T,T,S,S,S,S,T,S,S,T,S,N,S,S,S,T,T,S,S,S,S,T,N,T,T,S,S,S,S,T,S,S,T,T,N,S,S,S,T,T,S,S,S,T,T,N,T,T,S,S,S,S,T,S,T,S,S,N,S,S,S,T,T,S,S,T,S,T,N,T,T,S,S,S,S,T,S,T,S,T,N,S,S,S,T,T,T,S,S,T,T,N,T,T,S,S,S,S,T,S,T,T,S,N,S,S,S,T,S,S,S,S,T,N,T,T,S,S,S,S,T,S,T,T,T,N,S,S,S,S,N,T,T,S,S,S,S,S,N,N,S,T,S,T,T,T,S,T,T,T,S,T,T,T,S,S,T,S,S,T,T,S,T,S,S,T,S,T,T,T,S,T,S,S,S,T,T,S,S,T,S,T,N,N,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,T,S,T,T,T,S,T,T,T,S,T,T,S,T,T,S,S,S,T,T,S,T,S,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,T,N,N,N,N,N,S,S,S,T,T,S,S,S,S,T,S,T,T,S,S,T,S,S,S,T,T,S,S,T,S,S,N,T,S,S,S,N,T,N,N,S,S,S,T,T,T,S,T,T,T,S,T,T,T,S,S,T,S,S,T,T,S,T,S,S,T,S,T,T,T,S,T,S,S,S,T,T,S,S,T,S,T,N,S,N,S,T,T,T,S,N,S,N,T,S,S,T,T,T,S,T,T,T,S,T,T,T,S,S,T,S,S,T,T,S,T,S,S,T,S,T,T,T,S,T,S,S,S,T,T,S,S,T,S,T,S,T,S,T,T,T,T,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,S,N,T,N,S,S,S,S,S,T,N,T,S,S,S,N,S,N,S,T,T,T,S,T,T,T,S,T,T,T,S,S,T,S,S,T,T,S,T,S,S,T,S,T,T,T,S,T,S,S,S,T,T,S,S,T,S,T,N,N,S,S,S,T,T,T,S,T,T,T,S,T,T,T,S,S,T,S,S,T,T,S,T,S,S,T,S,T,T,T,S,T,S,S,S,T,T,S,S,T,S,T,S,T,S,T,T,T,T,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,S,N,S,N,N,S,N,N,N,T,N,N,S,S,S,T,T,T,S,S,T,S,S,T,T,S,S,T,S,T,S,T,T,S,S,S,S,T,S,T,T,S,S,T,S,S,N,S,N,S,S,N,S,T,N,T,S,T,T,T,S,N,S,S,S,S,T,S,T,S,N,T,S,S,T,N,T,S,S,T,T,T,S,S,T,S,S,T,T,S,S,T,S,T,S,T,T,S,S,S,S,T,S,T,T,S,S,T,S,S,S,T,S,T,T,T,T,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,S,N,S,N,N,S,S,S,T,N,T,S,S,S,N,S,N,S,T,T,T,S,S,T,S,S,T,T,S,S,T,S,T,S,T,T,S,S,S,S,T,S,T,T,S,S,T,S,S,N,N,S,S,S,T,T,T,S,S,T,S,S,T,T,S,S,T,S,T,S,T,T,S,S,S,S,T,S,T,T,S,S,T,S,S,S,T,S,T,T,T,T,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,S,N,S,N,N,S,S,S,T,N,T,S,S,S,S,S,S,S,N,T,T,S,N,T,N,N,S,S,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,T,S,T,T,T,S,T,T,T,S,T,T,S,T,T,S,S,S,T,T,S,T,S,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,T,N,S,S,S,T,S,T,S,N,S,S,S,T,T,S,T,N,T,N,S,S,T,N,S,S,N,T,N]

locTestTL :: TokenList
locTestTL =
  [S,S,S,S,N,S,S,S,T,S,S,S,T,S,T,N,T,T,S,S,S,S,T,N,S,S,S,T,T,S,T,T,T,S,N,T,T,S,S,S,S,T,S,N,S,S,S,T,T,T,S,T,S,S,N,T,T,S,S,S,S,T,T,N,S,S,S,T,T,S,S,T,S,T,N,T,T,S,S,S,S,T,S,S,N,S,S,S,T,T,T,S,S,T,S,N,T,T,S,S,S,S,T,S,T,N,S,S,S,T,S,S,S,S,S,N,T,T,S,S,S,S,T,T,S,N,S,S,S,T,T,S,S,S,S,T,N,T,T,S,S,S,S,T,T,T,N,S,S,S,T,S,S,S,S,S,N,T,T,S,S,S,S,T,S,S,S,N,S,S,S,T,T,S,T,T,T,S,N,T,T,S,S,S,S,T,S,S,T,N,S,S,S,T,T,T,S,T,S,T,N,T,T,S,S,S,S,T,S,T,S,N,S,S,S,T,T,S,T,T,S,T,N,T,T,S,S,S,S,T,S,T,T,N,S,S,S,T,T,S,S,S,T,S,N,T,T,S,S,S,S,T,T,S,S,N,S,S,S,T,T,S,S,T,S,T,N,T,T,S,S,S,S,T,T,S,T,N,S,S,S,T,T,T,S,S,T,S,N,T,T,S,S,S,S,T,T,T,S,N,S,S,S,T,T,T,S,T,S,N,T,T,S,S,S,S,T,T,T,T,N,S,S,S,T,S,S,S,S,S,N,T,T,S,S,S,S,T,S,S,S,S,N,S,S,S,S,N,T,T,S,S,S,S,S,N,N,S,T,S,T,T,T,S,T,T,T,S,T,T,T,S,S,T,S,S,T,T,S,T,S,S,T,S,T,T,T,S,T,S,S,S,T,T,S,S,T,S,T,N,S,S,S,T,T,S,S,T,S,S,N,T,N,T,T,S,S,S,S,N,N,S,T,S,T,T,T,S,T,T,T,S,T,T,T,S,S,T,S,S,T,T,S,T,S,S,T,S,T,T,T,S,T,S,S,S,T,T,S,S,T,S,T,N,S,S,S,T,T,S,S,T,S,T,N,T,N,T,T,S,S,S,T,T,S,S,T,S,S,N,T,T,T,S,S,S,T,T,S,S,T,S,T,N,T,T,T,N,S,T,S,T,T,S,T,T,S,S,S,T,T,S,T,T,T,T,S,T,T,S,S,S,T,T,S,T,T,T,S,T,S,S,S,T,T,S,S,T,S,T,S,T,T,T,S,S,T,T,S,T,T,T,S,T,S,S,N,T,N,S,T,N,N,N,N,S,S,S,T,T,S,T,T,S,S,S,T,T,S,T,T,T,T,S,T,T,S,S,S,T,T,S,T,T,T,S,T,S,S,S,T,T,S,S,T,S,T,S,T,T,T,S,S,T,T,S,T,T,T,S,T,S,S,N,S,T,S,S,S,N,S,T,S,S,T,S,N,T,S,S,S,S,T,N,S,T,S,N,N,T,N,N,S,S,S,T,T,S,S,S,S,T,S,T,T,S,S,T,S,S,S,T,T,S,S,T,S,S,N,T,S,S,S,N,T,N,N,S,S,S,T,T,T,S,T,T,T,S,T,T,T,S,S,T,S,S,T,T,S,T,S,S,T,S,T,T,T,S,T,S,S,S,T,T,S,S,T,S,T,N,S,N,S,T,T,T,S,N,S,N,T,S,S,T,T,T,S,T,T,T,S,T,T,T,S,S,T,S,S,T,T,S,T,S,S,T,S,T,T,T,S,T,S,S,S,T,T,S,S,T,S,T,S,T,S,T,T,T,T,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,S,N,T,N,S,S,S,S,S,T,N,T,S,S,S,N,S,N,S,T,T,T,S,T,T,T,S,T,T,T,S,S,T,S,S,T,T,S,T,S,S,T,S,T,T,T,S,T,S,S,S,T,T,S,S,T,S,T,N,N,S,S,S,T,T,T,S,T,T,T,S,T,T,T,S,S,T,S,S,T,T,S,T,S,S,T,S,T,T,T,S,T,S,S,S,T,T,S,S,T,S,T,S,T,S,T,T,T,T,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,S,N,S,N,N,S,N,N,N,T,N,N,S,S,S,T,T,T,S,S,T,S,S,T,T,S,S,T,S,T,S,T,T,S,S,S,S,T,S,T,T,S,S,T,S,S,N,S,N,S,S,N,S,T,N,T,S,T,T,T,S,N,S,S,S,S,T,S,T,S,N,T,S,S,T,N,T,S,S,T,T,T,S,S,T,S,S,T,T,S,S,T,S,T,S,T,T,S,S,S,S,T,S,T,T,S,S,T,S,S,S,T,S,T,T,T,T,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,S,N,S,N,N,S,S,S,T,N,T,S,S,S,N,S,N,S,T,T,T,S,S,T,S,S,T,T,S,S,T,S,T,S,T,T,S,S,S,S,T,S,T,T,S,S,T,S,S,N,N,S,S,S,T,T,T,S,S,T,S,S,T,T,S,S,T,S,T,S,T,T,S,S,S,S,T,S,T,T,S,S,T,S,S,S,T,S,T,T,T,T,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,S,N,S,N,N,S,S,S,T,N,T,S,S,S,S,S,S,S,N,T,T,S,N,T,N,N,S,S,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,T,S,T,T,T,S,T,T,T,S,T,T,S,T,T,S,S,S,T,T,S,T,S,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,T,N,S,S,S,T,S,T,S,N,S,S,S,T,T,S,T,N,T,N,S,S,T,N,S,S,N,T,N]

nameTL :: TokenList
nameTL =
  [S,S,S,S,N,S,S,S,T,S,T,S,S,S,S,N,T,T,S,S,S,S,T,N,S,S,S,T,T,S,T,T,S,S,N,T,T,S,S,S,S,T,S,N,S,S,S,T,T,S,S,T,S,T,N,T,T,S,S,S,S,T,T,N,S,S,S,T,T,S,S,S,S,T,N,T,T,S,S,S,S,T,S,S,N,S,S,S,T,T,T,S,S,T,T,N,T,T,S,S,S,S,T,S,T,N,S,S,S,T,T,S,S,T,S,T,N,T,T,S,S,S,S,T,T,S,N,S,S,S,T,S,S,S,S,S,N,T,T,S,S,S,S,T,T,T,N,S,S,S,T,T,S,S,T,S,T,N,T,T,S,S,S,S,T,S,S,S,N,S,S,S,T,T,S,T,T,T,S,N,T,T,S,S,S,S,T,S,S,T,N,S,S,S,T,T,T,S,T,S,S,N,T,T,S,S,S,S,T,S,T,S,N,S,S,S,T,T,S,S,T,S,T,N,T,T,S,S,S,S,T,S,T,T,N,S,S,S,T,T,T,S,S,T,S,N,T,T,S,S,S,S,T,T,S,S,N,S,S,S,T,S,S,S,S,S,N,T,T,S,S,S,S,T,T,S,T,N,S,S,S,T,T,T,T,S,S,T,N,T,T,S,S,S,S,T,T,T,S,N,S,S,S,T,T,S,T,T,T,T,N,T,T,S,S,S,S,T,T,T,T,N,S,S,S,T,T,T,S,T,S,T,N,T,T,S,S,S,S,T,S,S,S,S,N,S,S,S,T,T,T,S,S,T,S,N,T,T,S,S,S,S,T,S,S,S,T,N,S,S,S,T,S,S,S,S,S,N,T,T,S,S,S,S,T,S,S,T,S,N,S,S,S,T,T,S,T,T,T,S,N,T,T,S,S,S,S,T,S,S,T,T,N,S,S,S,T,T,S,S,S,S,T,N,T,T,S,S,S,S,T,S,T,S,S,N,S,S,S,T,T,S,T,T,S,T,N,T,T,S,S,S,S,T,S,T,S,T,N,S,S,S,T,T,S,S,T,S,T,N,T,T,S,S,S,S,T,S,T,T,S,N,S,S,S,T,T,T,S,T,S,N,T,T,S,S,S,S,T,S,T,T,T,N,S,S,S,T,S,S,S,S,S,N,T,T,S,S,S,S,T,T,S,S,S,N,S,S,S,S,N,T,T,S,S,S,S,T,T,T,T,S,N,S,S,S,T,S,S,T,S,S,S,N,T,T,S,S,S,S,T,T,T,T,T,N,S,S,S,T,T,S,S,T,S,T,N,T,T,S,S,S,S,T,S,S,S,S,S,N,S,S,S,T,T,S,T,T,S,S,N,T,T,S,S,S,S,T,S,S,S,S,T,N,S,S,S,T,T,S,T,T,S,S,N,T,T,S,S,S,S,T,S,S,S,T,S,N,S,S,S,T,T,S,T,T,T,T,N,T,T,S,S,S,S,T,S,S,S,T,T,N,S,S,S,T,S,S,S,S,S,N,T,T,S,S,S,S,T,S,S,T,S,S,N,S,S,S,S,N,T,T,S,S,S,S,S,N,N,S,T,S,T,T,T,S,T,T,T,S,T,T,T,S,S,T,S,S,T,T,S,T,S,S,T,S,T,T,T,S,T,S,S,S,T,T,S,S,T,S,T,N,S,S,S,T,T,S,S,T,S,S,N,N,S,T,S,T,T,T,S,S,T,S,S,T,T,S,S,T,S,T,S,T,T,S,S,S,S,T,S,T,T,S,S,T,S,S,N,S,S,S,T,T,T,T,S,N,N,S,T,S,T,T,T,S,T,T,T,S,T,T,T,S,S,T,S,S,T,T,S,T,S,S,T,S,T,T,T,S,T,S,S,S,T,T,S,S,T,S,T,N,S,S,S,T,T,S,S,T,S,S,N,N,S,T,S,T,T,T,S,T,T,T,S,T,T,T,S,S,T,S,S,T,T,S,T,S,S,T,S,T,T,T,S,T,S,S,S,T,T,S,S,T,S,T,N,N,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,T,S,T,T,T,S,T,T,T,S,T,T,S,T,T,S,S,S,T,T,S,T,S,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,T,N,N,N,N,N,S,S,S,T,T,S,S,S,S,T,S,T,T,S,S,T,S,S,S,T,T,S,S,T,S,S,N,T,S,S,S,N,T,N,N,S,S,S,T,T,T,S,T,T,T,S,T,T,T,S,S,T,S,S,T,T,S,T,S,S,T,S,T,T,T,S,T,S,S,S,T,T,S,S,T,S,T,N,S,N,S,T,T,T,S,N,S,N,T,S,S,T,T,T,S,T,T,T,S,T,T,T,S,S,T,S,S,T,T,S,T,S,S,T,S,T,T,T,S,T,S,S,S,T,T,S,S,T,S,T,S,T,S,T,T,T,T,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,S,N,T,N,S,S,S,S,S,T,N,T,S,S,S,N,S,N,S,T,T,T,S,T,T,T,S,T,T,T,S,S,T,S,S,T,T,S,T,S,S,T,S,T,T,T,S,T,S,S,S,T,T,S,S,T,S,T,N,N,S,S,S,T,T,T,S,T,T,T,S,T,T,T,S,S,T,S,S,T,T,S,T,S,S,T,S,T,T,T,S,T,S,S,S,T,T,S,S,T,S,T,S,T,S,T,T,T,T,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,S,N,S,N,N,S,N,N,N,T,N,N,S,S,S,T,T,T,S,S,T,S,S,T,T,S,S,T,S,T,S,T,T,S,S,S,S,T,S,T,T,S,S,T,S,S,N,S,N,S,S,N,S,T,N,T,S,T,T,T,S,N,S,S,S,S,T,S,T,S,N,T,S,S,T,N,T,S,S,T,T,T,S,S,T,S,S,T,T,S,S,T,S,T,S,T,T,S,S,S,S,T,S,T,T,S,S,T,S,S,S,T,S,T,T,T,T,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,S,N,S,N,N,S,S,S,T,N,T,S,S,S,N,S,N,S,T,T,T,S,S,T,S,S,T,T,S,S,T,S,T,S,T,T,S,S,S,S,T,S,T,T,S,S,T,S,S,N,N,S,S,S,T,T,T,S,S,T,S,S,T,T,S,S,T,S,T,S,T,T,S,S,S,S,T,S,T,T,S,S,T,S,S,S,T,S,T,T,T,T,T,S,T,T,S,S,T,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,S,N,S,N,N,S,S,S,T,N,T,S,S,S,S,S,S,S,N,T,T,S,N,T,N,N,S,S,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,T,S,T,T,T,S,T,T,T,S,T,T,S,T,T,S,S,S,T,T,S,T,S,S,T,S,T,T,S,T,T,T,S,S,T,T,S,S,T,S,T,N,S,S,S,T,S,T,S,N,S,S,S,T,T,S,T,N,T,N,S,S,T,N,S,S,N,T,N]

truthMachineTL :: TokenList
truthMachineTL =
  [S,S,S,N
  ,S,N,S
  ,T,N,T,T
  ,T,T,T
  ,N,T,S,S,N
  ,N,S,S,T,N
  ,S,S,S,T,N
  ,T,N,S,T
  ,N,S,N,T,N
  ,N,S,S,S,N
  ,S,S,S,N
  ,T,N,S,T
  ,N,N,N
  ]

--------------------------------------------------------------------------------

catIL :: InstructionList
catIL =
  [ Mark "0"
  , Const 1, InputChar
  , Const 1, Load, OutputChar
  , Const 1
  , Branch EZ "1"
  , Jump "0"

  , Mark "1"
  , End
  ]

helloWorldIL :: InstructionList
helloWorldIL =
  [ Const 72, OutputChar
  , Const 101, OutputChar
  , Const 108, OutputChar
  , Const 108, OutputChar
  , Const 111,OutputChar
  , Const 44, OutputChar
  , Const 32, OutputChar
  , Const 119, OutputChar
  , Const 111, OutputChar
  , Const 114, OutputChar
  , Const 108, OutputChar
  , Const 100, OutputChar
  , End
  ]

truthMachineIL :: InstructionList
truthMachineIL =
  [ Const 0
  , Dup
  , InputNum
  , Load
  , Branch EZ "0"

  , Mark "1"
  , Const 1, OutputNum
  , Jump "1"

  , Mark "0"
  , Const 0, OutputNum
  , End
  ]

----

calcO :: Output
calcO = "Enter some numbers, then -1 to finish\r\nNumber:Total is 0\r\n"

countO :: Output
countO = "1\n2\n3\n4\n5\n6\n7\n8\n9\n10\n"

factO :: Output
factO = "Enter a number: 10! = 3628800\r\n"

hanoiO :: Output
hanoiO = "Enter a number: 1 -> 3\r\n"

hWorldO :: Output
hWorldO = "Hello, world of spaces!\r\n"

locTestO :: Output
locTestO = "Enter a number: Enter a number: 3"

nameO :: Output
nameO = "Please enter your name: Hello WriteOnly\n\r\n"
