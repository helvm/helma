module HelVM.HelMA.Automata.WhiteSpace.EvaluatorSpecData where

import           HelVM.HelMA.Automata.WhiteSpace.Instruction
import           HelVM.HelMA.Automata.WhiteSpace.Token

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
  , Liter 1 , InputChar
  , Liter 1 , Load , OutputChar
  , Liter 1
  , Branch EZ "1"
  , Jump "0"

  , Mark "1"
  , End
  ]

helloWorldIL :: InstructionList
helloWorldIL =
  [ Liter 72 , OutputChar
  , Liter 101 , OutputChar
  , Liter 108 , OutputChar
  , Liter 108 , OutputChar
  , Liter 111,OutputChar
  , Liter 44 , OutputChar
  , Liter 32 , OutputChar
  , Liter 119 , OutputChar
  , Liter 111 , OutputChar
  , Liter 114 , OutputChar
  , Liter 108 , OutputChar
  , Liter 100 , OutputChar
  , End
  ]

truthMachineIL :: InstructionList
truthMachineIL =
  [ Liter 0
  , Dup
  , InputNum
  , Load
  , Branch EZ "0"

  , Mark "1"
  , Liter 1 , OutputNum
  , Jump "1"

  , Mark "0"
  , Liter 0 , OutputNum
  , End
  ]
