module HelVM.HelMA.Automata.ETA.EvaluatorSpecData where

import HelVM.HelMA.Automata.ETA.Token

helloTL :: TokenList
helloTL =
  [N,O,H,E,A,T,H,E,O,E,T,A,I,T,T,E,N,I,E,T,A,O,R
  ,R
  ,N,I,I,E,R
  ,N,S,A,E,N,A,R
  ,T,S,E,R
  ,N,A,T,O,E,N,A,T,O,E,N,A,H,O,E,R
  ,N,T,O,A,E,N,E,S,R
  ,O,O,O,O,O,O,O,R
  ,N,T,O,E,R
  ,N,I,N,E,N,A,H,A,E,N,A,T,O,E,R
  ,N,A,A,A,E,N,A,T,S,E,N,A,O,H,E,R
  ,O,O,O,O,O,O,O,R]

hello2TL :: TokenList
hello2TL =
  [N,E,N,T,O,E,N,I,N,E,N,A,H,A,E,N,A,T,O,E,N,A,A,A,E,N,A,T,S,E,N,A,O,H,E,N,I,I,E,R
  ,N,S,A,E,N,A,T,S,E,N,A,T,O,E,N,A,T,O,E,N,A,H,O,E,N,T,O,A,E,A,N,T,E,N,I,E,T,R
  ,N,T,E,N,E,T,A,N,O,T,E,T,I,S,T,O,O,E,A,T,I,N,S,S,T,E,S,R
  ,N,T,E,H,N,E,H,A,N,E,N,T,E,S,S,T,R
  ,S,N,T,I,E,N,T,E,H,T,R
  ,O,N,T,H,E,N,I,E,T,S,S,I,A,N,R]

pipTL :: TokenList
pipTL = [I,R,N,E,H,R,N,E,N,T,E,S,S,R,N,T,H,E,T,R,N,E,N,T,E,H,T,R,N,T,E,N,E,T,R,O,R,N,T,E,N,T,E,T,R]

pip2TL :: TokenList
pip2TL = [I,N,E,H,N,E,N,T,E,S,S,N,A,E,T,N,E,N,T,E,H,T,N,T,E,N,E,T,R,O,N,T,E,N,T,E,T,R]

factTL :: TokenList
factTL = [A,N,T,E,N,A,H,E,T,R,A,N,T,E,N,N,E,T,R,A,N,T,E,N,I,I,E,T,R,N,T,O,E,O,N,T,E,N,E,T,R,N,T,E,H,R,N,E,H,R,N,T,E,S,A,N,E,N,T,E,S,S,T,R,N,T,E,N,T,S,E,T,R,N,E,H,R,N,T,E,S,R,A,N,T,E,N,N,E,T,R,A,N,T,E,N,S,H,E,T,R,N,T,E,N,A,E,H,T,R,I,R,N,E,H,N,I,I,E,S,R,A,N,E,N,T,E,S,S,T,R,N,E,N,T,E,T,N,A,H,E,T,R,N,E,N,T,E,H,R,N,S,S,E,S,R,N,T,E,H,R,N,T,O,E,A,N,T,E,N,S,H,E,T,S,T,H,E,A,E,T,I,T,E,R,N,E,N,T,E,H,R,S,S,R,I,R,N,E,H,N,I,I,E,S,N,O,S,E,T,R,N,T,E,N,I,A,E,T,R,N,E,H,N,T,O,E,S,N,I,T,E,T,R,N,T,E,N,I,A,E,T,R,N,E,H,N,E,N,T,E,S,S,N,A,N,E,T,R,N,E,N,T,E,H,T,R,N,T,E,N,A,E,H,T,R,N,E,N,A,E,H,R,N,E,H,R,A,N,E,N,T,E,S,S,T,R,S,N,T,E,N,N,N,E,T,R,N,T,O,E,E,R,N,E,N,S,S,E,S,S,R,N,T,E,H,R,N,T,E,N,I,N,E,T,R,A,N,T,E,N,T,T,A,E,T,R,N,T,E,N,T,E,H,T,R,N,A,E,H,N,A,E,H,R,N,E,R,N,A,E,H,R,N,A,E,H,R,N,E,H,R,A,N,E,N,T,E,S,S,T,R,N,T,E,N,T,T,H,E,T,R,N,T,E,S,R,N,A,E,H,R,N,E,N,E,N,O,E,S,H,R,S,S,R,N,A,E,H,R,N,A,E,H,R,N,T,E,N,S,I,E,T,R,N,T,E,H,T,R,N,T,E,N,A,E,H,T,R,N,T,E,H,R,N,E,H,R,A,N,E,N,T,E,S,S,T,R,S,N,T,E,N,T,E,H,T,R,O,R,N,T,E,N,T,T,A,E,T,R]

bottlesTL :: TokenList
bottlesTL = [N,A,S,T,E,N,I,I,E,T,O,N,O,T,T,E,S,E,T,A,I,T,T,E,N,I,E,T,A,O,R,N,T,E,H,N,T,E,A,T,S,A,E,I,O,E,I,T,S,H,A,A,T,E,I,S,T,I,O,O,R,N,E,H,N,T,E,A,T,S,E,S,T,S,I,T,S,N,O,T,S,H,A,H,O,T,I,E,A,R,A,N,E,N,T,E,S,S,T,R,S,N,T,E,N,T,E,H,T,R,O,R,N,T,E,N,A,E,T,R,N,E,N,A,E,H,R,N,E,H,A,N,E,N,T,E,S,S,T,R,N,S,S,E,O,T,N,T,E,N,T,E,H,T,R,N,E,H,R,A,N,E,N,T,E,S,S,T,R,S,N,T,E,N,A,I,E,T,R,N,T,O,E,E,R,N,E,N,S,S,E,S,S,R,N,T,E,H,R,N,T,E,N,T,I,E,T,A,N,T,I,E,A,N,H,A,T,S,I,T,H,A,T,H,E,N,T,E,H,S,T,R,A,N,T,E,N,A,E,T,R,N,T,E,N,T,E,H,T,R,N,T,E,H,R,A,N,T,E,N,T,T,E,T,R,N,E,N,A,A,A,E,N,A,H,O,E,N,A,H,O,E,N,A,H,H,E,N,I,I,E,N,A,H,I,E,N,A,T,S,E,N,I,I,E,N,T,E,A,T,N,O,O,T,I,I,S,A,T,I,O,N,R,N,A,A,O,E,N,A,H,O,E,N,A,T,O,E,N,A,A,I,E,N,A,A,I,E,N,A,T,S,E,N,A,H,H,E,N,I,I,E,N,T,E,A,T,N,O,O,T,I,I,S,A,T,I,O,N,R,A,N,T,E,N,A,E,T,R,N,T,E,N,T,E,H,T,R,N,T,E,H,R,A,N,T,E,N,A,S,E,T,R,N,E,N,A,T,O,E,N,A,T,O,E,N,T,S,S,E,N,A,O,H,E,N,I,I,E,N,A,H,O,E,N,T,E,A,T,N,O,O,T,I,I,S,A,T,I,O,N,R,N,A,H,S,E,N,A,A,I,E,N,I,I,E,N,A,T,N,E,N,A,T,S,E,N,I,I,E,N,T,E,A,T,N,O,O,T,I,I,S,A,T,I,O,N,R,A,N,T,E,N,A,E,T,R,N,T,E,N,T,E,H,T,I,T,N,E,E,S,A,T,E,I,N,O,A,S,I,O,N,A,E,E,I,N,R,N,A,H,T,E,R,N,E,H,A,N,T,E,N,O,N,E,T,R,N,S,A,E,O,N,I,I,E,O,R,N,E,H,A,N,T,E,N,A,S,E,T,R,N,T,O,E,O,R,N,E,N,T,O,E,N,A,H,A,E,N,A,T,N,E,N,A,A,N,E,N,A,T,S,E,R,N,A,A,A,E,N,T,S,S,E,N,I,I,E,N,A,A,I,E,N,A,T,H,E,N,I,I,E,R,N,A,A,O,E,N,A,A,O,E,N,T,S,S,E,N,A,A,H,E,N,I,I,E,N,S,A,E,R,N,A,T,N,E,N,A,O,H,E,N,A,T,S,E,N,A,H,A,E,N,I,I,E,N,A,H,O,E,R,N,A,T,N,E,N,A,T,S,E,N,I,I,E,N,A,H,O,E,N,A,T,A,E,N,T,S,S,E,N,T,N,H,E,R,A,N,T,E,N,A,E,T,R,N,T,E,S,R,N,E,H,A,N,T,E,N,O,N,E,T,R,N,T,O,E,N,T,O,E,O,O,R,N,E,H,N,I,N,E,T,R,N,E,T,R]

crlfTL :: TokenList
crlfTL = [N,T,E,N,O,E,T,R,N,T,O,E,N,T,H,H,E,N,T,A,N,E,N,T,A,A,E,N,T,A,O,E,O,O,O,O,O,N,T,E,N,E,T,R,N,T,O,E,N,T,O,N,E,N,T,I,A,E,O,O,O,N,T,E,N,E,T,R,N,T,O,E,N,T,H,T,E,N,T,A,N,E,N,T,A,A,E,N,T,A,O,E,O,O,O,O,O,N,T,E,N,E,T,R]

-- EAS

trueEASTL :: TokenList
trueEASTL = []

helloEASTL :: TokenList
helloEASTL = [N,I,I,E,R
  ,N,S,A,E,N,A,T,S,E,N,A,T,O,E,N,A,T,O,E,N,A,H,O,E,N,T,O,A,E,R
  ,O,O,O,O,O,O,O,R
  ,N,T,O,E,R
  ,N,I,N,E,N,A,H,A,E,N,A,T,O,E,N,A,A,A,E,N,A,T,S,E,N,A,O,H,E,R
  ,O,O,O,O,O,O,O,R]

pipEASTL :: TokenList
pipEASTL = [I,R
  ,N,E,H,R
  ,N,E,N,T,E,S,S,R
  ,N,T,H,E,T,R
  ,N,E,N,T,E,H,T,R
  ,N,T,E,N,E,T,R
  ,O,R
  ,N,T,E,N,T,E,T,R]

pip2EASTL :: TokenList
pip2EASTL =  [I,N,E,H,N,E,N,T,E,S,S,N,A,E,T,N,E,N,T,E,H,T,N,T,E,N,E,T,R
  ,O,N,T,E,N,T,E,T,R]

reverseEASTL :: TokenList
reverseEASTL = [N,E,N,T,E,S,R
  ,I,R
  ,N,E,H,N,E,N,T,E,S,S,N,A,E,T,R
  ,N,E,N,T,E,H,T,R
  ,N,E,H,N,E,N,T,E,S,S,A,N,E,N,T,E,S,S,T,R
  ,N,E,N,T,E,H,T,N,T,E,N,E,T,R
  ,O,N,T,E,N,N,E,T,R]

functionEASTL :: TokenList
functionEASTL = [N,A,E,H,R
  ,N,A,E,H,R
  ,N,E,R
  ,N,T,E,H,R
  ,S,R
  ,S,R
  ,N,T,E,R
  ,N,A,E,H,R
  ,T,R]

writeStrEASTL :: TokenList
writeStrEASTL = [N,T,E,H,R
  ,N,E,H,R
  ,A,N,E,N,T,E,S,S,T,R
  ,S,N,T,E,N,T,E,H,T,R
  ,O,R
  ,N,T,E,N,T,E,T,R]

hello2EASTL :: TokenList
hello2EASTL =
  [N,E,N,T,O,E,N,I,N,E,N,A,H,A,E,N,A,T,O,E,N,A,A,A,E,N,A,T,S,E,N,A,O,H,E,N,I,I,E,N,S,A,E,N,A,T,S,E,N,A,T,O,E,N,A,T,O,E,N,A,H,O,E,N,T,O,A,E,R
  ,A,N,T,E,N,I,E,T,R
  ,N,T,E,N,E,T,R
  ,N,T,E,H,R
  ,N,E,H,R
  ,A,N,E,N,T,E,S,S,T,R
  ,S,N,T,E,N,T,E,H,T,R
  ,O,R
  ,N,T,E,N,I,E,T,R]

hello3EASTL :: TokenList
hello3EASTL =  [N,E,N,T,O,E,N,I,N,E,N,A,H,A,E,N,A,T,O,E,N,A,A,A,E,N,A,T,S,E,N,A,O,H,E,N,I,I,E,N,S,A,E,N,A,T,S,E,N,A,T,O,E,N,A,T,O,E,N,A,H,O,E,N,T,O,A,E,R
  ,A,N,T,E,N,I,E,T,R
  ,N,T,E,N,E,T,R
  ,N,T,E,H,R
  ,N,E,H,R
  ,A,N,E,N,T,E,S,S,T,R
  ,S,N,T,E,N,T,E,H,T,R
  ,O,R
  ,N,T,E,N,I,E,T,R]

hello4EASTL :: TokenList
hello4EASTL = [N,E,N,T,O,E,N,I,N,E,N,A,H,A,E,N,A,T,O,E,N,A,A,A,E,N,A,T,S,E,N,A,O,H,E,N,I,I,E,N,S,A,E,N,A,T,S,E,N,A,T,O,E,N,A,T,O,E,N,A,H,O,E,N,T,O,A,E,R
  ,A,N,T,E,N,I,E,T,R
  ,N,T,E,N,E,T,R
  ,N,T,E,H,R
  ,N,E,H,R
  ,A,N,E,N,T,E,S,S,T,R
  ,S,N,T,E,N,T,E,H,T,R
  ,O,R
  ,N,T,E,N,I,E,T,R]

writeNumEASTL :: TokenList
writeNumEASTL = [N,E,N,A,E,H,R
  ,N,E,H,A,N,E,N,T,E,S,S,T,R
  ,N,S,S,E,O,T,N,T,E,N,T,E,H,T,R
  ,N,E,H,R
  ,A,N,E,N,T,E,S,S,T,R
  ,S,N,T,E,N,T,I,E,T,R
  ,N,T,O,E,E,R
  ,N,E,N,S,S,E,S,S,R
  ,N,T,E,H,R
  ,N,T,E,N,I,E,T,R
  ,A,N,T,E,N,T,S,E,T,R
  ,N,T,E,N,T,E,H,T,R
  ,N,T,E,H,R
  ,N,E,H,R
  ,A,N,E,N,T,E,S,S,T,R
  ,S,N,T,E,N,T,E,H,T,R
  ,O,R
  ,N,T,E,N,T,S,E,T,R]

multiplyEASTL :: TokenList
multiplyEASTL = [N,A,E,H,N,A,E,H,R
  ,N,E,R
  ,N,A,E,H,R
  ,N,A,E,H,R
  ,N,E,H,R
  ,A,N,E,N,T,E,S,S,T,R
  ,N,T,E,N,A,T,E,T,R
  ,N,T,E,S,R
  ,N,A,E,H,R
  ,N,E,N,E,N,O,E,S,H,R
  ,S,S,R
  ,N,A,E,H,R
  ,N,A,E,H,R
  ,N,T,E,N,N,E,T,R
  ,N,T,E,H,T,R
  ,N,T,E,N,A,E,H,T,R]

readNumEASTL :: TokenList
readNumEASTL = [I,R
  ,N,E,H,N,I,I,E,S,R
  ,A,N,E,N,T,E,S,S,T,R
  ,N,E,N,T,E,T,N,T,E,T,R
  ,N,E,N,T,E,H,R
  ,N,S,S,E,N,I,I,E,S,R
  ,N,T,E,H,R
  ,N,T,O,E,A,N,T,E,N,A,N,E,T,R
  ,N,E,N,T,E,H,R
  ,S,S,R
  ,I,R
  ,N,E,H,N,I,I,E,S,N,A,H,E,T,R
  ,N,T,E,N,A,O,E,T,R
  ,N,E,H,N,T,O,E,S,N,A,A,E,T,R
  ,N,T,E,N,A,O,E,T,R
  ,N,E,H,N,E,N,T,E,S,S,N,S,E,T,R
  ,N,E,N,T,E,H,T,R
  ,N,T,E,N,A,E,H,T,R
  ,N,A,E,H,N,A,E,H,R
  ,N,E,R
  ,N,A,E,H,R
  ,N,A,E,H,R
  ,N,E,H,R
  ,A,N,E,N,T,E,S,S,T,R
  ,N,T,E,N,I,N,E,T,R
  ,N,T,E,S,R
  ,N,A,E,H,R
  ,N,E,N,E,N,O,E,S,H,R
  ,S,S,R
  ,N,A,E,H,R
  ,N,A,E,H,R
  ,N,T,E,N,O,A,E,T,R
  ,N,T,E,H,T,R
  ,N,T,E,N,A,E,H,T,R]

factEASTL :: TokenList
factEASTL = [A,N,T,E,N,A,H,E,T,R
  ,A,N,T,E,N,N,E,T,R
  ,A,N,T,E,N,I,I,E,T,R
  ,N,T,O,E,O,N,T,E,N,E,T,R
  ,N,T,E,H,R
  ,N,E,H,R
  ,N,T,E,S,A,N,E,N,T,E,S,S,T,R
  ,N,T,E,N,T,S,E,T,R
  ,N,E,H,R
  ,N,T,E,S,R
  ,A,N,T,E,N,N,E,T,R
  ,A,N,T,E,N,S,A,E,T,R
  ,N,T,E,N,A,E,H,T,R
  ,I,R
  ,N,E,H,N,I,I,E,S,R
  ,A,N,E,N,T,E,S,S,T,R
  ,N,E,N,T,E,T,N,A,H,E,T,R
  ,N,E,N,T,E,H,R
  ,N,S,S,E,N,I,I,E,S,R
  ,N,T,E,H,R
  ,N,T,O,E,A,N,T,E,N,S,A,E,T,R
  ,N,E,N,T,E,H,R
  ,S,S,R
  ,I,R
  ,N,E,H,N,I,I,E,S,N,O,S,E,T,R
  ,N,T,E,N,I,A,E,T,R
  ,N,E,H,N,T,O,E,S,N,I,T,E,T,R
  ,N,T,E,N,I,A,E,T,R
  ,N,E,H,N,E,N,T,E,S,S,N,A,N,E,T,R
  ,N,E,N,T,E,H,T,R
  ,N,T,E,N,A,E,H,T,R
  ,N,E,N,A,E,H,R
  ,N,E,H,A,N,E,N,T,E,S,S,T,R
  ,N,S,S,E,O,T,N,T,E,N,T,E,H,T,R
  ,N,E,H,R
  ,A,N,E,N,T,E,S,S,T,R
  ,S,N,T,E,N,S,H,E,T,R
  ,N,T,O,E,E,R
  ,N,E,N,S,S,E,S,S,R
  ,N,T,E,H,R
  ,N,T,E,N,N,H,E,T,R
  ,A,N,T,E,N,T,T,I,E,T,R
  ,N,T,E,N,T,E,H,T,R
  ,N,A,E,H,N,A,E,H,R
  ,N,E,R
  ,N,A,E,H,R
  ,N,A,E,H,R
  ,N,E,H,R
  ,A,N,E,N,T,E,S,S,T,R
  ,N,T,E,N,T,T,A,E,T,R
  ,N,T,E,S,R
  ,N,A,E,H,R
  ,N,E,N,E,N,O,E,S,H,R
  ,S,S,R
  ,N,A,E,H,R
  ,N,A,E,H,R
  ,N,T,E,N,S,S,E,T,R
  ,N,T,E,H,T,R
  ,N,T,E,N,A,E,H,T,R
  ,N,T,E,H,R
  ,N,E,H,R
  ,A,N,E,N,T,E,S,S,T,R
  ,S,N,T,E,N,T,E,H,T,R
  ,O,R
  ,N,T,E,N,T,T,I,E,T,R]

bottlesEASTL :: TokenList
bottlesEASTL = [N,T,E,N,I,O,E,T,R
  ,N,T,E,H,R
  ,N,E,H,R
  ,A,N,E,N,T,E,S,S,T,R
  ,S,N,T,E,N,T,E,H,T,R
  ,O,R
  ,N,T,E,N,A,E,T,R
  ,N,E,N,A,E,H,R
  ,N,E,H,A,N,E,N,T,E,S,S,T,R
  ,N,S,S,E,O,T,N,T,E,N,T,E,H,T,R
  ,N,E,H,R
  ,A,N,E,N,T,E,S,S,T,R
  ,S,N,T,E,N,A,I,E,T,R
  ,N,T,O,E,E,R
  ,N,E,N,S,S,E,S,S,R
  ,N,T,E,H,R
  ,N,T,E,N,T,I,E,T,R
  ,A,N,T,E,N,A,E,T,R
  ,N,T,E,N,T,E,H,T,R
  ,R
  ,N,T,E,H,R
  ,A,N,T,E,N,T,T,E,T,R
  ,N,E,N,A,A,A,E,N,A,H,O,E,N,A,H,O,E,N,A,H,H,E,N,I,I,E,N,A,H,I,E,N,A,T,S,E,N,I,I,E,N,A,A,O,E,N,A,H,O,E,N,A,T,O,E,N,A,A,I,E,N,A,A,I,E,N,A,T,S,E,N,A,H,H,E,N,I,I,E,R
  ,A,N,T,E,N,A,E,T,R
  ,N,T,E,N,T,E,H,T,R
  ,N,T,E,H,R
  ,A,N,T,E,N,O,H,E,T,R
  ,N,E,N,A,T,O,E,N,A,T,O,E,N,T,S,S,E,N,A,O,H,E,N,I,I,E,N,A,H,O,E,N,A,H,S,E,N,A,A,I,E,N,I,I,E,N,A,T,N,E,N,A,T,S,E,N,I,I,E,R
  ,A,N,T,E,N,A,E,T,R
  ,N,T,E,N,T,E,H,T,R
  ,N,O,E,R
  ,N,E,H,A,N,T,E,N,O,N,E,T,R
  ,N,S,A,E,O,N,I,I,E,O,R
  ,N,E,H,A,N,T,E,N,O,H,E,T,R
  ,N,T,O,E,O,R
  ,N,E,N,T,O,E,N,A,H,A,E,N,A,T,N,E,N,A,A,N,E,N,A,T,S,E,N,A,A,A,E,N,T,S,S,E,N,I,I,E,N,A,A,I,E,N,A,T,H,E,N,I,I,E,N,A,A,O,E,N,A,A,O,E,N,T,S,S,E,N,A,A,H,E,N,I,I,E,N,S,A,E,N,A,T,N,E,N,A,O,H,E,N,A,T,S,E,N,A,H,A,E,N,I,I,E,N,A,H,O,E,N,A,T,N,E,N,A,T,S,E,N,I,I,E,N,A,H,O,E,N,A,T,A,E,N,T,S,S,E,N,T,N,H,E,R
  ,A,N,T,E,N,A,E,T,R
  ,N,T,E,S,R
  ,N,E,H,A,N,T,E,N,O,N,E,T,R
  ,N,T,O,E,N,T,O,E,O,O,R
  ,N,E,H,N,I,I,E,T,R
  ,N,E,T,R]

euclidEASTL :: TokenList
euclidEASTL = [N,A,E,H,N,A,E,H,R
  ,N,E,H,R
  ,A,N,E,N,T,E,S,S,T,R
  ,N,T,E,N,T,O,E,T,R
  ,N,E,N,T,E,S,H,R
  ,E,R
  ,N,E,N,A,E,H,T,R
  ,N,T,E,H,R
  ,N,T,E,N,A,E,T,R
  ,N,E,T,R
  ,N,T,E,N,A,E,H,R
  ,T,R]
