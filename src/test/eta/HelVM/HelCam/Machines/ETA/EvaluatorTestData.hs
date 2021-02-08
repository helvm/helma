module HelVM.HelCam.Machines.ETA.EvaluatorTestData where

import HelVM.HelCam.Machines.ETA.Token

import HelVM.HelCam.Common.Util

--------------------------------------------------------------------------------

hello :: Output
hello ="Hello, world!\n"

helloETA :: Source
helloETA =
  "No heat: \"hello.eta\", written by Mike Taylor         \n\
  \                                                       \n\
  \** FUNGICIDE **                                        \n\
  \-- Fungus calendar --                                  \n\
  \CURTSEY:                                               \n\
  \    Fungal toe!  Fungal toe!  Fungal hoe!              \n\
  \        (Burnt programmer nucleus)                     \n\
  \    Ooooooo!                                           \n\
  \ CRUDDY 2nd TOE:                                       \n\
  \    Nine(!) fungal hyaena toe5!                        \n\
  \    Dungy alfalfa, penalty superlunary -- Oh, blubber! \n\
  \    Oo! Oooo! OW!"

helloZIP :: Source
helloZIP = "NOHEATHEOETAITTENIETAO\n\nNIIE\nNSAENA\nTSE\nNATOENATOENAHOE\nNTOAENES\nOOOOOOO\nNTOE\nNINENAHAENATOE\nNAAAENATSENAOHE\nOOOOOOO"

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
  ,O,O,O,O,O,O,O]

hello2ETA :: Source
hello2ETA =
  "NEN toe nine NA hyaena toe N <aaa!> Renault sudden Adolph Enid: ugly M$IE4.0 \n\
  \Cygnus agenda: t'send a toe 2 nato.  Bend a ghoul entourage ant!  Venice T.  \n\
  \NT, Be, Next -- a not-budget list of Operating Systems.                      \n\
  \Nurture 1 hundred hangmen, uterus 1st.                                       \n\
  \s3ntient L1fe-f0rm?  J@bb@ $ Hut?!                                           \n\
  \-- On the Niet (Russian)"

hello2ZIP :: Source
hello2ZIP = "NENTOENINENAHAENATOENAAAENATSENAOHENIIE\nNSAENATSENATOENATOENAHOENTOAEANTENIET\nNTENETANOTETISTOOEATINSSTES\nNTEHNEHANENTESST\nSNTIENTEHT\nONTHENIETSSIAN"

hello2TL :: TokenList
hello2TL =
  [N,E,N,T,O,E,N,I,N,E,N,A,H,A,E,N,A,T,O,E,N,A,A,A,E,N,A,T,S,E,N,A,O,H,E,N,I,I,E,R
  ,N,S,A,E,N,A,T,S,E,N,A,T,O,E,N,A,T,O,E,N,A,H,O,E,N,T,O,A,E,A,N,T,E,N,I,E,T,R
  ,N,T,E,N,E,T,A,N,O,T,E,T,I,S,T,O,O,E,A,T,I,N,S,S,T,E,S,R
  ,N,T,E,H,N,E,H,A,N,E,N,T,E,S,S,T,R
  ,S,N,T,I,E,N,T,E,H,T,R
  ,O,N,T,H,E,N,I,E,T,S,S,I,A,N]

----

hello3ZIP ::Source
hello3ZIP = "Niie\nNsaeNatseNatoeNatoeNahoeNtoae\nOOOOOOO\nNtoe\nNineNahaeNatoeNaaaeNatseNaohe\nOOOOOOO\n"

hello3ETA ::Source
hello3ETA = hello3ZIP


crlf :: Output
crlf = "OK\n"

crlfETA :: Source
crlfETA =
  "Nte Noe T                                       \n\
  \Ntoe Nthhe Ntane Ntaae Ntaoe O O O O O Nte Ne T \n\
  \Ntoe Ntone Ntiae O O O Nte Ne T                 \n\
  \Ntoe Nthte Ntane Ntaae Ntaoe O O O O O Nte Ne T"
