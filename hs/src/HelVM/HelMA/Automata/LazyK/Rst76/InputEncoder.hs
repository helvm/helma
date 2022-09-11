module HelVM.HelMA.Automata.LazyK.Rst76.InputEncoder (
  readInput,
  eof,
  pre,
  suc,
  church,
) where

import           HelVM.HelMA.Automata.LazyK.Rst76.Constants

import           HelVM.HelMA.Automata.LazyK.Constants
import           HelVM.HelMA.Automata.LazyK.Lambda

import qualified Data.ByteString.Lazy                       as LBS

readInput :: LBS.ByteString -> Lambda
readInput = encodeInput . fmap fromIntegral . LBS.unpack

encodeInput :: [Natural] -> Lambda
encodeInput = foldr (consXY . church) eof

eof :: Lambda
eof = K `App` church 256

pre :: Lambda
pre = delete (Var "n") $ Var "n" `App` f `App` (K `App` church 0) `App` I
  where
  f = delete (Var "x") $ delete (Var "y") $ Var "y" `App` (Var "x" `App` suc)

suc :: Lambda
suc = App S b

church :: Natural -> Lambda
church 0   = App S K
church 1   = I
church 2   = suc `App` I
church 4   = m `App` church 2
church 8   = S `App` (S `App` b) `App` I `App` church 2
church 16  = si `App` m `App` church 2
church 256 = m `App` church 4
church n   = numB n `App` b

numB :: Natural -> Lambda
numB 0 = K `App` false
numB 1 = false
numB 2 = ss `App` numB 1
numB 3 = ss `App` numB 2
numB 4 = ssi `App` numB 2
numB 5 = ss `App` numB 4
numB 6 = ssss1 `App` numB 2
numB 7 = ss `App` numB 6
numB 8 = S `App` numB 3 `App` numB 2
numB 9 = ssss `App` numB 2
numB 10 = ss `App` numB 9
numB 11 = ss `App` numB 10
numB 12 = S `App` (ss `App` sss) `App` numB 2
numB 13 = ss `App` numB 12
numB 14 = ss `App` numB 13
numB 15 = S `App` (ss `App` ssiss) `App` numB 3
numB 16 = ss `App` ssi `App` numB 2
numB 17 = ss `App` numB 16
numB 18 = ss `App` numB 17
numB 19 = ss `App` numB 18
numB 20 = ssss1 `App` numB 4
numB 21 = ss `App` numB 20
numB 22 = ss `App` numB 21
numB 23 = ss `App` numB 22
numB 24 = S `App` (ss `App` (ss `App` sss)) `App` numB 2
numB 25 = ss `App` (S `App` (K `App` ss) `App` ssi) `App` numB 2
numB 26 = ss `App` numB 25
numB 27 = ssi `App` numB 3
numB 28 = ss `App` numB 27
numB 29 = ss `App` numB 28
numB 30 = ssss1 `App` numB 5
numB 31 = ss `App` numB 30
numB 32 = S `App` numB 5 `App` numB 2
numB 33 = ss `App` numB 32
numB 34 = ss `App` numB 33
numB 35 = S `App` (ss `App` ssiss) `App` numB 5
numB 36 = ss `App` ssss1 `App` numB 2
numB 37 = ss `App` numB 36
numB 38 = ss `App` numB 37
numB 39 = ss `App` numB 38
numB 40 = S `App` (si `App` numB 2) `App` numB 20
numB 41 = ss `App` numB 40
numB 42 = ssss1 `App` numB 6
numB 43 = ss `App` numB 42
numB 44 = ss `App` numB 43
numB 45 = S `App` (ss `App` (ss `App` ssiss)) `App` numB 3
numB 46 = ss `App` numB 45
numB 47 = ss `App` numB 46
numB 48 = S `App` (ss `App` (ss `App` (ss `App` sss))) `App` numB 2
numB 49 = ss `App` (S `App` (K `App` ss) `App` ssss1) `App` numB 2
numB 50 = ss `App` numB 49
numB 51 = ss `App` numB 50
numB 52 = ss `App` numB 51
numB 53 = ss `App` numB 52
numB 54 = S `App` (si `App` numB 2) `App` numB 27
numB 55 = ss `App` numB 54
numB 56 = ssss1 `App` numB 7
numB 57 = ss `App` numB 56
numB 58 = ss `App` numB 57
numB 59 = ss `App` numB 58
numB 60 = S `App` (si `App` numB 2) `App` numB 30
numB 61 = ss `App` numB 60
numB 62 = S `App` (si `App` numB 2) `App` numB 31
numB 63 = S `App` (ss `App` ssiss) `App` numB 7
numB 64 = ssss `App` numB 3
numB 65 = ss `App` numB 64
numB 66 = ss `App` numB 65
numB 67 = ss `App` numB 66
numB 68 = ss `App` numB 67
numB 69 = ss `App` numB 68
numB 70 = S `App` (S `App` numB 6 `App` S) `App` numB 64
numB 71 = ss `App` numB 70
numB 72 = ssss1 `App` numB 8
numB 73 = ss `App` numB 72
numB 74 = S `App` (si `App` numB 2) `App` numB 37
numB 75 = ss `App` numB 74
numB 76 = S `App` (si `App` numB 2) `App` numB 38
numB 77 = ss `App` numB 76
numB 78 = S `App` (si `App` numB 2) `App` numB 39
numB 79 = ss `App` numB 78
numB 80 = S `App` (ss `App` sss) `App` numB 4
numB 81 = ssssss `App` numB 2
numB 82 = ss `App` numB 81
numB 83 = ss `App` numB 82
numB 84 = ss `App` numB 83
numB 85 = ss `App` numB 84
numB 86 = ss `App` numB 85
numB 87 = S `App` (S `App` numB 6 `App` S) `App` numB 81
numB 88 = ss `App` numB 87
numB 89 = S `App` (S `App` numB 8 `App` S) `App` numB 81
numB 90 = ssss1 `App` numB 9
numB 91 = ss `App` numB 90
numB 92 = ss `App` numB 91
numB 93 = ss `App` numB 92
numB 94 = ss `App` numB 93
numB 95 = ss `App` numB 94
numB 96 = S `App` (ss `App` (ss `App` (ss `App` (ss `App` sss)))) `App` numB 2
numB 97 = ss `App` numB 96
numB 98 = S `App` (si `App` numB 2) `App` numB 49
numB 99 = S `App` (ss `App` ssiss) `App` numB 9
numB 100 = S `App` numB 2 `App` numB 10
numB 101 = ss `App` numB 100
numB 102 = ss `App` numB 101
numB 103 = ss `App` numB 102
numB 104 = ss `App` numB 103
numB 105 = S `App` (S `App` numB 24 `App` S) `App` numB 81
numB 106 = S `App` (S `App` numB 16 `App` S) `App` numB 90
numB 107 = S `App` (S `App` numB 27 `App` S) `App` numB 80
numB 108 = S `App` (ss `App` (ss `App` sss)) `App` numB 3
numB 109 = ss `App` numB 108
numB 110 = ssss1 `App` numB 10
numB 111 = ss `App` numB 110
numB 112 = ss `App` numB 111
numB 113 = ss `App` numB 112
numB 114 = ss `App` numB 113
numB 115 = ss `App` numB 114
numB 116 = S `App` (si `App` numB 4) `App` numB 29
numB 117 = S `App` (S `App` numB 36 `App` S) `App` numB 81
numB 118 = ss `App` numB 117
numB 119 = S `App` (S `App` numB 9 `App` S) `App` numB 110
numB 120 = S `App` (ss `App` ssiss) `App` numB 10
numB 121 = S `App` numB 2 `App` numB 11
numB 122 = ss `App` numB 121
numB 123 = ss `App` numB 122
numB 124 = ss `App` numB 123
numB 125 = ss `App` (S `App` (K `App` ss) `App` ss) `App` numB 3
numB 126 = ss `App` numB 125
numB 127 = ss `App` numB 126
numB 128 = S `App` numB 7 `App` numB 2
numB 256 = ssi `App` numB 4
numB n | n < 256 = ss `App` numB (n - 1)
numB n = error $ show n
