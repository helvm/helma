module HelVM.HelCam.Machines.SubLeq.EvaluatorTestData where

hello :: String
hello = "Hello world!\n"

helloSQ :: String
helloSQ = "15 17 -1 17 -1 -1 16 1 -1 16 3 -1 15 15 0 0 -1 72 101 108 108 111 44 32 119 111 114 108 100 33 10 0"

helloSQIL :: Integral i => [i]
helloSQIL = [15, 17, -1, 17, -1, -1, 16, 1, -1, 16, 3, -1, 15, 15, 0, 0, -1, 72, 101, 108, 108, 111, 32, 119, 111, 114, 108, 100, 33, 10, 0]
