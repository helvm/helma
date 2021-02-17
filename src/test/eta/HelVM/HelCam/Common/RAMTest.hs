module HelVM.HelCam.Common.RAMTest where

import HelVM.HelCam.Common.RAM.ListRAM as L
import HelVM.HelCam.Common.RAM.SeqRAM as S

import Test.HUnit

testsOfRAM :: Test
testsOfRAM = test
  [ "emptyList"      ~: "test emptyList"    ~: []        ~=? (toList L.empty ::[Integer])
  , "emptySeq"       ~: "test emptyList"    ~: []        ~=? (toList S.empty ::[Integer])
  , "l load empty"   ~: "test l load empty" ~: 0         ~=? (L.load L.empty 1 ::Integer)
  , "s load empty"   ~: "test s load empty" ~: 0         ~=? (S.load S.empty 1 ::Integer)
  , "l load empty"   ~: "test l load empty" ~: 0         ~=? (L.load L.empty 2 ::Integer)
  , "s load empty"   ~: "test s load empty" ~: 0         ~=? (S.load S.empty 2 ::Integer)
  , "l one"          ~: "L.store 1 1 L.empty" ~: [0, 1]    ~=? (toList $ L.store 1 1 L.empty ::[Integer])
  , "s one"          ~: "S.store 1 1 S.empty" ~: [0, 1]    ~=? (toList $ S.store 1 1 S.empty ::[Integer])
  , "l one"          ~: "test l one"        ~: [0, 2]    ~=? (toList $ L.store 1 2 L.empty ::[Integer])
  , "s one"          ~: "test s one"        ~: [0, 2]    ~=? (toList $ S.store 1 2 S.empty ::[Integer])
  , "l one"          ~: "L.store 1 1 $ L.store 1 1 L.empty" ~: [0, 1]    ~=? (toList $ L.store 1 1 $ L.store 1 1 L.empty ::[Integer])
  , "s one"          ~: "L.store 1 1 $ L.store 1 1 L.empty" ~: [0, 1]    ~=? (toList $ S.store 1 1 $ S.store 1 1 S.empty ::[Integer])
  , "l one"          ~: "test l one"        ~: [0, 0, 1] ~=? (toList $ L.store 2 1 L.empty ::[Integer])
  , "s one"          ~: "test s one"        ~: [0, 0, 1] ~=? (toList $ S.store 2 1 S.empty ::[Integer])
  , "l one"          ~: "test l one"        ~: [0, 1, 2] ~=? (toList $ L.store 2 2 $ L.store 1 1 L.empty ::[Integer])
  , "s one"          ~: "test s one"        ~: [0, 1, 2] ~=? (toList $ S.store 2 2 $ S.store 1 1 S.empty ::[Integer])
  ]
