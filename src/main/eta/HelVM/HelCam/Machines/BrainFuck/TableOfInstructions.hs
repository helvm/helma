module HelVM.HelCam.Machines.BrainFuck.TableOfInstructions where

import HelVM.HelCam.Machines.BrainFuck.Token

type HalfTable = TokenList
type Table = (HalfTable, HalfTable)
type TableD = Table -> Table

prevInst :: TableD
prevInst (inst:prev, next) = (prev, inst:next)
prevInst ([], _)           = error "End of the table"

nextInst :: TableD
nextInst (prev, inst:next) = (inst:prev, next)
nextInst (_, [])           = error "End of the table"

matchPrevJmp :: TableD
matchPrevJmp table@(JmpPast:_, _) =                                                   table
matchPrevJmp table@(JmpBack:_, _) = matchPrevJmp $ prevInst $ matchPrevJmp $ prevInst table
matchPrevJmp table                =                           matchPrevJmp $ prevInst table

matchNextJmp :: TableD
matchNextJmp table@(_, JmpBack:_) =                               nextInst table
matchNextJmp table@(_, JmpPast:_) = matchNextJmp $ matchNextJmp $ nextInst table
matchNextJmp table                =                matchNextJmp $ nextInst table

jumpPast :: TableD
jumpPast table = matchNextJmp $ nextInst table

jumpBack :: TableD
jumpBack table = matchPrevJmp $ prevInst table
