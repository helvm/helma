module HelVM.HelMA.Automata.BrainFuck.Impl.Flat.TableOfInstructions where

import           HelVM.HelMA.Automata.BrainFuck.Impl.Flat.Instruction

type HalfTable = FlatTreeInstructionList
type Table = (HalfTable , HalfTable)
type TableD = Table -> Table

prevInst :: TableD
prevInst (inst : prev , next) = (prev , inst : next)
prevInst ([] , _)             = error "End of the table"

nextInst :: TableD
nextInst (prev , inst : next) = (inst : prev , next)
nextInst (_ , [])             = error "End of the table"

matchPrevJmp :: TableD
matchPrevJmp table@(JmpPast : _ , _) =                                      table
matchPrevJmp table@(JmpBack : _ , _) = (matchPrevJmp . prevInst . jumpBack) table
matchPrevJmp table                   =                            jumpBack  table

matchNextJmp :: TableD
matchNextJmp table@(_ , JmpBack : _) =                 nextInst  table
matchNextJmp table@(_ , JmpPast : _) = (matchNextJmp . jumpPast) table
matchNextJmp table                   =                 jumpPast  table

jumpPast :: TableD
jumpPast = matchNextJmp . nextInst

jumpBack :: TableD
jumpBack = matchPrevJmp . prevInst
