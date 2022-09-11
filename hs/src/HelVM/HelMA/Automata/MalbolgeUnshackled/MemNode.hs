module HelVM.HelMA.Automata.MalbolgeUnshackled.MemNode where

import HelVM.HelMA.Automata.MalbolgeUnshackled.Trit  
import HelVM.HelMA.Automata.MalbolgeUnshackled.Value  

type MemNodes = Trit -> MemNode

data MemNode = MemNode {
    nodes :: MemNodes, 
    next :: MemNode,
    value :: IORef Value, 
    modClass :: Int, 
    width :: Int
    }