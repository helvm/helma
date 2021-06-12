module HelVM.Common.Containers.ChunksOf where

--import qualified Data.List.Split as List
--import qualified Data.Sequence   as Seq

class ChunksOf e c | c -> e where
  chunksOf :: Int -> c -> c

--instance ChunksOf e [e] where
--  chunksOf = List.chunksOf
--
--instance ChunksOf e (Seq e) where
--  chunksOf = Seq.chunksOf
