{-# LANGUAGE BangPatterns #-}
module HelVM.HelMA.Automata.Piet.Filler
  ( fillAll
  ) where

import           HelVM.HelMA.Automata.Piet.Types.Coordinates
import           HelVM.HelMA.Automata.Piet.Types.Matrix

import           Control.Monad.ST                            ( runST )
import qualified Data.IntMap.Strict                          as IM
import qualified Data.Vector                                 as V
import qualified Data.Vector.Unboxed.Mutable                 as UMV

-- PUBLIC API

fillAll ∷ Eq a ⇒ Matrix a → (Matrix Int, IntMap BlockCoordinates)
fillAll image
  | V.null image = (V.empty, IM.empty)
  | otherwise = runST $ do
      let height = V.length image
          -- Obliczamy maksymalną szerokość dla bezpiecznego rozplanowania płaskiej tablicy
          maxWidth = V.foldl' (\acc row -> max acc (V.length row)) 0 image
          totalSize = height * maxWidth

      if maxWidth == 0
        then pure (V.map (const V.empty) image, IM.empty)
        else do
          -- -1 = nieodwiedzone / puste
          filledRefs <- UMV.replicate totalSize (-1)
          
          stackX <- UMV.unsafeNew totalSize
          stackY <- UMV.unsafeNew totalSize

          -- Bezpieczne pobieranie piksela (uwzględnia nieregularne długości wierszy)
          let getPixel x y = (image V.! y) V.!? x
              
              -- Pomocniczy wskaźnik do sprawdzania długości konkretnego wiersza
              getRowLen y = V.length (image V.! y)

          let scanGrid !y !x !currentBlockId !accMap
                | y >= height = pure accMap
                | x >= getRowLen y = scanGrid (y + 1) 0 currentBlockId accMap
                | otherwise = do
                    let idx = y * maxWidth + x
                    val <- UMV.unsafeRead filledRefs idx
                    if val /= -1
                      then scanGrid y (x + 1) currentBlockId accMap
                      else do
                        case getPixel x y of
                          Nothing -> scanGrid y (x + 1) currentBlockId accMap
                          Just targetColor -> do
                            -- Znaleźliśmy nowy blok
                            UMV.unsafeWrite filledRefs idx currentBlockId
                            UMV.unsafeWrite stackX 0 x
                            UMV.unsafeWrite stackY 0 y

                            let runDfs !stackPtr !coordsAcc
                                  | stackPtr < 0 = pure coordsAcc
                                  | otherwise    = do
                                      currX <- UMV.unsafeRead stackX stackPtr
                                      currY <- UMV.unsafeRead stackY stackPtr
                                      
                                      let currCoord = (currX, currY)
                                          newAcc = currCoord : coordsAcc

                                      s1 <- checkAndPush targetColor currentBlockId currX (currY - 1) (stackPtr - 1)
                                      s2 <- checkAndPush targetColor currentBlockId currX (currY + 1) s1
                                      s3 <- checkAndPush targetColor currentBlockId (currX - 1) currY s2
                                      s4 <- checkAndPush targetColor currentBlockId (currX + 1) currY s3

                                      runDfs s4 newAcc

                                checkAndPush targetCol blockId nx ny !sPtr
                                  | ny >= 0 && ny < height && nx >= 0 && nx < getRowLen ny = do
                                      let nIdx = ny * maxWidth + nx
                                      nVal <- UMV.unsafeRead filledRefs nIdx
                                      if nVal == -1 && getPixel nx ny == Just targetCol
                                        then do
                                          UMV.unsafeWrite filledRefs nIdx blockId
                                          let nextPtr = sPtr + 1
                                          UMV.unsafeWrite stackX nextPtr nx
                                          UMV.unsafeWrite stackY nextPtr ny
                                          pure nextPtr
                                        else pure sPtr
                                  | otherwise = pure sPtr

                            blockCoords <- runDfs 0 []
                            let newMap = IM.insert currentBlockId blockCoords accMap
                            scanGrid y (x + 1) (currentBlockId + 1) newMap

          blockMap <- scanGrid 0 0 0 IM.empty

          -- Rekonstrukcja macierzy wyjściowej z zachowaniem dokładnych długości oryginalnych wierszy!
          resultMatrix <- V.generateM height $ \y -> do
            let rowLen = V.length (image V.! y)
            V.generateM rowLen $ \x -> do
              val <- UMV.unsafeRead filledRefs (y * maxWidth + x)
              pure $ if val == -1 then 0 else val

          pure (resultMatrix, blockMap)
