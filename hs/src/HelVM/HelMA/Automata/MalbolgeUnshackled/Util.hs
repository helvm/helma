module HelVM.HelMA.Automata.MalbolgeUnshackled.Util where

import           System.IO (hPutStr)  

-- | Remove extra digits in list
compressList :: (Foldable t, Eq a) => t a -> [a]
compressList l0 = foldr cpr [] l0 where
    cpr x l @ [y] | x == y = l
    cpr x l = x : l

crash :: b
crash = error $! show $ foldl' (^) (2 :: Integer) $ cycle [(2 :: Integer)]

hang :: b
hang = error $! show $ foldl' (+) (0 :: Integer) $ cycle [-1,1]

-- | Errors that shouldn't be possible.
bug :: Text -> a
bug msg = error $ "Internal error: " <> msg

debug :: MonadIO m => String -> m ()
debug msg = liftIO $ hPutStr stderr msg
