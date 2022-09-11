{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecursiveDo    #-}

module HelVM.HelMA.Automata.MalbolgeUnshackled.MalbolgeUnshackled where

import HelVM.HelMA.Automata.MalbolgeUnshackled.MemNode
import HelVM.HelMA.Automata.MalbolgeUnshackled.Trit
import HelVM.HelMA.Automata.MalbolgeUnshackled.Util
import HelVM.HelMA.Automata.MalbolgeUnshackled.Value

import           System.IO.Unsafe    (unsafeInterleaveIO)

import           Control.Monad
import           Data.Array
import           Data.Char           (isSpace)
import           System.IO (getChar , hClose , hPutStr , isEOF , putChar)
import           System.Random       (randomIO, randomRIO)

import Prelude hiding ( bug )

-- | The memory structure, a combined trie and linked list

data BranchEnv = BEnv {
    restArray :: Array Int Value, modCombine :: Int -> Trit -> Int }

type UMonad = StateT UState IO
data UState = UState {
    other :: OtherState,
    a     :: Value,
    c     :: MemNode,
    d     :: MemNode }
data OtherState = Other { -- Things that change rarely, if at all
    memory       :: MemNode,
    rotWidth     :: Int,
    maxWidth     :: Int,
    growthPolicy :: Int -> UMonad OtherState }
data Instruction = OutOfBounds | DefaultNop | I | LS | DIV | MUL | J | P | O | V
    deriving stock (Show)

-- | Main function
main :: IO ((), UState)
main = do
    args <- getArgs
    let(allowOutOfBounds, file) = case args of
        [file]       -> (True, file)
        ["-n", file] -> (False, file)
        _            -> error "Usage: [-n] <filename>"
    mem <- fillMemory allowOutOfBounds =<< readFile file
    runProg mem

{-
 - Memory and its initialization
 -}

-- Initialize a new memory from the program
fillMemory :: Bool -> String -> IO MemNode
fillMemory allowOutOfBounds prog = mdo
    mem <- newMemory rArr
    rArr <- fillProg allowOutOfBounds (nodes mem T0) $
        checkProgLength $ filter (not . isSpace) prog
    return mem
  where
    checkProgLength p@(_:_:_) = p
    checkProgLength _         = error "Source program too short"

fillProg :: Bool -> MemNode -> String -> IO (Array Int Value)
fillProg _ _ [] = return $ bug "Return value of fillProg _ _ [] used"
fillProg allowOutOfBounds fromNode (c:rest) =
    case instruction v m of
        DefaultNop -> illegalProgram
        OutOfBounds | not allowOutOfBounds -> illegalProgram
        i -> do
            writeIORef (value fromNode) v
            case rest of
                [lc] -> do
                    fillRestProg
                    -- Return array of values to fill in the rest of memory.
                    -- Repeats every restMod (=6) values.
                    return $ listArray (0,5) $ take restMod $
                        drop (2 + (restMod-2-m) `mod` restMod) $
                        iterate2 (flip opValue) v (charToValue lc)
                _ -> fillRestProg
  where
    m = modClass fromNode
    v = charToValue c
    fillRestProg = fillProg allowOutOfBounds (next fromNode) rest
    illegalProgram = error $
        "Illegal instruction " <> show c <>
        " in source at offset " <> show m

newMemory :: Array Int Value -> IO MemNode
newMemory rArr = mdo
    t01 <- newNode be0 t0 t02 T1
    t02 <- newNode be0 t0 (t01 `nodes` T0) T2
    t10 <- newNode be1 t1 t1 T0
    t12 <- newNode be1 t1 (t12 `nodes` T0) T2
    t20 <- newNode be2 t2 t21 T0
    t21 <- newNode be2 t2 t2 T1
    (t0, be0) <- topNode rArr (nodeMap doubleBug t01 t02) t01 T0
    (t1, be1) <- topNode rArr (nodeMap t10 doubleBug t12) t12 T1
    (t2, be2) <- topNode rArr (nodeMap t20 t21 doubleBug) t0 T2
    return $ MemNode {
        nodes = nodeMap t0 t1 t2,
        next = rootBug, value = rootBug, modClass = rootBug, width = rootBug }
  where
    doubleBug = bug "Duplicate initial trit"
    rootBug = bug "Root memory node used as leaf"

topNode :: (MonadIO m, Enum a) => Array Int Value -> MemNodes -> MemNode -> a -> m (MemNode, BranchEnv)
topNode rArr nodes next trit = do
    value <- newIORef $ rArr ! (modClass `mod` restMod)
    return
        (MemNode {
            nodes = nodes, next = next,
            value = value, modClass = modClass, width = 0 },
        BEnv {restArray = rArr, modCombine = modCombine} )
  where
    tval = fromEnum trit
    modAs = tval * ((3^10-1) `div` 2)
    modClass = modAs `mod` fullMod
    modAdjust = (modClass - (modClass * 3 + tval)) `mod` fullMod
    modCombine modClass trit =
        (modClass * 3 + fromEnum trit + modAdjust) `mod` fullMod

newNodes :: BranchEnv -> MemNode -> MemNode -> IO (Trit -> MemNode)
newNodes benv parent next = unsafeInterleaveIO $ do
    n2 <- newNode benv parent (next `nodes` T0) T2
    n1 <- newNode benv parent n2 T1
    n0 <- newNode benv parent n1 T0
    return $ nodeMap n0 n1 n2

newNode :: BranchEnv -> MemNode -> MemNode -> Trit -> IO MemNode
newNode benv parent next trit = unsafeInterleaveIO $ mdo
    this <- do
        nodes <- newNodes benv this next
        modClass <- return $! modCombine benv (modClass parent) trit
        width <- return $! width parent + 1
        value <- newIORef $ restArray benv ! (modClass `mod` restMod)
        return MemNode {
            nodes = nodes, next = next,
            value = value, modClass = modClass, width = width }
    return this


-- | Running a program in memory
runProg :: MemNode -> IO ((), UState)
runProg mem = do
    o <- selectPolicy mem
    let memStart = mem `nodes` T0
    runStateT progLoop UState {
        other = o,
        a = ListV [T0],
        c = memStart,
        d = memStart }

progLoop :: StateT UState IO ()
progLoop = do
    c <- gets c
    v <- io $ readIORef (value c)
    case instruction v (modClass c) of
        V -> return ()
        i -> do
            execInstr i
            postalStage
            progLoop

-- | Encrypt and increment
postalStage :: StateT UState IO ()
postalStage = do
    st@UState{c=c@MemNode{value}, d} <- get
    v <- liftM vToOffset $ io $ readIORef value
    case v of
        OffsetV T0 o | o >= 33 && o <= 126
            -> io $ writeIORef value (encrypt ! o)
        _ -> crash
    put st{c = next c, d = next d}

-- | Execute any instruction other than v
execInstr :: Instruction -> StateT UState IO ()
execInstr OutOfBounds = hang
execInstr DefaultNop  = return ()
execInstr I = do
    st@UState {c, d} <- get
    io $ modifyIORef (value c) vToOffset
    newc <- lookupMem =<< io (readIORef $ value d)
    put st {c = newc}
execInstr LS = do
    a <- liftM vToOffset $ gets a
    io $ case a of
        OffsetV T0 o    -> putChar $ toEnum $ fromInteger o
        OffsetV T2 0    -> hClose stdout
        OffsetV T2 (-1) -> putChar '\n'
        _               -> error $ "Unimplemented output character"
execInstr DIV = do
    newa <- io $ do
        e <- isEOF
        if e then return $ OffsetV T2 0 else do
          ch <- getChar
          return $ if ch=='\n'
            then OffsetV T2 (-1)
            else OffsetV T0 $ toInteger $ fromEnum ch
    st <- get
    put st{a = newa}
execInstr MUL = do
    st@UState{d=d@MemNode{value}, other=Other{rotWidth}} <- get
    res <- liftM (rotate rotWidth) $ io $ readIORef value
    io $ writeIORef value res
    put st{a = res}
execInstr J = do
    st@UState{d=d@MemNode{value}, other=o@Other{maxWidth}} <- get
    newd@MemNode{width} <- lookupMem =<< io (readIORef value)
    newOther <- if width > maxWidth then growthPolicy o width else return o
    put st{d = newd, other = newOther}
execInstr P = do
    st@UState{a,d=d@MemNode{value}} <- get
    res <- liftM (opValue a) $ io $ readIORef value
    io $ writeIORef value res
    put st{a = res}
execInstr O = return ()


-- | Look up a memory node from an address value
lookupMem :: MonadState UState m => Value -> m MemNode
lookupMem addr = do
    mem <- gets (memory . other)
    let ListV l = vToList addr
    return $ foldl' nodes mem $ reverse l

-- | Decode instruction from value and mod
instruction :: Value -> Int -> Instruction
instruction val m = case vToOffset val of
    OffsetV T0 o
        | o < 33 || o > 126 -> OutOfBounds
        | otherwise -> case (fromInteger o + m) `mod` instrMod of
            4 -> I ; 5 -> LS ; 23 -> DIV ; 39 -> MUL
            40 -> J ; 62 -> P ; 68 -> O ; 81 -> V
            _ -> DefaultNop
    OffsetV _ _ -> OutOfBounds


-- | Random policy generator
selectPolicy :: MonadIO m => MemNode -> m OtherState
selectPolicy mem = do
    rw <- randomRIO (10, 15)
    det <- randomIO
    gp <- if det then do
        step <- randomRIO (4, 12)
        slack <- randomRIO (0, 5)
        return $ \newMax -> do
            o@Other{rotWidth} <- gets other
            return o{ maxWidth = newMax, rotWidth =
                if 2*newMax + slack > rotWidth
                    then max (2*newMax) (rotWidth + step)
                    else rotWidth }
      else do
        prob <- randomRIO (0.2 :: Double, 0.8)
        slack <- randomRIO (0, 5)
        return $ \newMax -> do
            o@Other{rotWidth} <- gets other
            let min0 = newMax * 2
            will <- if min0 > rotWidth
                then return True
                else liftM (<= prob) $ io randomIO
            newRot <- if will
                then liftM (+ max min0 rotWidth) $ io $ randomRIO (0, slack)
                else return rotWidth
            return o{ maxWidth = newMax, rotWidth = newRot }
    return Other {
        memory=mem, maxWidth=0, rotWidth=rw, growthPolicy=gp }

-- | Iteration of a function on the previous two values, like Fibonacci sequence
iterate2 :: (b -> b -> b) -> b -> b -> [b]
iterate2 f l1 l2 = map fst $ flip iterate (l1,l2) $ \(p1,p2) -> (p2, f p1 p2)

-- | Moduli for filling memory and decoding instructions
restMod :: Int
restMod = 6 :: Int

instrMod :: Int
instrMod = 94 :: Int

fullMod :: Int
fullMod = lcm restMod instrMod



io :: IO a -> UMonad a
io = liftIO :: IO a -> UMonad a
