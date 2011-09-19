{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Control.Concurrent.Chan.Class
    where

import qualified Control.Concurrent.Chan as C
import Control.Concurrent.MVar

{-
 - We create a set of classes for FIFO Chan types, using the function names from
 - Control.Concurrent.Chan, but ommiting deprecated functions.
 -     We also omit the 'dupChan' function because it is not central to the Chan
 - concept. Similarly we omit getChanContents as a method, because it is not
 - necessary that every Chan should have a stream interface.
-}


-- | A class for chan types with a \"write end\" and a \"read end\". A minimal
-- complete instance defines 'readChan' and one of 'writeChan' or
-- 'writeList2Chan'.
class SplitChan i o | i -> o, o -> i where
    -- | Read the next value from the 'OutChan'.
    readChan :: o a -> IO a

    -- | Write an entire list of items to a chan type
    writeList2Chan :: i a -> [a] -> IO ()
    writeList2Chan = mapM_ . writeChan

    -- | Write a value to a Chan type.
    writeChan :: i a -> a -> IO ()
    writeChan c = writeList2Chan c . return

-- | A class for 'SplitChan' types that can be instantiated without programmer
-- input. /e.g./ the standard haskell @Chan@ is a member of this class, however
-- a bounded chan type that took an @Int@ to define the buffer size would not.
class (SplitChan i o)=> NewSplitChan i o where
    newSplitChan :: IO (i a, o a)


-- -------------------------------
-- INSTANCES FOR STANDARD TYPES --
-- -------------------------------


instance SplitChan C.Chan C.Chan where
    writeList2Chan = C.writeList2Chan
    writeChan = C.writeChan
    readChan = C.readChan

instance NewSplitChan C.Chan C.Chan where
    newSplitChan = do c <- C.newChan
                      return (c,c)

-- an MVar is a singly-bounded Chan. Think about it.
instance SplitChan MVar MVar where
    writeChan = putMVar
    readChan = takeMVar
       
instance NewSplitChan MVar MVar where
    newSplitChan = do v <- newEmptyMVar
                      return (v,v)
