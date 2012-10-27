{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
module Control.Concurrent.STM.TChan.Class 
    where

import qualified Control.Concurrent.STM.TChan as C
import Control.Concurrent.STM


-- | A class capturing Chan operations in STM.
class SplitTChan i o | i -> o, o -> i where
    -- | Write a value to the in chan.
    writeTChan :: i a -> a -> STM ()
    -- | Read the next value from the out chan.
    readTChan :: o a -> STM a
    -- | Get the next value from the @TChan@ without removing it,
    -- retrying if the channel is empty.
    peekTChan :: o a -> STM a
    -- | A version of 'peekTChan' which does not retry. Instead it
    -- returns @Nothing@ if no value is available.
    tryPeekTChan :: o a -> STM (Maybe a)
    -- | A version of 'readTChan' which does not retry. Instead it
    -- returns @Nothing@ if no value is available.
    tryReadTChan :: o a -> STM (Maybe a)
    -- |Returns 'True' if the supplied 'TChan' is empty.
    isEmptyTChan :: o a -> STM Bool



-- | A class for 'SplitTChan' types that can be instantiated without programmer
-- input. /e.g./ the standard haskell @TChan@ is a member of this class, however
-- a bounded chan type that took an @Int@ to define the buffer size would not.
class (SplitTChan i o)=> NewSplitTChan i o where
    newSplitTChan :: STM (i a, o a)



-- instances -- 


-- one-bounded chan:
instance SplitTChan TMVar TMVar where
    readTChan = takeTMVar
    writeTChan = putTMVar
    peekTChan = readTMVar
    tryReadTChan = tryTakeTMVar
    tryPeekTChan = tryReadTMVar
    isEmptyTChan = isEmptyTMVar

instance SplitTChan C.TChan C.TChan where
    writeTChan = C.writeTChan
    readTChan = C.readTChan
    peekTChan = C.peekTChan
    tryReadTChan = C.tryReadTChan
    tryPeekTChan = C.tryPeekTChan
    isEmptyTChan = C.isEmptyTChan

instance NewSplitTChan TMVar TMVar where
    newSplitTChan = do v <- newEmptyTMVar
                       return (v,v)

instance NewSplitTChan TChan TChan where
    newSplitTChan = do v <- C.newTChan
                       return (v,v)
