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


-- | A class for Chan types that can be written to. A minimum complete
-- instance defines one of 'writeList2Chan' or 'writeChan'
class WritableChan c where
    -- | Write an entire list of items to a chan type
    writeList2Chan :: c a -> [a] -> IO ()
    writeList2Chan = mapM_ . writeChan

    -- | Write a value to a Chan type.
    writeChan :: c a -> a -> IO ()
    writeChan c = writeList2Chan c . return

-- | A class for Chan types that can be read from. 
class ReadableChan c where
    -- | Read the next value from the 'OutChan'.
    readChan :: c a -> IO a



-- -------------------------------
-- INSTANCES FOR STANDARD TYPES --
-- -------------------------------


instance WritableChan C.Chan where
    writeList2Chan = C.writeList2Chan
    writeChan = C.writeChan

instance ReadableChan C.Chan where
    readChan = C.readChan


-- an MVar is a bounded Singleton Chan. Think about it.
instance WritableChan MVar where
    writeChan = putMVar

instance ReadableChan MVar where
    readChan = takeMVar
