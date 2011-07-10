{-# LANGUAGE GADTs #-}
module Control.Concurrent.Chan.Split (
    -- * Chan pairs
      newChanPair
    , InChan()
    , OutChan()
    -- * Write operations
    , writeChan
    , writeList2Chan
    -- * Read operations
    , readChan
    , getChanContents
    , dupChan     

    -- , chanPlus

    ) where

import qualified Control.Concurrent.Chan as C
import Data.Cofunctor


-- TODO: test performance of this with and without fmaped / cofmaped values in
-- comparison with standard Chan. Test to see if we can improve performance
-- using special constructor for fmaped / cofmaped version


-- | The "write side" of a chan pair
data InChan i where
    InChan :: C.Chan a -> (i -> a) -> InChan i

-- | The "read side" of a chan pair
data OutChan o where
    OutChan :: C.Chan a -> (a -> o) -> OutChan o

-- | Create corresponding read and write ends of a chan pair. Writes to the
-- 'InChan' side can be read on the 'OutChan' side.
newChanPair :: IO (InChan a, OutChan a)
newChanPair = undefined





-- | Write a value to an 'InChan'.
writeChan :: InChan a -> a -> IO ()
writeChan = undefined

-- | Write an entire list of items to an 'InChan'.
writeList2Chan :: InChan a -> [a] -> IO ()
writeList2Chan = undefined


-- | Read the next value from the 'OutChan'.
readChan :: OutChan a -> IO a
readChan = undefined

-- | Return a lazy list representing the contents of the supplied OutChan, much
-- like System.IO.hGetContents.
getChanContents :: OutChan a -> IO [a]
getChanContents = undefined



-- | Duplicate an 'OutChan': the duplicate channel begins empty, but data
-- written to the corresponding 'InChan' will appear in both, i.e. consuming a
-- value from the copy will have no affect on the values in the original
-- OutChan.
dupChan :: OutChan a -> IO (OutChan a)
dupChan = undefined
