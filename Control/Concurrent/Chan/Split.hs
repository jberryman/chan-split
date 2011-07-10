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

    ) where

import qualified Control.Concurrent.Chan as C
import Data.Cofunctor
import Control.Applicative
import Control.Arrow


-- TODO: test performance of this with and without fmaped / cofmaped values in
-- comparison with standard Chan. Test to see if we can improve performance
-- using special constructor for fmaped / cofmaped version


-- | The "write side" of a chan pair
data InChan i where
    InChan :: (i -> a) -> C.Chan a -> InChan i

-- | The "read side" of a chan pair
data OutChan o where
    OutChan :: (a -> o) -> C.Chan a -> OutChan o

-- | Create corresponding read and write ends of a chan pair. Writes to the
-- 'InChan' side can be read on the 'OutChan' side.
newChanPair :: IO (InChan a, OutChan a)
newChanPair = (InChan id &&& OutChan id) <$> C.newChan



-- | Write a value to an 'InChan'.
writeChan :: InChan a -> a -> IO ()
writeChan (InChan f c) = C.writeChan c . f

-- | Write an entire list of items to an 'InChan'.
writeList2Chan :: InChan a -> [a] -> IO ()
writeList2Chan (InChan f c) = C.writeList2Chan c . map f


-- | Read the next value from the 'OutChan'.
readChan :: OutChan a -> IO a
readChan (OutChan f c) = f <$> C.readChan c 

-- | Return a lazy list representing the contents of the supplied OutChan, much
-- like System.IO.hGetContents.
getChanContents :: OutChan a -> IO [a]
getChanContents (OutChan f c) = map f <$> C.getChanContents c



-- | Duplicate an 'OutChan': the duplicate channel begins empty, but data
-- written to the corresponding 'InChan' will appear in both, i.e. consuming a
-- value from the copy will have no affect on the values in the original
-- OutChan.
dupChan :: OutChan a -> IO (OutChan a)
dupChan (OutChan f c) = OutChan f <$> C.dupChan c

{-
-- | EXPERIMENTAL: combine multiple output chans, interleaving their values
mergeOutChans :: [OutChan a] -> IO (OutChan a)
mergeOutChans cs = 
    as <- mapM C.getChanContents cs
    ...
    -}

instance Cofunctor InChan where
    cofmap f' (InChan f c) = InChan (f . f') c

instance Functor OutChan where
    fmap f' (OutChan f c) = OutChan (f' . f) c
