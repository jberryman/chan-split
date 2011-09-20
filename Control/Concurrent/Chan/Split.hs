{-# LANGUAGE GADTs, MultiParamTypeClasses #-}
module Control.Concurrent.Chan.Split (
    -- * Chan pairs
      InChan()
    , OutChan()
    -- * Utility functions:
    , getChanContents
    , dupChan     
    -- * Supporting module
    , module Control.Concurrent.Chan.Class

    ) where

import qualified Control.Concurrent.Chan as C
import Data.Functor.Contravariant
import Control.Applicative
import Control.Arrow
import Control.Concurrent.Chan.Class



-- TODO: test performance of this with and without fmaped / contramap values in
-- comparison with standard Chan. Test to see if we can improve performance
-- using special constructor for fmaped / contramap version


-- | The "write side" of a chan pair
data InChan i where
    InChan :: (i -> a) -> C.Chan a -> InChan i

-- | The "read side" of a chan pair
data OutChan o where
    OutChan :: (a -> o) -> C.Chan a -> OutChan o

instance NewSplitChan InChan OutChan where
    -- | Create corresponding read and write ends of a chan pair. Writes to the
    -- 'InChan' side can be read on the 'OutChan' side.
    newSplitChan = (InChan id &&& OutChan id) <$> C.newChan


instance SplitChan InChan OutChan where
    writeChan (InChan f c) = C.writeChan c . f
    writeList2Chan (InChan f c) = C.writeList2Chan c . map f
    readChan (OutChan f c) = f <$> C.readChan c 

instance Contravariant InChan where
    contramap f' (InChan f c) = InChan (f . f') c

instance Functor OutChan where
    fmap f' (OutChan f c) = OutChan (f' . f) c




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

