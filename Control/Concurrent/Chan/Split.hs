{-# LANGUAGE MultiParamTypeClasses, DeriveDataTypeable #-}
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

--
-- inspired by Leon P. Smith's from-scratch implementation
-- copy-pasta from Control.Concurrent.Chan
--

import System.IO.Unsafe ( unsafeInterleaveIO )
import Control.Concurrent.MVar
import Control.Exception (mask_)
import Data.Typeable

import Control.Concurrent.Chan.Class

type Stream a = MVar (ChItem a)
data ChItem a = ChItem a (Stream a)

-- | The \"write side\" of a chan pair
newtype InChan i = InChan (MVar (Stream i)) -- Invariant: Stream i always empty
    deriving (Eq, Typeable)

-- | The \"read side\" of a chan pair
newtype OutChan i = OutChan (MVar (Stream i)) 
    deriving (Eq, Typeable)

instance NewSplitChan InChan OutChan where
    -- | Create corresponding read and write ends of a chan pair. Writes to the
    -- 'InChan' side can be read on the 'OutChan' side.
    newSplitChan = do
       hole  <- newEmptyMVar
       readVar  <- newMVar hole
       writeVar <- newMVar hole
       return ( InChan writeVar, OutChan readVar )


instance SplitChan InChan OutChan where
    writeChan (InChan writeVar) val = do
          new_hole <- newEmptyMVar
          mask_ $ do
            old_hole <- takeMVar writeVar
            putMVar old_hole (ChItem val new_hole)
            putMVar writeVar new_hole

    writeList2Chan ch ls = sequence_ (map (writeChan ch) ls)

    readChan (OutChan readVar) = do
          modifyMVar readVar $ \read_end -> do
            (ChItem val new_read_end) <- readMVar read_end
                -- Use readMVar here, not takeMVar,
                -- else dupChan doesn't work
            return (new_read_end, val)


-- | Return a lazy list representing the contents of the supplied OutChan, much
-- like System.IO.hGetContents.
getChanContents :: OutChan a -> IO [a]
getChanContents ch = unsafeInterleaveIO (do
                            x  <- readChan ch
                            xs <- getChanContents ch
                            return (x:xs)
                        )

-- | Duplicate an 'OutChan': the duplicate channel contains any unread messages
-- in the original (n.b. this differs from the behavior of dupChan in Chan),
-- and data written to the corresponding 'InChan' will appear in both, i.e.
-- consuming a value from the copy will have no affect on the values in the
-- original OutChan.
--
-- (Note that a duplicated channel is not equal to its original.
-- So: @fmap (c /=) $ dupChan c@ returns @True@ for all @c@.)
dupChan :: OutChan a -> IO (OutChan a)
dupChan (OutChan writeVar) = OutChan `fmap` withMVar writeVar newMVar
