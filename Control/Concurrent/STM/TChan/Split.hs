{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Control.Concurrent.STM.TChan.Split (
    -- | Much of the STM chan functionality exists in the 'SplitTChan' and class,
    -- which you should see for documentation.
	
	-- * TChan pairs
	InTChan,
	OutTChan,

    -- ** Construction
    -- | See also the 'NewSplitTChan' class.
	
	--newBroadcastTChan, --replaced by...
	newInTChan,
	-- *** In IO
	newSplitTChanIO,
	--newBroadcastTChanIO, --replaced by...
	newInTChanIO,

    -- ** Putting values back
    unGetTChan,
    
    -- ** Duplication
    dupTChan,
    cloneTChan
  ) where

import GHC.Conc

import Data.Typeable (Typeable)

import Control.Concurrent.STM.TChan.Class

-- | The input side of an unbounded FIFO channel.
newtype InTChan a = InTChan (TVar (TVarList a))
  deriving (Eq, Typeable)

-- | The output side of an unbounded FIFO channel.
newtype OutTChan a = OutTChan (TVar (TVarList a))
  deriving (Eq, Typeable)

type TVarList a = TVar (TList a)
data TList a = TNil | TCons a {-# UNPACK #-} !(TVarList a)

instance NewSplitTChan InTChan OutTChan where 
    newSplitTChan = do
      hole <- newTVar TNil
      read <- newTVar hole
      write <- newTVar hole
      return (InTChan write, OutTChan read)

instance SplitTChan InTChan OutTChan where
    writeTChan (InTChan write) a = do
      listend <- readTVar write -- listend == TVar pointing to TNil
      new_listend <- newTVar TNil
      writeTVar listend (TCons a new_listend)
      writeTVar write new_listend

    readTChan (OutTChan read) = do
      listhead <- readTVar read
      head <- readTVar listhead
      case head of
        TNil -> retry
        TCons a tail -> do
        writeTVar read tail
        return a

    tryReadTChan (OutTChan read) = do
      listhead <- readTVar read
      head <- readTVar listhead
      case head of
        TNil       -> return Nothing
        TCons a tl -> do
          writeTVar read tl
          return (Just a)

    peekTChan (OutTChan read) = do
      listhead <- readTVar read
      head <- readTVar listhead
      case head of
        TNil      -> retry
        TCons a _ -> return a

    tryPeekTChan (OutTChan read) = do
      listhead <- readTVar read
      head <- readTVar listhead
      case head of
        TNil      -> return Nothing
        TCons a _ -> return (Just a)

    isEmptyTChan (OutTChan read) = do
      listhead <- readTVar read
      head <- readTVar listhead
      case head of
        TNil -> return True
        TCons _ _ -> return False


-- |@IO@ version of 'newTChan'.  This is useful for creating top-level
-- 'TChan's using 'System.IO.Unsafe.unsafePerformIO', because using
-- 'atomically' inside 'System.IO.Unsafe.unsafePerformIO' isn't
-- possible.
newSplitTChanIO :: IO (OutTChan a, InTChan a)
newSplitTChanIO = do
  hole <- newTVarIO TNil
  read <- newTVarIO hole
  write <- newTVarIO hole
  return (OutTChan read, InTChan write)


-- -- | Create a write-only 'TChan'.  More precisely, 'readTChan' will 'retry'
-- -- even after items have been written to the channel.  The only way to read
-- -- a broadcast channel is to duplicate it with 'dupTChan'.
-- --
-- -- Consider a server that broadcasts messages to clients:
-- --
-- -- >serve :: TChan Message -> Client -> IO loop
-- -- >serve broadcastChan client = do
-- -- >    myChan <- dupTChan broadcastChan
-- -- >    forever $ do
-- -- >        message <- readTChan myChan
-- -- >        send client message
-- --
-- -- The problem with using 'newTChan' to create the broadcast channel is that if
-- -- it is only written to and never read, items will pile up in memory.  By
-- -- using 'newBroadcastTChan' to create the broadcast channel, items can be
-- -- garbage collected after clients have seen them.
-- newBroadcastTChan :: STM (TChan a)
-- newBroadcastTChan = do
--     dummy_hole <- newTVar TNil
--     write_hole <- newTVar TNil
--     read <- newTVar dummy_hole
--     write <- newTVar write_hole
--     return (TChan read write)

-- -- | @IO@ version of 'newBroadcastTChan'.
-- newBroadcastTChanIO :: IO (TChan a)
-- newBroadcastTChanIO = do
--     dummy_hole <- newTVarIO TNil
--     write_hole <- newTVarIO TNil
--     read <- newTVarIO dummy_hole
--     write <- newTVarIO write_hole
--     return (TChan read write)

-- ------------------------------
-- NOTE: functions above are useful, but description of usage isn't relevant I don't 
-- think; messages should be garbage collected once the read end is GC'd (i.e.
-- in the scenarion where there are only writers and no readers.
--
-- The equivalent functions are still useful to have though:
-- ------------------------------

-- | Create a new write end of a TChan. Use 'dupTChan' to get an 'OutChan' that
-- values can be read from.
newInTChan :: STM (InTChan a)
newInTChan = do
    write_hole <- newTVar TNil
    write <- newTVar write_hole
    return (InTChan write)


-- | @IO@ version of 'newInTChan'.
newInTChanIO :: IO (InTChan a)
newInTChanIO = do
    write_hole <- newTVarIO TNil
    write <- newTVarIO write_hole
    return (InTChan write)

-- |Create a duplicate 'OutChan' from an 'InChan'. The 'OutChan' starts 
-- empty but will receive a copy of all subsequent values written.
dupTChan :: InTChan a -> STM (OutTChan a)
dupTChan (InTChan write) = do
  hole <- readTVar write  
  new_read <- newTVar hole
  return (OutTChan new_read)

-- |Put a data item back onto a channel, where it will be the next item read.
unGetTChan :: OutTChan a -> a -> STM ()
unGetTChan (OutTChan read) a = do
   listhead <- readTVar read
   newhead <- newTVar (TCons a listhead)
   writeTVar read newhead

-- |Clone a 'TChan': similar to dupTChan, but the cloned channel starts with the
-- same content available as the original channel.
cloneTChan :: OutTChan a -> STM (OutTChan a)
cloneTChan (OutTChan read) = do
  readpos <- readTVar read
  new_read <- newTVar readpos
  return (OutTChan new_read)
