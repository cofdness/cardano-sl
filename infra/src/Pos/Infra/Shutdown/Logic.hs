module Pos.Infra.Shutdown.Logic
       ( triggerShutdown
       , waitForShutdown
       ) where

import           Universum

import           Control.Concurrent.STM (check, readTVar, writeTVar)

import           Pos.Infra.Shutdown.Class (HasShutdownContext (..))
import           Pos.Infra.Shutdown.Types (ShutdownContext (..),
                     shdnIsTriggered)
import           Pos.Util.Wlog (WithLogger, logInfo, logError)

import           Control.Monad.IO.Class (liftIO)
import           System.Environment (lookupEnv)

triggerShutdown
    :: (MonadIO m, MonadReader ctx m, WithLogger m, HasShutdownContext ctx)
    => m ()
triggerShutdown = do
    mMisIgnoreShutdown <- liftIO $ lookupEnv "CARDANO_INJECT_FAILURE_IGNORE_SHUTDOWN"
    case mMisIgnoreShutdown of
      Just _  -> logError "Injected failure: CARDANO_INJECT_FAILURE_IGNORE_SHUTDOWN"
      Nothing -> do
        logInfo "NODE SHUTDOWN TRIGGERED, WAITING FOR WORKERS TO TERMINATE"
        view (shutdownContext . shdnIsTriggered) >>= atomically . flip writeTVar True

-- | Wait for the shutdown var to be true.
waitForShutdown :: ShutdownContext -> IO ()
waitForShutdown (ShutdownContext v) = atomically (readTVar v >>= check)
