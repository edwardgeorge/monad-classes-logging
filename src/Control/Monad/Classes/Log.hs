{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts,
             FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Control.Monad.Classes.Log where
import Control.Monad.Classes
import Control.Monad.Log hiding (MonadLog(..),
                                 logMessage,
                                 mapLogMessage,
                                 mapLogMessageM,
                                 logDebug,
                                 logInfo,
                                 logNotice,
                                 logWarning,
                                 logError,
                                 logCritical,
                                 logAlert,
                                 logEmergency)
import qualified Control.Monad.Log as Log
import Control.Monad.Trans.Class (MonadTrans(..))
import GHC.Prim (proxy#, Proxy#)

data EffLog (w :: *)

type instance CanDo (LoggingT msg m) eff = LoggingCanDo msg eff
type instance CanDo (PureLoggingT msg m) eff = LoggingCanDo msg eff
type instance CanDo (DiscardLoggingT msg m) eff = LoggingCanDo msg eff

type family LoggingCanDo msg eff where
  LoggingCanDo msg (EffLog msg) = 'True
  LoggingCanDo msg eff          = 'False

#ifdef USE_FEUERBACH
class Monad m => MonadLogN (k :: Nat) message m where
#else
class Monad m => MonadLogN (k :: Peano) message m where
#endif
  logMessageFreeN :: Proxy# k -> (forall n. Monoid n => (message -> n) -> n) -> m ()

type MonadLog msg m = MonadLogN (Find (EffLog msg) m) msg m

instance Monad m => MonadLogN 'Zero msg (LoggingT msg m) where
  logMessageFreeN _ = Log.logMessageFree
  {-# INLINABLE logMessageFreeN #-}

instance (Monad m, Monoid msg) => MonadLogN 'Zero msg (PureLoggingT msg m) where
  logMessageFreeN _ = Log.logMessageFree
  {-# INLINABLE logMessageFreeN #-}

instance Monad m => MonadLogN 'Zero msg (DiscardLoggingT msg m) where
  logMessageFreeN _ = Log.logMessageFree
  {-# INLINABLE logMessageFreeN #-}

#ifdef USE_FEUERBACH
instance (MonadTrans t, MonadLogN k msg m, Monad (t m)) => MonadLogN ('Suc k) msg (t m) where
#else
instance (MonadTrans t, MonadLogN k msg m, Monad (t m)) => MonadLogN ('Succ k) msg (t m) where
#endif
  logMessageFreeN _ f = lift $ logMessageFreeN (proxy# :: Proxy# k) f
  {-# INLINABLE logMessageFreeN #-}

logMessageFree :: forall msg m. MonadLog msg m => (forall n. Monoid n => (msg -> n) -> n) -> m ()
logMessageFree = logMessageFreeN (proxy# :: Proxy# (Find (EffLog msg) m))
{-# INLINABLE logMessageFree #-}

logMessage :: MonadLog msg m => msg -> m ()
logMessage m = logMessageFree (\inject -> inject m)
{-# INLINABLE logMessage #-}

mapLogMessage :: MonadLog msg' m => (msg -> msg') -> LoggingT msg m a -> m a
mapLogMessage f m = runLoggingT m (logMessage . f)
{-# INLINABLE mapLogMessage #-}

mapLogMessageM :: MonadLog msg' m => (msg -> m msg') -> LoggingT msg m a -> m a
mapLogMessageM f m = runLoggingT m ((>>= logMessage) . f)
{-# INLINABLE mapLogMessageM #-}

logDebug :: MonadLog (WithSeverity a) m => a -> m ()
logDebug = logMessage . WithSeverity Debug
{-# INLINEABLE logDebug #-}

logInfo :: MonadLog (WithSeverity a) m => a -> m ()
logInfo      = logMessage . WithSeverity Informational
{-# INLINEABLE logInfo #-}

logNotice :: MonadLog (WithSeverity a) m => a -> m ()
logNotice    = logMessage . WithSeverity Notice
{-# INLINEABLE logNotice #-}

logWarning :: MonadLog (WithSeverity a) m => a -> m ()
logWarning   = logMessage . WithSeverity Warning
{-# INLINEABLE logWarning #-}

logError :: MonadLog (WithSeverity a) m => a -> m ()
logError     = logMessage . WithSeverity Error
{-# INLINEABLE logError #-}

logCritical :: MonadLog (WithSeverity a) m => a -> m ()
logCritical  = logMessage . WithSeverity Critical
{-# INLINEABLE logCritical #-}

logAlert :: MonadLog (WithSeverity a) m => a -> m ()
logAlert     = logMessage . WithSeverity Alert
{-# INLINEABLE logAlert #-}

logEmergency :: MonadLog (WithSeverity a) m => a -> m ()
logEmergency = logMessage . WithSeverity Emergency
{-# INLINEABLE logEmergency #-}

-- MonadWriter instances

instance Monad m => MonadWriterN 'Zero msg (LoggingT msg m) where
  tellN _ msg = logMessage msg

instance (Monad m, Monoid msg) => MonadWriterN 'Zero msg (PureLoggingT msg m) where
  tellN _ msg = logMessage msg

instance Monad m => MonadWriterN 'Zero msg (DiscardLoggingT msg m) where
  tellN _ msg = logMessage msg
