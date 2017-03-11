{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
import Control.Monad.Classes hiding (exec)
import Control.Monad.Classes.Log
import Control.Monad.Log (LoggingT, runLoggingT)
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.State (State, execState)

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain $ testGroup "tests" [testCase "should compile" testcase
                                       ,testCase "writer impl" writecase]
  where testcase  = assertEqual "correct results" opresults (exec op)
        writecase = assertEqual "correct writes"  opresults (exec opw)
        opresults = [RRes "bar", LRes "foo"]

data Foo = Foo String
data Bar = Bar String

data Res = LRes String | RRes String
  deriving (Eq, Ord, Show)

logit :: (MonadReader a m, MonadLog a m) => (a -> a) -> m ()
logit f = ask >>= logMessage . f

-- | This is the operation that will not compile under mtl
-- | due to functional dependencies.
op :: (MonadReader Foo m, MonadReader Bar m, MonadLog Foo m, MonadLog Bar m) => m ()
op = logit (id @Foo) >> logit (id @Bar)

-- | handler for Foo
foo :: MonadState [Res] m => Foo -> m ()
foo (Foo s) = modify $ \r -> LRes s : r

-- | Handler for Bar
bar :: MonadState [Res] m => Bar -> m ()
bar (Bar s) = modify $ \r -> RRes s : r

exec :: ReaderT Foo (ReaderT Bar (LoggingT Foo (LoggingT Bar (State [Res])))) () -> [Res]
exec m = execState (runLoggingT (runLoggingT (runReaderT (runReaderT m (Foo "foo")) (Bar "bar")) foo) bar) []

writeit :: (MonadReader a m, MonadWriter a m) => (a -> a) -> m ()
writeit f = ask >>= tell . f

opw :: (MonadReader Foo m, MonadReader Bar m, MonadWriter Foo m, MonadWriter Bar m) => m ()
opw = writeit (id @Foo) >> writeit (id @Bar)
