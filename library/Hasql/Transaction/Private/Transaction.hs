module Hasql.Transaction.Private.Transaction
where

import Hasql.Transaction.Private.Prelude
import Hasql.Transaction.Private.Model
import qualified Control.Monad.Catch as Catch
import qualified Hasql.Statement as A
import qualified Hasql.Session as B
import qualified Hasql.Transaction.Private.Statements as C
import qualified Hasql.Transaction.Private.Sessions as D


-- |
-- A composable abstraction over the retryable transactions.
--
-- Executes multiple queries under the specified mode and isolation level,
-- while automatically retrying the transaction in case of conflicts.
-- Thus this abstraction closely reproduces the behaviour of 'STM'.
newtype Transaction a =
  Transaction (StateT TransactionEnd B.Session a)
  deriving (Functor, Applicative, Monad, Catch.MonadThrow, Catch.MonadCatch, Catch.MonadMask)

instance Semigroup a => Semigroup (Transaction a) where
  (<>) = liftA2 (<>)

instance Monoid a => Monoid (Transaction a) where
  mempty = pure mempty
  mappend = liftA2 mappend

defaultMaximumRetries :: Int
defaultMaximumRetries = maxBound

-- |
-- Execute the transaction using the provided isolation level and mode.
--
-- If the given `Transaction` exits with an exception, the transaction is
-- rolled back.
{-# INLINE run #-}
run :: Transaction a -> IsolationLevel -> Mode -> Bool -> B.Session a
run transaction isolation mode preparable =
  runAtMost transaction isolation mode preparable defaultMaximumRetries

-- |
-- Execute the transaction using the provided isolation level, mode and the
-- maximum number of retries.
--
-- If the given `Transaction` exits with an exception, the transaction is
-- rolled back.
--
-- If we couldn't commit within the specified number of tries, this function
-- fails with a serialization error.
{-# INLINE runAtMost #-}
runAtMost :: Transaction a -> IsolationLevel -> Mode -> Bool -> Int -> B.Session a
runAtMost (Transaction session) isolation mode preparable maxRetries =
  D.inRetryingTransaction isolation mode (runStateT session EndCommit) preparable maxRetries

-- |
-- Possibly a multi-statement query,
-- which however cannot be parameterized or prepared,
-- nor can any results of it be collected.
{-# INLINE sql #-}
sql :: ByteString -> Transaction ()
sql =
  Transaction . lift . B.sql

-- |
-- Parameters and a specification of the parametric query to apply them to.
{-# INLINE statement #-}
statement :: a -> A.Statement a b -> Transaction b
statement params statement =
  Transaction . lift $ B.statement params statement

-- |
-- Cause transaction to ultimately roll back and retry.
{-# INLINE condemn #-}
condemn :: Transaction ()
condemn =
  Transaction $ put EndRetry

-- |
-- Cause transaction to ultimately roll back and exit.
{-# INLINE abandon #-}
abandon :: Transaction ()
abandon =
  Transaction $ put EndAbandon

-- |
-- Unsafely perform IO in the Transaction monad.
--
-- Most of the warnings for `GHC.Conc.unsafeIOToSTM` also apply to this
-- function:
--
--   * The transaction code may be re-run multiple times, so you need to be
--     prepared for this if your IO has any side effects.
--
--   * The data that was fetched from the database may turn out to be
--     inconsistent. Invariants that you expect to be true (whether by
--     convention or due to an explicit database constraint) may not be true
--     inside the transaction.
--
--   * Any writes that were performed inside the transaction may be later
--     rolled back, so you cannot rely on anything being stored in the database
--     either.
--
-- Unlike `GHC.Conc.unsafeIOToSTM`, however, we don't abort the transaction
-- by completely dropping its execution, so the exception handlers will work
-- as usual, and `Control.Monad.Catch.bracket` can be safely relied upon for
-- proper resource management.
{-# INLINE unsafeRunIO #-}
unsafeRunIO :: IO a -> Transaction a
unsafeRunIO io =
  Transaction $ liftIO io
