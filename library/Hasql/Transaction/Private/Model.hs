module Hasql.Transaction.Private.Model
where

import Hasql.Transaction.Private.Prelude

-- |
--
data Mode =
  -- |
  -- Read-only. No writes possible.
  Read |
  -- |
  -- Write and commit.
  Write
  deriving (Show, Eq, Ord, Enum, Bounded)

-- |
-- For reference see
-- <http://www.postgresql.org/docs/current/static/transaction-iso.html the Postgres' documentation>.
--
data IsolationLevel =
  ReadCommitted |
  RepeatableRead |
  Serializable
  deriving (Show, Eq, Ord, Enum, Bounded)

-- |
-- What to do at the end of the transaction.
data TransactionEnd =
  -- |
  -- Commit the transaction and exit.
  EndCommit |
  -- |
  -- Rollback the transaction and try again.
  EndRetry |
  -- |
  -- Rollback the transaction and exit.
  EndAbandon
  deriving (Show, Eq, Ord, Enum, Bounded)
