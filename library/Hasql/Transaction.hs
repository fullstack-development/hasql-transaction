-- |
-- An API for declaration of transactions.
module Hasql.Transaction
(
  -- * Transaction monad
  TransactionT,
  Transaction,
  condemn,
  abandon,
  MonadSession (..),
)
where

import Hasql.Session
import Hasql.Transaction.Private.Transaction
import Hasql.Transaction.Private.Model
