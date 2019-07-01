module Hasql.Transaction.Transaction.Transaction where

import Hasql.Transaction.Prelude
import Hasql.Transaction.Requisites.Model
import Hasql.Session (Session)
import Hasql.Statement (Statement)
import qualified Hasql.Session as Session


{-|
Composable transaction providing for automated conflict resolution
with input @i@ and output @o@.

Mode and level is associated with the transaction,
which makes them a part of the composition as well.
In a composed transaction they become the strictest of the ones
associated with the transactions that constitute it.
-}
data Transaction i o =
  Transaction Mode Level (i -> StateT Condemnation Session o)

deriving instance Functor (Transaction i)

instance Applicative (Transaction i) where
  pure = Transaction Read ReadCommitted . const . pure
  (<*>) = binOp $ \ lSession rSession i -> lSession i <*> rSession i

instance Profunctor Transaction where
  dimap fn1 fn2 = mapSession $ \ session -> fmap fn2 . session . fn1

instance Strong Transaction where
  first' = first
  second' = second

instance Choice Transaction where
  left' = left
  right' = right

instance Semigroupoid Transaction where
  o = binOp (<=<)

instance Category Transaction where
  id = Transaction Read ReadCommitted return
  (.) = o

instance Arrow Transaction where
  arr fn = Transaction Read ReadCommitted (return . fn)
  (***) = binOp $ \ lSession rSession (li, ri) -> (,) <$> lSession li <*> rSession ri

instance ArrowChoice Transaction where
  (+++) = binOp $ \ lSession rSession -> either (fmap Left . lSession) (fmap Right . rSession)

{-|
Because mode and isolation are always composed the same way,
we can focus on composing just the sessions.
-}
{-# INLINE binOp #-}
binOp ::
  (
    (li -> StateT Condemnation Session lo) ->
    (ri -> StateT Condemnation Session ro) ->
    (i -> StateT Condemnation Session o)
  ) ->
  Transaction li lo -> Transaction ri ro -> Transaction i o
binOp composeSessions (Transaction lMode lIsolation lSession) (Transaction rMode rIsolation rSession) = let
  mode = max lMode rMode
  isolation = max lIsolation rIsolation
  session = composeSessions lSession rSession
  in Transaction mode isolation session

{-# INLINE mapSession #-}
mapSession ::
  ((i1 -> StateT Condemnation Session o1) -> (i2 -> StateT Condemnation Session o2)) ->
  Transaction i1 o1 -> Transaction i2 o2
mapSession sessionFn (Transaction mode isolation session) = Transaction mode isolation (sessionFn session)

{-|
Cause transaction to eventually roll back.

This allows to perform some transactional actions,
collecting their results, and decide,
whether to commit the introduced changes to the DB
based on those results,
as well as emit those results outside of the transaction.
-}
condemn :: Transaction () ()
condemn = Transaction Read ReadCommitted (\ _ -> put Condemned)

{-|
Execute a possibly multistatement SQL string under a mode and level.
SQL strings cannot be dynamically parameterized or produce a result.

__Warning:__ SQL must not be transaction-related
like `BEGIN`, `COMMIT` or `ABORT`, otherwise you'll break the abstraction.
-}
sql :: Mode -> Level -> ByteString -> Transaction () ()
sql mode level sql = session mode level (\ _ -> Session.sql sql)

{-|
Execute a single statement under a mode and level.

__Warning:__ The statement must not be transaction-related
like `BEGIN`, `COMMIT` or `ABORT`, otherwise you'll break the abstraction.
-}
statement :: Mode -> Level -> Statement i o -> Transaction i o
statement mode level statement = session mode level (\ input -> Session.statement input statement)

{-|
Execute a composition of statements under the same mode and level.

__Warning:__

1. You must know that it is possible to break the abstraction,
if you execute statements such as `BEGIN` inside of the session.

1. For the same reason you cannot execute other transactions inside of that session.

1. You must beware that in case of conflicts any IO code that you may lift
into session will get executed multiple times.
This is the way the automatic conflict resolution works.
So be cautious about doing any mutations or rocket launches in that IO!
Simply pinging for things such as current time is totally fine though.

Due to the mentioned it's highly advised to keep all the session code
inside of the definition of a transaction.
Thus you'll be guaranteed to have control over what's going on inside of the
executed session and it will not be possible for this code
to be affected by any outside changes or used elsewhere.
-}
session :: Mode -> Level -> (i -> Session o) -> Transaction i o
session mode level session = Transaction mode level (lift . session)