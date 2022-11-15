module Hasql.Transaction.Private.Sessions
where

import Hasql.Transaction.Private.Prelude
import Hasql.Transaction.Private.Model
import Hasql.Session
import qualified Control.Monad.Catch as Catch
import qualified Hasql.Transaction.Private.Statements as Statements


{-
We may want to
do one transaction retry in case of the 23505 error, and fail if an identical
error is seen.
-}
inRetryingTransaction :: IsolationLevel -> Mode -> Session (a, TransactionEnd) -> Bool -> Int -> Session a
inRetryingTransaction level mode session preparable = go
  where
    go retriesLeft = do
      attemptRes <- tryTransaction level mode session preparable
      case attemptRes of
        Just a -> return a
        Nothing ->
          if retriesLeft >= 0
            then go (retriesLeft - 1)
            else throwError retriesExhaustedError

tryTransaction :: IsolationLevel -> Mode -> Session (a, TransactionEnd) -> Bool -> Session (Maybe a)
tryTransaction level mode body preparable =
  fmap snd $
  Catch.generalBracket initTransaction closeTransaction insideTransaction
  where
    initTransaction =
      begin

    closeTransaction () exitCase =
      case exitCase of
        Catch.ExitCaseSuccess (Just (result, EndCommit)) -> trySerializableMaybe (commitWith result)
        Catch.ExitCaseSuccess (Just (result, EndAbandon)) -> abortWith (Just result)
        _ -> abortWith Nothing

    insideTransaction () =
        trySerializableMaybe body

    begin =
      statement () (Statements.beginTransaction level mode preparable)

    commitWith result =
      result <$ statement () (Statements.commitTransaction preparable)

    abortWith result = do
      result <$ statement () (Statements.abortTransaction preparable)

isSerializationError error = case error of
  QueryError _ _ (ResultError (ServerError "40001" _ _ _ _)) -> True
  _ -> False

trySerializableMaybe body =
  catchError (fmap Just body) $ \error ->
    if isSerializationError error
      then pure Nothing
      else throwError error

handleTransactionError error =
  if isSerializationError error
    then pure Nothing
    else throwError error

retriesExhaustedError :: QueryError
retriesExhaustedError =
  QueryError "" [] (ResultError (UnexpectedResult "Failed to commit a transaction within the alloted number of tries"))
