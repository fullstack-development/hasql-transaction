# 1.0.2

- Use `MonadMask` for opening and closing the transaction.
- Make `Transaction` a transformer.
- Add `unsafeLift` and `unsafeRunIO`.
- Add `abandon`.
- Make use of the `MonadSession` typeclass for `sql` and `statement`.
