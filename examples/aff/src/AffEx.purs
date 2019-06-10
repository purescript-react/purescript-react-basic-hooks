module AffEx where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, Milliseconds(..), delay, error, message, throwError)
import React.Basic.DOM as R
import React.Basic.Events (handler_)
import React.Basic.Hooks (CreateComponent, component, fragment, (/\))
import React.Basic.Hooks as React
import React.Basic.Hooks.Aff (useAff)
import React.Basic.Hooks.ResetToken (useResetToken)

mkAffEx :: CreateComponent {}
mkAffEx = do
  component "AffEx" \props -> React.do
    let id = 0 -- pretend this is a prop
    resetToken /\ reset <- useResetToken
    r1 <- useAff (id /\ resetToken) delayedSuccess
    r2 <- useAff (id /\ resetToken) delayedFailure

    pure $ fragment
      [ R.button
          { onClick: handler_ reset
          , children: [ R.text "Reset" ]
          }
      , showResult 1 r1
      , showResult 2 r2
      ]
  where
    showResult n r =
      R.div_
        [ R.text $ "Request " <> show n <> ": " <> case r of
            Nothing -> "loading..."
            Just (Left err) -> message err
            Just (Right msg) -> msg
        ]

delayedSuccess :: Aff String
delayedSuccess = do
  delay $ Milliseconds 1000.0
  pure "Success!"

delayedFailure :: Aff String
delayedFailure = do
  delay $ Milliseconds 2000.0
  throwError $ error "Failure!"