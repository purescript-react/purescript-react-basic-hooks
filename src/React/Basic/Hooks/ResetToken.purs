module React.Basic.Hooks.ResetToken
  ( UseResetToken(..)
  , ResetToken
  , useResetToken
  ) where

import Prelude

import Data.Newtype (class Newtype)
import Effect (Effect)
import React.Basic.Hooks (type (/\), Hook, UseState, coerceHook, useState, (/\))
import React.Basic.Hooks as React

--| Useful for resetting effects or component state. A `ResetToken` can be
--| used alongside other hook dependencies to force a reevaluation of
--| whatever depends on those dependencies.
--|
--| For example, consider an effect or API call which depends on the state
--| of a search bar. You may want a button in the UI to refresh stale data.
--| In this case you would include a `ResetToken` in your search effect/aff's
--| dependencies and call `useResetToken`'s reset effect in the button's
--| `onClick` handler.
useResetToken :: Hook UseResetToken (ResetToken /\ (Effect Unit))
useResetToken =
  coerceHook React.do
    resetToken /\ setResetToken <- useState 0
    let reset = setResetToken (_ + 1)
    pure (ResetToken resetToken /\ reset)

newtype UseResetToken hooks
  = UseResetToken (UseState Int hooks)

derive instance ntUseResetToken :: Newtype (UseResetToken hooks) _

newtype ResetToken = ResetToken Int

derive newtype instance eqResetToken :: Eq ResetToken

instance showResetToken :: Show ResetToken where
  show (ResetToken token) = "(ResetToken " <> show token <> ")"
