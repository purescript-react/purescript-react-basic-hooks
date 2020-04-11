module React.Basic.Hooks.ResetToken
  ( UseResetToken
  , ResetToken
  , useResetToken
  ) where

import Prelude
import Data.Newtype (class Newtype)
import Effect (Effect)
import React.Basic.Hooks (type (/\), Hook, UseState, coerceHook, unsafeRenderEffect, useState, (/\))
import React.Basic.Hooks as React
import Unsafe.Coerce (unsafeCoerce)
import Unsafe.Reference (unsafeRefEq)

-- | Useful for resetting effects or component state. A `ResetToken` can be
-- | used alongside other hook dependencies to force a reevaluation of
-- | whatever depends on those dependencies.
-- |
-- | For example, an effect or API call which depends on the state of a
-- | search bar with filters. You may want a button in the UI either for UX
-- | reasons or to refresh possibly stale data. In this case you would
-- | include a `ResetToken` in your search effect/aff's dependencies and
-- | call run `useResetToken`'s reset effect in the button's `onClick`.
useResetToken :: Hook UseResetToken (ResetToken /\ (Effect Unit))
useResetToken =
  coerceHook React.do
    initialResetToken <- unsafeRenderEffect createResetToken
    resetToken /\ setResetToken <- useState initialResetToken
    let
      reset = do
        resetToken' <- createResetToken
        setResetToken \_ -> resetToken'
    pure (resetToken /\ reset)

newtype UseResetToken hooks
  = UseResetToken (UseState ResetToken hooks)

derive instance ntUseResetToken :: Newtype (UseResetToken hooks) _

foreign import data ResetToken :: Type

instance eqResetToken :: Eq ResetToken where
  eq = unsafeRefEq

createResetToken :: Effect ResetToken
createResetToken = unsafeCoerce \_ -> {}
