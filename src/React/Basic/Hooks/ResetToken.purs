module React.Basic.Hooks.ResetToken
  ( UseResetToken
  , ResetToken
  , useResetToken
  ) where

import Prelude

import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import React.Basic.Hooks (Hook, UseState, type (/\), useState, (/\))
import React.Basic.Hooks as React
import Unsafe.Coerce (unsafeCoerce)
import Unsafe.Reference (unsafeRefEq)

type UseResetToken = UseState ResetToken

-- Useful for resetting effects or component state in place of a
-- real key, i.e. a "reset" or "force rerender" button.
useResetToken :: Hook UseResetToken (ResetToken /\ (Effect Unit))
useResetToken = React.do
  resetToken /\ setResetToken <- useState initialResetToken
  let
    reset = do
      resetToken' <- createResetToken
      setResetToken \_ -> resetToken'
  pure (resetToken /\ reset)

foreign import data ResetToken :: Type

instance eqResetToken :: Eq ResetToken where
  eq = unsafeRefEq

createResetToken :: Effect ResetToken
createResetToken = unsafeCoerce \_ -> {}

initialResetToken :: ResetToken
initialResetToken = unsafePerformEffect createResetToken