module React.Basic.Hooks.Aff where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect.Aff (Aff, error, killFiber, launchAff, launchAff_)
import Effect.Class (liftEffect)
import React.Basic.Hooks (Hook, UseEffect, UseState, coerceHook, useEffect, useState, (/\))
import React.Basic.Hooks as React

newtype UseAff key a hooks
  = UseAff (UseEffect key (UseState (Maybe a) hooks))

derive instance ntUseAff :: Newtype (UseAff key a hooks) _

-- | `useAff` is used for asynchronous effects or `Aff`. The asynchronous effect
-- | is re-run whenever the key changes. If another `Aff` runs when the key
-- | changes before the previous async resolves, it will cancel the previous
-- | in-flight effect.
-- |
-- | *Note: Aff failures are thrown. If you need to capture an error state, be
-- |   sure to capture it in your data type!*
useAff ::
  forall key a.
  Eq key =>
  key ->
  Aff a ->
  Hook (UseAff key a) (Maybe a)
useAff key aff =
  coerceHook React.do
    result /\ setResult <- useState Nothing
    useEffect key do
      setResult (const Nothing)
      fiber <-
        launchAff do
          r <- aff
          liftEffect do
            setResult \_ -> Just r
      pure do
        launchAff_ do
          killFiber (error "Effect hook discarded.") fiber
    pure result
