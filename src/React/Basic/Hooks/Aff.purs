module React.Basic.Hooks.Aff where

import Prelude
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect (Effect)
import Effect.Aff (Aff, error, killFiber, launchAff, launchAff_)
import Effect.Class (liftEffect)
import React.Basic.Hooks (Hook, UnsafeReference(..), UseEffect, UseReducer, UseState, type (/\), coerceHook, useEffect, useReducer, useState, (/\))
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

newtype UseAffReducer state action hooks
  = UseAffReducer
  ( UseEffect (UnsafeReference (Array (Aff (Array action))))
      (UseReducer { state :: state, effects :: Array (Aff (Array action)) } action hooks)
  )

derive instance ntUseAffReducer :: Newtype (UseAffReducer state action hooks) _

-- | Provide an initial state and a reducer function. This is a more powerful
-- | version of `useReducer`, where a state change can additionally queue
-- | asynchronous operations. The results of those operations must be  mapped
-- | into the reducer's `action` type. This is essentially the Elm architecture.
-- |
-- | *Note: Aff failures are thrown. If you need to capture an error state, be
-- |   sure to capture it in your action type!*
useAffReducer ::
  forall state action.
  state ->
  (state -> action -> { state :: state, effects :: Array (Aff (Array action)) }) ->
  Hook (UseAffReducer state action) (state /\ (action -> Effect Unit))
useAffReducer initialState reducer =
  coerceHook React.do
    { state, effects } /\ dispatch <-
      useReducer { state: initialState, effects: [] } (_.state >>> reducer)
    useEffect (UnsafeReference effects) do
      for_ effects \aff ->
        launchAff_ do
          actions <- aff
          liftEffect do for_ actions dispatch
      mempty
    pure (state /\ dispatch)
