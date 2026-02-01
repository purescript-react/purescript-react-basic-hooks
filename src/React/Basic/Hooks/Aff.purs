module React.Basic.Hooks.Aff
  ( useAff
  , useSteppingAff
  , UseAff(..)
  , useAffReducer
  , AffReducer
  , mkAffReducer
  , runAffReducer
  , noEffects
  , UseAffReducer(..)
  , useAffActionState
  , useAffActionStateWithPermalink
  , UseAffActionState(..)
  ) where

import Prelude

import Control.Promise (Promise, fromAff)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Function.Uncurried (Fn2, Fn3, mkFn2, mkFn3, runFn2)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Nullable (Nullable, toNullable)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, Error, error, killFiber, launchAff, launchAff_, throwError, try)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn3, EffectFn4, runEffectFn3, runEffectFn4)
import Effect.Unsafe (unsafePerformEffect)
import React.Basic.Hooks (type (&), type (/\), Hook, Reducer, UnsafeReference(..), UseActionState, UseEffect, UseMemo, UseReducer, UseState, coerceHook, mkReducer, unsafeHook, unsafeRenderEffect, useEffect, useMemo, useReducer, useState, (/\))
import React.Basic.Hooks as React

--| `useAff` is used for asynchronous effects or `Aff`. The asynchronous effect
--| is re-run whenever the deps change. If another `Aff` runs when the deps
--| change before the previous async resolves, it will cancel the previous
--| in-flight effect.
--|
--| *Note: This hook requires parent components to handle error states! Don't
--|   forget to implement a React error boundary or avoid `Aff` errors entirely
--|   by incorporating them into your result type!*
useAff ::
  forall deps a.
  Eq deps =>
  deps ->
  Aff a ->
  Hook (UseAff deps a) (Maybe a)
useAff = useAff' (const Nothing)

--| A variant of `useAff` where the asynchronous effect's result is preserved up
--| until the next run of the asynchronous effect _completes_.
--|
--| Contrast this with `useAff`, where the asynchronous effect's result is
--| preserved only up until the next run of the asynchronous effect _starts_.
useSteppingAff ::
  forall deps a.
  Eq deps =>
  deps ->
  Aff a ->
  Hook (UseAff deps a) (Maybe a)
useSteppingAff = useAff' identity

useAff' ::
  forall deps a.
  Eq deps =>
  (Maybe (Either Error a) -> Maybe (Either Error a)) ->
  deps ->
  Aff a ->
  Hook (UseAff deps a) (Maybe a)
useAff' initUpdater deps aff =
  coerceHook React.do
    result /\ setResult <- useState Nothing
    useEffect deps do
      setResult initUpdater
      fiber <-
        launchAff do
          r <- try aff
          liftEffect do
            setResult \_ -> Just r
      pure do
        launchAff_ do
          killFiber (error "Stale request cancelled") fiber
    unsafeRenderEffect case result of
      Just (Left err) -> throwError err
      Just (Right a) -> pure (Just a)
      Nothing -> pure Nothing

newtype UseAff deps a hooks
  = UseAff
      ( hooks
      & UseState (Maybe (Either Error a))
      & UseEffect deps
      )

derive instance ntUseAff :: Newtype (UseAff deps a hooks) _

--| Provide an initial state and a reducer function. This is a more powerful
--| version of `useReducer`, where a state change can additionally queue
--| asynchronous operations. The results of those operations must be  mapped
--| into the reducer's `action` type. This is essentially the Elm architecture.
--|
--| Generally, I recommend `useAff` paired with tools like `useResetToken` over
--| `useAffReducer` as there are many ways `useAffReducer` can result in race
--| conditions. `useAff` with proper dependency management will handle previous
--| request cancellation and ensure your `Aff` result is always in sync with
--| the provided `deps`, for example. To accomplish the same thing with
--| `useAffReducer` would require tracking `Fiber`s manually in your state
--| somehow.. :c
--|
--| That said, `useAffReducer` can still be helpful when converting from the
--| current `React.Basic` (non-hooks) API or for those used to Elm.
--|
--| *Note: Aff failures are thrown. If you need to capture an error state, be
--|   sure to capture it in your action type!*
useAffReducer ::
  forall state action.
  state ->
  AffReducer state action ->
  Hook (UseAffReducer state action) (state /\ (action -> Effect Unit))
useAffReducer initialState affReducer =
  coerceHook React.do
    reducer' <-
      useMemo (UnsafeReference affReducer) \_ ->
        unsafePerformEffect do
          mkReducer (\{ state } -> runAffReducer affReducer state)
    { state, effects } /\ dispatch <-
      useReducer { state: initialState, effects: [] } reducer'
    useEffect (UnsafeReference effects) do
      for_ effects \aff ->
        launchAff_ do
          actions <- aff
          liftEffect do for_ actions dispatch
      mempty
    pure (state /\ dispatch)

newtype UseAffReducer state action hooks
  = UseAffReducer
      ( hooks
      & UseMemo (UnsafeReference (AffReducer state action))
          ( Reducer
              { effects :: Array (Aff (Array action))
              , state :: state
              }
              action
          )
      & UseReducer { state :: state, effects :: Array (Aff (Array action)) } action
      & UseEffect (UnsafeReference (Array (Aff (Array action))))
      )

derive instance ntUseAffReducer :: Newtype (UseAffReducer state action hooks) _

newtype AffReducer state action
  = AffReducer
  ( Fn2
      state
      action
      { state :: state, effects :: Array (Aff (Array action)) }
  )

mkAffReducer ::
  forall state action.
  (state -> action -> { state :: state, effects :: Array (Aff (Array action)) }) ->
  Effect (AffReducer state action)
mkAffReducer = pure <<< AffReducer <<< mkFn2

--| Run a wrapped `Reducer` function as a normal function (like `runFn2`).
--| Useful for testing, simulating actions, or building more complicated
--| hooks on top of `useReducer`
runAffReducer ::
  forall state action.
  AffReducer state action ->
  state ->
  action ->
  { state :: state, effects :: Array (Aff (Array action)) }
runAffReducer (AffReducer reducer) = runFn2 reducer

noEffects ::
  forall state action.
  state ->
  { state :: state
  , effects :: Array (Aff (Array action))
  }
noEffects state = { state, effects: [] }

--| Aff version of `useActionState` for managing async form actions.
--| The action function receives the previous state and form data, and returns
--| an `Aff` that resolves to the new state. React will automatically handle
--| the pending state whilst the Aff is running.
--|
--| *Note: Aff failures are thrown as React errors. If you need to capture an
--|   error state, incorporate it into your state type (e.g., `Either Error MyState`)!*
--|
--| ```purs
--| state /\ formAction /\ isPending <- useAffActionState initialState \prevState formData -> do
--|   result <- submitToServer formData
--|   pure (processResult prevState result)
--|
--| pure $ R.button
--|   { disabled: isPending
--|   , onClick: handler_ (formAction myFormData)
--|   }
--| ```
useAffActionState ::
  forall state formData.
  state ->
  (state -> formData -> Aff state) ->
  Hook (UseAffActionState state formData) (state /\ ((formData -> Effect Unit) /\ Boolean))
useAffActionState initialState affFn =
  coerceHook React.do
    unsafeHook do
      let affFnAsPromise prevState formData = fromAff (affFn prevState formData)
      runEffectFn3 useAffActionState_
        mkTuple3
        affFnAsPromise
        initialState
  where
  mkTuple3 :: forall a b c. Fn3 a b c (a /\ (b /\ c))
  mkTuple3 = mkFn3 \a b c -> Tuple a (Tuple b c)

--| Like `useAffActionState` but with a permalink for progressive enhancement.
--| The form will submit to this URL if JavaScript is disabled.
--|
--| ```purs
--| state /\ formAction /\ isPending <- useAffActionStateWithPermalink initialState affFn "/api/submit"
--|
--| pure $ R.form
--|   { action: formAction
--|   , children: [ ... ]
--|   }
--| ```
useAffActionStateWithPermalink ::
  forall state formData.
  state ->
  (state -> formData -> Aff state) ->
  String ->
  Hook (UseAffActionState state formData) (state /\ ((formData -> Effect Unit) /\ Boolean))
useAffActionStateWithPermalink initialState affFn permalink =
  coerceHook React.do
    unsafeHook do
      let affFnAsPromise prevState formData = fromAff (affFn prevState formData)
      runEffectFn4 useAffActionStateWithPermalink_
        mkTuple3
        affFnAsPromise
        initialState
        permalink
  where
  mkTuple3 :: forall a b c. Fn3 a b c (a /\ (b /\ c))
  mkTuple3 = mkFn3 \a b c -> Tuple a (Tuple b c)

foreign import useAffActionState_ ::
  forall state formData.
  EffectFn3
    (forall a b c. Fn3 a b c (a /\ (b /\ c)))
    (state -> formData -> Effect (Promise state))
    state
    (state /\ ((formData -> Effect Unit) /\ Boolean))

foreign import useAffActionStateWithPermalink_ ::
  forall state formData.
  EffectFn4
    (forall a b c. Fn3 a b c (a /\ (b /\ c)))
    (state -> formData -> Effect (Promise state))
    state
    String
    (state /\ ((formData -> Effect Unit) /\ Boolean))

newtype UseAffActionState state formData hooks
  = UseAffActionState (hooks & UseActionState state formData)

derive instance ntUseAffActionState :: Newtype (UseAffActionState state formData hooks) _
