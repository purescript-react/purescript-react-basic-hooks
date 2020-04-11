module React.Basic.Hooks.Aff
  ( useAff
  , UseAff
  ) where

import Prelude
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect.Aff (Aff, Error, error, killFiber, launchAff, launchAff_, throwError, try)
import Effect.Class (liftEffect)
import React.Basic.Hooks (Hook, UseEffect, UseState, coerceHook, unsafeRenderEffect, useEffect, useState, (/\))
import React.Basic.Hooks as React

-- | `useAff` is used for asynchronous effects or `Aff`. The asynchronous effect
-- | is re-run whenever the deps change. If another `Aff` runs when the deps
-- | change before the previous async resolves, it will cancel the previous
-- | in-flight effect.
-- |
-- | *Note: This hook requires parent components to handle error states! Don't
-- |   forget to implement a React error boundary or avoid `Aff` errors entirely
-- |   by incorporating them into your result type!*
useAff ::
  forall deps a.
  Eq deps =>
  deps ->
  Aff a ->
  Hook (UseAff deps a) (Maybe a)
useAff deps aff =
  coerceHook React.do
    result /\ setResult <- useState Nothing
    useEffect deps do
      setResult (const Nothing)
      fiber <-
        launchAff do
          r <- try aff
          liftEffect do
            setResult \_ -> Just r
      pure do
        launchAff_ do
          killFiber (error "Fiber cancelled for newer request") fiber
    unsafeRenderEffect case result of
      Just (Left err) -> throwError err
      Just (Right a) -> pure (Just a)
      Nothing -> pure Nothing

newtype UseAff deps a hooks
  = UseAff (UseEffect deps (UseState (Maybe (Either Error a)) hooks))

derive instance ntUseAff :: Newtype (UseAff deps a hooks) _
