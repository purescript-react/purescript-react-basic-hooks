module React.Basic.Hooks.Aff where

import Prelude
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Effect.Aff (Aff, error, killFiber, launchAff_, runAff)
import Effect.Exception (Error)
import React.Basic.Hooks (Hook, UseEffect, UseState, newtypeHook, useEffect, useState, (/\))
import React.Basic.Hooks as React

newtype UseAff key a hooks
  = UseAff (UseEffect key (UseState (Result a) hooks))

derive instance ntUseAff :: Newtype (UseAff key a hooks) _

type Result a
  = Maybe (Either Error a)

useAff ::
  forall key a.
  Eq key =>
  key ->
  Aff a ->
  Hook (UseAff key a) (Result a)
useAff key aff =
  newtypeHook React.do
    result /\ setResult <- useState Nothing
    useEffect key do
      setResult (const Nothing)
      fiber <- runAff (setResult <<< const <<< Just) aff
      pure do
        launchAff_ do
          killFiber (error "Effect hook discarded.") fiber
    pure result
