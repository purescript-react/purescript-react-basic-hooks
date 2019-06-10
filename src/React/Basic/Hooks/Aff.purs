module React.Basic.Hooks.Aff where

import Prelude

import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, error, killFiber, launchAff_, runAff)
import Effect.Exception (Error)
import React.Basic.Hooks (Hook, UseEffect, UseState, useEffect, useState, (/\))
import React.Basic.Hooks as React

type UseAff key a hooks =
  UseEffect key (UseState (Maybe (Either Error a)) hooks)

useAff
  :: forall key a
   . Eq key
  => key
  -> Aff a
  -> Hook (UseAff key a) (Maybe (Either Error a))
useAff key aff = React.do
  result /\ setResult <- useState Nothing
  useEffect key do
    setResult (const Nothing)
    fiber <- runAff (setResult <<< const <<< Just) aff
    pure do
      launchAff_ do
        killFiber (error "Effect hook discarded.") fiber
  pure result