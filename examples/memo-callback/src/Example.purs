module Example where

import Prelude
import Data.Foldable (traverse_)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import React.Basic.DOM as R
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler, handler_)
import React.Basic.Hooks (ReactComponent, UnsafeReference(..), component, fragment, useEffect, useLazy, useMemo, useState, (/\))
import React.Basic.Hooks as React

mkExample :: Effect (ReactComponent {})
mkExample = do
  component "MemoCallback" \props -> React.do
    initialValue /\ setInitialValue <- useState 0
    counter /\ setCounter <- useState initialValue
    increment <-
      useLazy
        (UnsafeReference setCounter) \_ -> setCounter (_ + 1)
    reset <-
      useLazy
        (initialValue /\ UnsafeReference setCounter) \_ -> setCounter \_ -> initialValue
    useEffect (UnsafeReference increment) do
      log "increment updated -- this should only get logged once!"
      pure mempty
    useEffect (UnsafeReference reset) do
      log "reset updated -- this should only get logged when the 'initialValue' is changed!"
      pure mempty
    let
      memoTestVal = Just 1
    memoTest <- useMemo memoTestVal
    useEffect (UnsafeReference memoTest) do
      log "memoTest updated -- this should only get logged once!"
      pure mempty
    pure
      $ fragment
          [ R.text "Change initial value:"
          , R.div_
              [ R.input
                  { value: show initialValue
                  , onChange:
                      handler targetValue
                        $ (_ >>= Int.fromString)
                        >>> traverse_ (const >>> setInitialValue)
                  }
              , R.button
                  { onClick: handler_ reset
                  , children: [ R.text "Reset" ]
                  }
              ]
          , R.button
              { onClick: handler_ increment
              , children: [ R.text $ "Increment: " <> show counter ]
              }
          ]
