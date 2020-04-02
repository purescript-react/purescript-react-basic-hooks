module Example where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import React.Basic.DOM as R
import React.Basic.Events (handler_)
import React.Basic.Hooks (ReactComponent, UnsafeReference(..), component, fragment, useCallback, useEffect, useState, (/\))
import React.Basic.Hooks as React

mkExample :: Effect (ReactComponent {})
mkExample = do
  component "MemoCallback" \props -> React.do
    counter /\ setCounter <- useState 0
    increment <-
      useCallback (UnsafeReference setCounter)
        $ setCounter (_ + 1)
    useEffect (UnsafeReference increment) do
      log "increment updated"
      pure mempty
    pure
      $ fragment
          [ R.button
              { onClick: handler_ increment
              , children: [ R.text $ "Increment: " <> show counter ]
              }
          ]
