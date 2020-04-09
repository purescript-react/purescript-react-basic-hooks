module Example where

import Prelude
import Effect.Class.Console (log)
import React.Basic.DOM as R
import React.Basic.Events (handler_)
import React.Basic.Hooks (Component, component, fragment, useEffect, useState, (/\))
import React.Basic.Hooks as React

mkExample :: Component Unit
mkExample = do
  component "Counter" \_ -> React.do
    counter /\ setCounter <- useState 0
    useEffect counter do
      log $ "Count: " <> show counter
      pure mempty
    pure
      $ fragment
          [ R.button
              { onClick: handler_ $ setCounter (_ + 1)
              , children: [ R.text $ "Increment: " <> show counter ]
              }
          ]
