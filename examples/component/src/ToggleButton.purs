module ToggleButton where

import Prelude
import Effect.Console (log)
import React.Basic.DOM as R
import React.Basic.Events (handler_)
import React.Basic.Hooks (Component, component, useEffect, useState, (/\))
import React.Basic.Hooks as React

mkToggleButton :: Component { label :: String }
mkToggleButton = do
  component "ToggleButton" \{ label } -> React.do
    on /\ setOn <- useState false
    useEffect on do
      log $ "State: " <> if on then "On" else "Off"
      pure (pure unit)
    pure
      $ R.button
          { onClick: handler_ $ setOn not
          , children:
              [ R.text label
              , R.text if on then " On" else " Off"
              ]
          }
