module Example where

import Prelude
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import React.Basic.DOM as R
import React.Basic.DOM.Events (preventDefault, stopPropagation, targetValue, timeStamp)
import React.Basic.Events (EventHandler, handler, merge)
import React.Basic.Hooks (Component, Hook, UseState, component, fragment, useState, (/\))
import React.Basic.Hooks as React

mkExample :: Component Unit
mkExample = do
  component "ControlledInput" \_ -> React.do
    firstName <- useInput "hello"
    lastName <- useInput "world"
    pure
      $ R.form_
          [ renderInput firstName
          , renderInput lastName
          ]
  where
  renderInput input =
    fragment
      [ R.input { onChange: input.onChange, value: input.value }
      , R.p_ [ R.text ("Current value = " <> show input.value) ]
      , R.p_ [ R.text ("Changed at = " <> maybe "never" show input.lastChanged) ]
      ]

useInput ::
  String ->
  Hook
    (UseState { value :: String, lastChanged :: Maybe Number })
    { onChange :: EventHandler
    , value :: String
    , lastChanged :: Maybe Number
    }
useInput initialValue = React.do
  { value, lastChanged } /\ replaceState <- useState { value: initialValue, lastChanged: Nothing }
  pure
    { onChange:
        handler
          (preventDefault >>> stopPropagation >>> merge { targetValue, timeStamp }) \{ timeStamp, targetValue } -> do
          replaceState \_ ->
            { value: fromMaybe "" targetValue
            , lastChanged: Just timeStamp
            }
    , value
    , lastChanged
    }
