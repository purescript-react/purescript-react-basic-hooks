module ControlledInput where

import Prelude

import Control.Applicative.Indexed (ipure)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import React.Basic.DOM as R
import React.Basic.DOM.Events (preventDefault, stopPropagation, targetValue, timeStamp)
import React.Basic.Events (EventHandler, handler, merge)
import React.Basic.Hooks (CreateComponent, Render, UseState, component, fragment, useState, (/\))
import React.Basic.Hooks as React

mkControlledInput :: CreateComponent {}
mkControlledInput = do
  component "ControlledInput" \props -> React.do
    firstName <- useInput "hello"
    lastName <- useInput "world"

    React.pure $ R.form_
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

useInput
  :: forall hooks
   . String
  -> Render
       hooks
       (UseState { value :: String, lastChanged :: Maybe Number } hooks)
       { onChange :: EventHandler
       , value :: String
       , lastChanged :: Maybe Number
       }
useInput initialValue = React.do
  { value, lastChanged } /\ replaceState <- useState { value: initialValue, lastChanged: Nothing }
  ipure
    { onChange: handler
        (preventDefault >>> stopPropagation >>> merge { targetValue, timeStamp })
        \{ timeStamp, targetValue } -> do
          replaceState \_ ->
            { value: fromMaybe "" targetValue
            , lastChanged: Just timeStamp
            }
    , value
    , lastChanged
    }
