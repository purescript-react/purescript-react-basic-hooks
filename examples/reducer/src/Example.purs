module Example where

import Prelude
import React.Basic.DOM as R
import React.Basic.Events (handler_)
import React.Basic.Hooks (Component, component, fragment, useReducer, (/\))
import React.Basic.Hooks as React

data Action
  = Increment
  | Decrement

mkExample :: Component Unit
mkExample = do
  component "Reducer" \_ -> React.do
    state /\ dispatch <-
      useReducer { counter: 0 } \state -> case _ of
        Increment -> state { counter = state.counter + 1 }
        Decrement -> state { counter = state.counter - 1 }
    pure
      $ fragment
          [ R.button
              { onClick: handler_ $ dispatch Decrement
              , children: [ R.text $ "Decrement" ]
              }
          , R.button
              { onClick: handler_ $ dispatch Increment
              , children: [ R.text $ "Increment" ]
              }
          , R.div_
              [ R.text $ show state.counter
              ]
          ]
