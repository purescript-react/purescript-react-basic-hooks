module Example where

import Prelude
import Effect (Effect)
import React.Basic.DOM as R
import React.Basic.Events (handler_)
import React.Basic.Hooks (type (/\), ReactComponent, JSX, ReactContext, component, createContext, element, provider, useContext, useState, (/\))
import React.Basic.Hooks as React

mkExample :: Effect (ReactComponent {})
mkExample = do
  counterContext <- createContext (0 /\ pure unit)
  store <- mkStore counterContext
  counter <- mkCounter counterContext
  component "Context" \props -> React.do
    pure
      $ element store
          { content:
              [ element counter {}
              , element counter {}
              , element counter {}
              ]
          }

mkStore ::
  ReactContext (Int /\ (Effect Unit)) ->
  Effect (ReactComponent { content :: Array JSX })
mkStore context = do
  component "Store" \{ content } -> React.do
    counter /\ setCounter <- useState 0
    let
      increment = setCounter (_ + 1)
    pure
      $ provider context
          (counter /\ increment)
          content

mkCounter ::
  ReactContext (Int /\ (Effect Unit)) ->
  Effect (ReactComponent {})
mkCounter counterContext = do
  component "Counter" \props -> React.do
    counter /\ increment <- useContext counterContext
    pure
      $ R.button
          { onClick: handler_ increment
          , children: [ R.text $ "Increment: " <> show counter ]
          }
