module Context where

import Prelude
import Effect (Effect)
import React.Basic.DOM as R
import React.Basic.Events (handler_)
import React.Basic.Hooks (type (/\), CreateComponent, JSX, ReactContext, component, createContext, element, provider, useContext, useState, (/\))
import React.Basic.Hooks as React

mkContext :: CreateComponent {}
mkContext = do
  counterContext <- createContext (0 /\ pure unit)
  store <- mkStore counterContext
  counter <- mkCounter counterContext
  component "Context" \props -> React.do
    pure
      $ element store
          { children:
            [ element counter {}
            , element counter {}
            , element counter {}
            ]
          }

mkStore ::
  ReactContext (Int /\ (Effect Unit)) ->
  CreateComponent { children :: Array JSX }
mkStore context = do
  component "Store" \{ children } -> React.do
    counter /\ setCounter <- useState 0
    let
      increment = setCounter (_ + 1)
    pure
      $ provider context
          (counter /\ increment)
          children

mkCounter ::
  ReactContext (Int /\ (Effect Unit)) ->
  CreateComponent {}
mkCounter counterContext = do
  component "Counter" \props -> React.do
    counter /\ increment <- useContext counterContext
    pure
      $ R.button
          { onClick: handler_ increment
          , children: [ R.text $ "Increment: " <> show counter ]
          }
