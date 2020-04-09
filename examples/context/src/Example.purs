module Example where

import Prelude
import Effect (Effect)
import React.Basic.DOM as R
import React.Basic.Events (handler_)
import React.Basic.Hooks (type (/\), JSX, ReactContext, Component, component, createContext, provider, useContext, useState, (/\))
import React.Basic.Hooks as React

mkExample :: Component Unit
mkExample = do
  counterContext <- createContext (0 /\ pure unit)
  store <- mkStore counterContext
  counter <- mkCounter counterContext
  component "Context" \_ -> React.do
    pure
      $ store
          [ counter unit
          , counter unit
          , counter unit
          ]

mkStore ::
  ReactContext (Int /\ (Effect Unit)) ->
  Component (Array JSX)
mkStore context = do
  component "Store" \content -> React.do
    counter /\ setCounter <- useState 0
    let
      increment = setCounter (_ + 1)
    pure
      $ provider context
          (counter /\ increment)
          content

mkCounter ::
  ReactContext (Int /\ (Effect Unit)) ->
  Component Unit
mkCounter counterContext = do
  component "Counter" \_ -> React.do
    counter /\ increment <- useContext counterContext
    pure
      $ R.button
          { onClick: handler_ increment
          , children: [ R.text $ "Increment: " <> show counter ]
          }
