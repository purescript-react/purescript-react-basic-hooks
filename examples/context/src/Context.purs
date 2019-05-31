module Context where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import React.Basic.DOM as R
import React.Basic.Events (handler_)
import React.Basic.Hooks (Context, CreateComponent, JSX, Tuple, component, contextProvider, createContext, element, fragment, useContext, useState, (/\))
import React.Basic.Hooks as React

mkContext :: CreateComponent {}
mkContext = do

  counterContext <- createContext
  store <- mkStore counterContext
  counter <- mkCounter counterContext

  component "Context" \props -> React.do

    pure $ element store
      { children:
          [ element counter {}
          , element counter {}
          , element counter {}
          ]
      }

mkStore
  :: Context (Tuple Int (Effect Unit))
  -> CreateComponent { children :: Array JSX }
mkStore context = do
  component "Store" \{ children } -> React.do
    counter /\ setCounter <- useState 0
    let increment = setCounter (_ + 1)
    pure $
      contextProvider context
        (counter /\ increment)
        (fragment children)

mkCounter
  :: Context (Tuple Int (Effect Unit))
  -> CreateComponent {}
mkCounter counterContext = do
  component "Counter" \props -> React.do
    mCounter <- useContext counterContext

    case mCounter of
      Nothing -> do
        pure $ R.text "no counter value found"
      Just (counter /\ increment) -> do
        pure $
          R.button
            { onClick: handler_ increment
            , children: [ R.text $ "Increment: " <> show counter ]
            }
