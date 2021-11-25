module Test.Spec.UseReducerSpec where

import Prelude

import Effect.Class (liftEffect)
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture_)
import React.Basic.Hooks (mkReducer, reactComponent, useReducer, (/\))
import React.Basic.Hooks as Hooks
import React.TestingLibrary (cleanup, fireEventClick, renderComponent)
import Test.Spec (Spec, after_, before, describe, it)
import Test.Spec.Assertions.DOM (textContentShouldEqual)

spec ∷ Spec Unit
spec =
  after_ cleanup do
    before setup do
      describe "useReducer" do
        it "works with simple values" \{ useReducerWithInt } -> do
          { findByText } <- renderComponent useReducerWithInt {}
          elem <- findByText "0"
          fireEventClick elem
          elem `textContentShouldEqual` "1"

        it "works with function values" \{ useReducerWithFn } -> do
          -- this is not a normal way to use useReducer, but it
          -- still shouldn't break at runtime
          { findByText } <- renderComponent useReducerWithFn {}
          elem <- findByText "0"
          fireEventClick elem
          elem `textContentShouldEqual` "1"

  where
    setup = liftEffect do

      useReducerWithInt <- do
        reducer <- mkReducer \count -> case _ of
          Add -> count + 1
          Subtract -> count - 1

        reactComponent "Counter" \_ -> Hooks.do
          count /\ dispatch <- useReducer 0 reducer
          pure $ R.button
            { onClick: capture_ do
                dispatch Add
            , children: [ R.text $ show count ]
            }

      useReducerWithFn <- do
        reducer <- mkReducer \countFn -> case _ of
          Add -> \_ -> countFn unit + 1
          Subtract -> \_ -> countFn unit - 1

        reactComponent "Counter" \_ -> Hooks.do
          count /\ dispatch <- useReducer (\_ -> 0) reducer
          pure $ R.button
            { onClick: capture_ do
                dispatch Add
            , children: [ R.text $ show $ count unit ]
            }

      pure
        { useReducerWithInt
        , useReducerWithFn
        }

data Action = Add | Subtract