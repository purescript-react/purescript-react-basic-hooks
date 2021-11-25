module Test.Spec.UseStateSpec where

import Prelude

import Effect.Class (liftEffect)
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture_)
import React.Basic.Hooks (reactComponent, useState, useState', (/\))
import React.Basic.Hooks as Hooks
import React.TestingLibrary (cleanup, fireEventClick, renderComponent)
import Test.Spec (Spec, after_, before, describe, it)
import Test.Spec.Assertions.DOM (textContentShouldEqual)


spec ∷ Spec Unit
spec =
  after_ cleanup do
    before setup do
      describe "useState" do
        it "works with simple values" \{ useStateWithInt } -> do
          { findByText } <- renderComponent useStateWithInt {}
          elem <- findByText "0"
          fireEventClick elem
          elem `textContentShouldEqual` "1"

        it "works with function values" \{ useStateWithFn } -> do
          -- this is not a normal way to use useState, but it
          -- still shouldn't break at runtime
          { findByText } <- renderComponent useStateWithFn {}
          elem <- findByText "0"
          fireEventClick elem
          elem `textContentShouldEqual` "1"

      describe "useState'" do
        it "works with simple values" \{ useState'WithInt } -> do
          { findByText } <- renderComponent useState'WithInt {}
          elem <- findByText "0"
          fireEventClick elem
          elem `textContentShouldEqual` "1"

        it "works with function values" \{ useState'WithFn } -> do
          -- this is not a normal way to use useState', but it
          -- still shouldn't break at runtime
          { findByText } <- renderComponent useState'WithFn {}
          elem <- findByText "0"
          fireEventClick elem
          elem `textContentShouldEqual` "1"

  where
    setup = liftEffect do
      useStateWithInt <-
        reactComponent "Counter" \_ -> Hooks.do
          count /\ setCount <- useState 0
          pure $ R.button
            { onClick: capture_ do
                setCount (_ + 1)
            , children: [ R.text $ show count ]
            }

      useStateWithFn <-
        reactComponent "Counter" \_ -> Hooks.do
          count /\ setCount <- useState (\_ -> 0)
          pure $ R.button
            { onClick: capture_ do
                setCount (\fn -> \_ -> fn unit + 1)
            , children: [ R.text $ show $ count unit ]
            }

      useState'WithInt <-
        reactComponent "Counter" \_ -> Hooks.do
          count /\ setCount <- useState' 0
          pure $ R.button
            { onClick: capture_ do
                setCount $ count + 1
            , children: [ R.text $ show count ]
            }

      useState'WithFn <-
        reactComponent "Counter" \_ -> Hooks.do
          count /\ setCount <- useState' (\_ -> 0)
          pure $ R.button
            { onClick: capture_ do
                setCount \_ -> count unit + 1
            , children: [ R.text $ show $ count unit ]
            }

      pure
        { useStateWithInt
        , useStateWithFn
        , useState'WithInt
        , useState'WithFn
        }