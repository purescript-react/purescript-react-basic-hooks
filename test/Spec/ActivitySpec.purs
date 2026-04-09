module Test.Spec.ActivitySpec where

import Prelude

import Data.Tuple.Nested ((/\))
import Effect.Class (liftEffect)
import Foreign.Object as Object
import React.Basic.DOM as R
import React.Basic.Events (handler_)
import React.Basic.Hooks (reactComponent)
import React.Basic.Hooks as Hooks
import React.Basic.Hooks.Activity (ActivityMode(..), activity)
import React.TestingLibrary (cleanup, fireEventClick, renderComponent)
import Test.Spec (Spec, after_, before, describe, it)
import Test.Spec.Assertions.DOM (textContentShouldEqual)

spec :: Spec Unit
spec =
  after_ cleanup do
    before setup do
      describe "Activity" do
        it "shows children when mode is Visible" \{ visibleActivity } -> do
          { findByTestId } <- renderComponent visibleActivity {}
          contentElem <- findByTestId "content"
          contentElem `textContentShouldEqual` "Visible Content"

        it "toggles visibility and preserves state" \{ toggleActivity } -> do
          { findByTestId } <- renderComponent toggleActivity {}
          counterElem <- findByTestId "counter"
          toggleButton <- findByTestId "toggle"
          incrementButton <- findByTestId "increment"

          counterElem `textContentShouldEqual` "Count: 0"

          fireEventClick incrementButton
          counterElem `textContentShouldEqual` "Count: 1"

          fireEventClick incrementButton
          counterElem `textContentShouldEqual` "Count: 2"

          fireEventClick toggleButton
          fireEventClick toggleButton

          counterElem' <- findByTestId "counter"
          counterElem' `textContentShouldEqual` "Count: 2"

  where
  setup = liftEffect do
    visibleActivity <-
      reactComponent "VisibleActivityExample" \(_ :: {}) -> Hooks.do
        pure $ activity
          { mode: Visible
          , children:
              [ R.div
                  { _data: Object.singleton "testid" "content"
                  , children: [ R.text "Visible Content" ]
                  }
              ]
          }

    counterComponent <-
      reactComponent "Counter" \(_ :: {}) -> Hooks.do
        count /\ setCount <- Hooks.useState 0
        pure $ R.div_
          [ R.div
              { _data: Object.singleton "testid" "counter"
              , children: [ R.text $ "Count: " <> show count ]
              }
          , R.button
              { _data: Object.singleton "testid" "increment"
              , onClick: handler_ (setCount (_ + 1))
              , children: [ R.text "Increment" ]
              }
          ]

    toggleActivity <-
      reactComponent "ToggleActivityExample" \(_ :: {}) -> Hooks.do
        isVisible /\ setIsVisible <- Hooks.useState true
        pure $ R.div_
          [ R.button
              { _data: Object.singleton "testid" "toggle"
              , onClick: handler_ (setIsVisible not)
              , children: [ R.text if isVisible then "Hide" else "Show" ]
              }
          , activity
              { mode: if isVisible then Visible else Hidden
              , children: [ Hooks.element counterComponent {} ]
              }
          ]

    pure { visibleActivity, toggleActivity }
