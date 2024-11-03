module Test.Spec.React18HooksSpec where

import Prelude

import Control.Monad.Rec.Class (forever)
import Data.Array as Array
import Data.Foldable (sequence_, traverse_)
import Data.Maybe (fromMaybe)
import Data.Monoid (guard, power)
import Data.String as String
import Data.Tuple.Nested ((/\))
import Effect.Aff (Milliseconds(..), apathize, delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Foreign.Object as Object
import React.Basic (fragment)
import React.Basic.DOM as R
import React.Basic.DOM.Events (targetValue)
import React.Basic.Events (handler, handler_)
import React.Basic.Hooks (reactComponent)
import React.Basic.Hooks as Hooks
import React.TestingLibrary (cleanup, fireEventClick, renderComponent, typeText)
import Test.Spec (Spec, after_, before, describe, it)
import Test.Spec.Assertions (shouldNotEqual)
import Test.Spec.Assertions.DOM (textContentShouldEqual)
import Web.DOM.Element (getAttribute)
import Web.HTML.HTMLElement as HTMLElement

spec ∷ Spec Unit
spec =
  after_ cleanup do
    before setup do
      describe "React 18 hooks" do
        it "useId works" \{ useId } -> do
          { findByTestId } <- renderComponent useId {}
          elem <- findByTestId "use-id"
          idʔ <- getAttribute "id" (HTMLElement.toElement elem) # liftEffect
          let id = idʔ # fromMaybe ""
          id `shouldNotEqual` ""
          elem `textContentShouldEqual` id

        it "useTransition works" \{ useTransition } -> do
          { findByText } <- renderComponent useTransition {}
          elem <- findByText "0"
          fireEventClick elem
          elem `textContentShouldEqual` "1"

        it "useDeferredValue hopefully works" \{ useDeferredValue } -> do
          { findByTestId } <- renderComponent useDeferredValue {}
          spanElem <- findByTestId "span"
          spanElem `textContentShouldEqual` "0"
          findByTestId "input" >>= typeText (power "text" 100)
          spanElem `textContentShouldEqual` "400"

        it "useSyncExternalStore" \{ useSyncExternalStore } -> do
          { findByTestId } <- renderComponent useSyncExternalStore {}
          spanElem <- findByTestId "span"
          spanElem `textContentShouldEqual` "0"
          delay (350.0 # Milliseconds)
          spanElem `textContentShouldEqual` "3"

        it "useInsertionEffect works" \{ useInsertionEffect } -> do
          { findByText } <- renderComponent useInsertionEffect {}
          void $ findByText "insertion-done"

  where
  setup = liftEffect ado

    useId <-
      reactComponent "UseIDExample" \(_ :: {}) -> Hooks.do
        id <- Hooks.useId
        pure $ R.div
          { id
          , _data: Object.singleton "testid" "use-id"
          , children: [ R.text id ]
          }

    useTransition <-
      reactComponent "UseTransitionExample" \(_ :: {}) -> Hooks.do
        isPending /\ startTransition <- Hooks.useTransition
        count /\ setCount <- Hooks.useState 0
        let handleClick = startTransition do setCount (_ + 1)
        pure $ R.div
          { children:
              [ guard isPending (R.text "Pending")
              , R.button
                  { onClick: handler_ handleClick
                  , children: [ R.text (show count) ]
                  }
              ]
          }

    useDeferredValue <-
      reactComponent "UseDeferredValueExample" \(_ :: {}) -> Hooks.do
        text /\ setText <- Hooks.useState' ""
        textLength <- Hooks.useDeferredValue (String.length text)
        pure $ fragment
          [ R.input
              { onChange: handler targetValue (traverse_ setText)
              , _data: Object.singleton "testid" "input"
              }
          , R.span
              { _data: Object.singleton "testid" "span"
              , children: [ R.text (show textLength) ]
              }
          ]

    useInsertionEffect <-
      reactComponent "UseInsertionEffectExample" \(_ :: {}) -> Hooks.do
        text /\ setText <- Hooks.useState' "waiting"
        Hooks.useInsertionEffect unit do
          setText "insertion-done"
          mempty
        pure $ R.span_ [ R.text text ]

    useSyncExternalStore <- do
      { subscribe, getSnapshot, getServerSnapshot } <- do
        subscribersRef <- Ref.new []
        intRef <- Ref.new 0
        -- Update the intRef every 100ms.
        launchAff_ $ apathize $ forever do
          delay (100.0 # Milliseconds)
          intRef # Ref.modify_ (_ + 1) # liftEffect
          subscribers <- subscribersRef # Ref.read # liftEffect
          liftEffect $ sequence_ subscribers

        pure
          { subscribe: \callback -> do
              subscribersRef # Ref.modify_ (Array.cons callback)
              pure $
                subscribersRef # Ref.modify_ (Array.drop 1)
          , getSnapshot: Ref.read intRef
          , getServerSnapshot: Ref.read intRef
          }

      reactComponent "UseSyncExternalStoreExample" \(_ :: {}) -> Hooks.do
        number <- Hooks.useSyncExternalStore
          subscribe
          getSnapshot
          getServerSnapshot
        pure $ R.span { _data: Object.singleton "testid" "span", children: [ R.text (show number) ] }

    in { useId, useTransition, useDeferredValue, useInsertionEffect, useSyncExternalStore }