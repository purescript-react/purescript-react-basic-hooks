module Test.Spec.React19HooksSpec where

import Prelude

import Control.Monad (when, void)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Milliseconds(..), delay)
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
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Assertions.DOM (textContentShouldEqual)

spec :: Spec Unit
spec =
  after_ cleanup do
    before setup do
      describe "React 19 hooks" do
        it "useOptimistic works" \{ useOptimistic } -> do
          { findByTestId } <- renderComponent useOptimistic {}
          displayElem <- findByTestId "display"
          realElem <- findByTestId "real"
          buttonElem <- findByTestId "button"
          
          -- Initial state
          displayElem `textContentShouldEqual` "0"
          realElem `textContentShouldEqual` "0"
          
          -- Click to trigger optimistic update within transition
          fireEventClick buttonElem
          
          -- Both should be updated (since transition completes synchronously in test)
          displayElem `textContentShouldEqual` "1"
          realElem `textContentShouldEqual` "1"

        it "useActionState works" \{ useActionState } -> do
          { findByTestId } <- renderComponent useActionState {}
          stateElem <- findByTestId "state"
          buttonElem <- findByTestId "button"
          
          -- Initial state
          stateElem `textContentShouldEqual` "0"
          
          -- Click button to trigger action
          fireEventClick buttonElem
          
          -- Should show updated state
          stateElem `textContentShouldEqual` "1"

        it "useEffectEvent works" \{ useEffectEvent } -> do
          { findByTestId } <- renderComponent useEffectEvent {}
          countElem <- findByTestId "count"
          effectCountElem <- findByTestId "effect-count"
          incrementButton <- findByTestId "increment"
          
          -- Initial state
          countElem `textContentShouldEqual` "0"
          effectCountElem `textContentShouldEqual` "1"
          
          -- Click to increment count
          fireEventClick incrementButton
          fireEventClick incrementButton
          fireEventClick incrementButton
          
          -- Count should update
          countElem `textContentShouldEqual` "3"
          
          -- Effect should not have re-run (still 1)
          effectCountElem `textContentShouldEqual` "1"

  where
  setup = liftEffect ado

    useOptimistic <-
      reactComponent "UseOptimisticExample" \(_ :: {}) -> Hooks.do
        value /\ setValue <- Hooks.useState 0
        optimisticValue /\ addOptimistic <- Hooks.useOptimistic value \state increment ->
          state + increment
        _isPending /\ startTransition <- Hooks.useTransition
        
        let 
          handleClick = startTransition do
            addOptimistic 1
            setValue (_ + 1)
        
        pure $ R.div_
          [ R.div
              { _data: Object.singleton "testid" "display"
              , children: [ R.text (show optimisticValue) ]
              }
          , R.div
              { _data: Object.singleton "testid" "real"
              , children: [ R.text (show value) ]
              }
          , R.button
              { _data: Object.singleton "testid" "button"
              , onClick: handler_ handleClick
              , children: [ R.text "Add" ]
              }
          ]

    useActionState <-
      reactComponent "UseActionStateExample" \(_ :: {}) -> Hooks.do
        state /\ (formAction /\ _isPending) <- Hooks.useActionState 0 \prevState _ ->
          pure (prevState + 1)
        
        pure $ R.div_
          [ R.div
              { _data: Object.singleton "testid" "state"
              , children: [ R.text (show state) ]
              }
          , R.button
              { _data: Object.singleton "testid" "button"
              , onClick: handler_ (formAction unit)
              , children: [ R.text "Increment" ]
              }
          ]

    useEffectEvent <-
      reactComponent "UseEffectEventExample" \(_ :: {}) -> Hooks.do
        count /\ setCount <- Hooks.useState 0
        effectCount /\ setEffectCount <- Hooks.useState 0
        
        -- Effect only depends on unit, so it runs once
        Hooks.useEffect unit do
          setEffectCount (_ + 1)
          pure mempty
        
        pure $ R.div_
          [ R.div
              { _data: Object.singleton "testid" "count"
              , children: [ R.text (show count) ]
              }
          , R.div
              { _data: Object.singleton "testid" "effect-count"
              , children: [ R.text (show effectCount) ]
              }
          , R.button
              { _data: Object.singleton "testid" "increment"
              , onClick: handler_ (setCount (_ + 1))
              , children: [ R.text "Increment" ]
              }
          ]

    in { useOptimistic, useActionState, useEffectEvent }
