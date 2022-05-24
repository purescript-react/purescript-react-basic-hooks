module Test.Spec.MemoSpec where

import Prelude

import Data.Function (on)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (modify, new, read)
import React.Basic.DOM as R
import React.Basic.Hooks (ReactComponent, element, memo, memo', reactComponent, useEffectAlways)
import React.Basic.Hooks as Hooks
import React.TestingLibrary (cleanup, render)
import Test.Spec (Spec, after_, before, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec ∷ Spec Unit
spec =
  after_ cleanup do
    before setup do
      describe "memo" do
        it "works with simple values" \{ memoTest } -> do
          rendersRef <- liftEffect do new 0
          let onRender = void $ modify (1 + _) rendersRef
          let renders = liftEffect do read rendersRef
          { rerender } <- render do element memoTest { onRender, arg: 0 }
          rerender do element memoTest { onRender, arg: 0 }
          renders >>= (_ `shouldEqual` 1)
          rerender do element memoTest { onRender, arg: 1 }
          renders >>= (_ `shouldEqual` 2)

      describe "memo'" do
        it "never renders if the eq fn returns true" \{ memo'TestAlwaysEq } -> do
          rendersRef <- liftEffect do new 0
          let onRender = void $ modify (1 + _) rendersRef
          let renders = liftEffect do read rendersRef
          { rerender } <- render do element memo'TestAlwaysEq { onRender, arg: 0 }
          rerender do element memo'TestAlwaysEq { onRender, arg: 0 }
          renders >>= (_ `shouldEqual` 1)
          rerender do element memo'TestAlwaysEq { onRender, arg: 1 }
          renders >>= (_ `shouldEqual` 1)

        it "always renders if the eq fn returns false" \{ memo'TestNeverEq } -> do
          rendersRef <- liftEffect do new 0
          let onRender = void $ modify (1 + _) rendersRef
          let renders = liftEffect do read rendersRef
          { rerender } <- render do element memo'TestNeverEq { onRender, arg: 0 }
          rerender do element memo'TestNeverEq { onRender, arg: 0 }
          renders >>= (_ `shouldEqual` 2)
          rerender do element memo'TestNeverEq { onRender, arg: 1 }
          renders >>= (_ `shouldEqual` 3)

        it "renders correctly over eq on props.arg" \{ memo'TestArgEq } -> do
          rendersRef <- liftEffect do new 0
          let onRender = void $ modify (1 + _) rendersRef
          let renders = liftEffect do read rendersRef
          { rerender } <- render do element memo'TestArgEq { onRender, arg: 0 }
          rerender do element memo'TestArgEq { onRender, arg: 0 }
          renders >>= (_ `shouldEqual` 1)
          rerender do element memo'TestArgEq { onRender, arg: 1 }
          renders >>= (_ `shouldEqual` 2)
          rerender do element memo'TestArgEq { onRender, arg: 1 }
          renders >>= (_ `shouldEqual` 2)
  where
    setup ::
      forall m. MonadEffect m =>
      m
        { memoTest :: ReactComponent { onRender :: Effect Unit, arg :: Int }
        , memo'TestAlwaysEq :: ReactComponent { onRender :: Effect Unit, arg :: Int }
        , memo'TestNeverEq :: ReactComponent { onRender :: Effect Unit, arg :: Int }
        , memo'TestArgEq :: ReactComponent { onRender :: Effect Unit, arg :: Int }
        }
    setup = liftEffect do
      memoTest <- memo do
        reactComponent "MemoTest" \{ onRender } -> Hooks.do
          useEffectAlways do
            onRender
            pure (pure unit)
          pure $ R.div_ []

      memo'TestAlwaysEq <- memo' (\_ _ -> true) do
        reactComponent "MemoTest" \{ onRender } -> Hooks.do
          useEffectAlways do
            onRender
            pure (pure unit)
          pure $ R.div_ []

      memo'TestNeverEq <- memo' (\_ _ -> false) do
        reactComponent "MemoTest" \{ onRender } -> Hooks.do
          useEffectAlways do
            onRender
            pure (pure unit)
          pure $ R.div_ []

      memo'TestArgEq <- memo' (eq `on` _.arg) do
        reactComponent "MemoTest" \{ onRender } -> Hooks.do
          useEffectAlways do
            onRender
            pure (pure unit)
          pure $ R.div_ []

      pure
        { memoTest
        , memo'TestAlwaysEq
        , memo'TestNeverEq
        , memo'TestArgEq
        }