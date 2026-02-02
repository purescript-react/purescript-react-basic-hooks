module Test.Spec.ViewTransitionSpec where

import Prelude

import Test.Spec (Spec, describe, pending)

spec :: Spec Unit
spec =
  describe "ViewTransition component (requires React 19.2+ experimental)" do
    pending "renders children with enter animation"
    pending "renders with exit animation when removed"
    pending "handles update animations"
    pending "handles shared element transitions with names"
    pending "accepts AnimationMap for different transition types"
    pending "calls onEnter callback when entering"
