-- Vendored in because of
-- https://github.com/purescript-spec/purescript-spec-discovery/issues/18
module Test.Discovery (discover) where

import Prelude
import Data.Traversable (sequence_)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Test.Spec (Spec)

foreign import getSpecs ∷
  String ->
  Effect (Array (Spec Unit))

discover ∷
  ∀ m.
  MonadEffect m =>
  String ->
  m (Spec Unit)
discover pattern = getSpecs pattern >>= (pure <<< sequence_) # liftEffect