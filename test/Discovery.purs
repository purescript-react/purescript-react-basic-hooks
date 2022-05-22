-- Vendored in because of
-- https://github.com/purescript-spec/purescript-spec-discovery/issues/18
module Test.Discovery (discover) where

import Prelude

import Control.Promise (Promise, toAffE)
import Data.Traversable (sequence_)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Test.Spec (Spec)

foreign import getSpecs ∷
  String ->
  Effect (Promise (Array (Spec Unit)))

discover ∷ String -> Aff (Spec Unit)
discover pattern = liftAff do
  specsPromise <- toAffE $ getSpecs pattern
  pure $ sequence_ specsPromise