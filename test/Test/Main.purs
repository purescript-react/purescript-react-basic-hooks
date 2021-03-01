module Test.Main where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import React.Basic.Hooks.Suspense (Suspended(..), SuspenseResult)
import React.Basic.Hooks.Suspense.Store (class HasBackend, fromKey, get, mkSuspenseStore)
import Unsafe.Coerce (unsafeCoerce)

main :: Effect Unit
main = do
  store <- mkSuspenseStore (Just $ Milliseconds 200.0)
  let
    runGet :: forall k v s. HasBackend k v s => k -> Effect (SuspenseResult v)
    runGet k = case get store k of Suspended r' -> r'

    c1 = Key "1" :: Key Cat

    c2 = Key "2" :: Key Cat

    d1 = Key "1" :: Key Dog

    d2 = Key "2" :: Key Dog

    go = do
      c1' <- runGet c1
      d1' <- runGet d1
      c2' <- runGet c2
      d2' <- runGet d2
      c1'' <- runGet c1
      d1'' <- runGet d1
      c2'' <- runGet c2
      d2'' <- runGet d2
      l c1'
      l c1''
      l c2'
      l c2''
      l d1'
      l d1''
      l d2'
      l d2''
  go
  launchAff_ do
    delay (Milliseconds 100.0)
    liftEffect go
    delay (Milliseconds 200.0)
    liftEffect go
    liftEffect go
  where
  l :: forall v. v -> Effect Unit
  l v = do
    log (unsafeCoerce v)

newtype Key v
  = Key String

derive instance eqKey :: Eq (Key v)

data Cat
  = Cat { name :: String }

derive instance eqCat :: Eq Cat

data Dog
  = Dog { name :: String }

derive instance eqDog :: Eq Dog

instance backendCat :: HasBackend (Key Cat) Cat "Cat" where
  fromKey (Key key) = key
  backend key = do
    delay $ Milliseconds 0.0
    pure $ Cat { name: fromKey key }

instance backendDog :: HasBackend (Key Dog) Dog "Dog" where
  fromKey (Key key) = key
  backend key = do
    delay $ Milliseconds 0.0
    pure $ Dog { name: fromKey key }
