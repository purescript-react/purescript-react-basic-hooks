module React.Basic.Hooks.Suspense.Store
  ( mkSuspenseStore
  , SuspenseStore
  , get
  , get'
  ) where

import Prelude
import Control.Alt ((<|>))
import Data.DateTime.Instant (Instant, unInstant)
import Data.Either (Either(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Time.Duration (Milliseconds)
import Effect (Effect)
import Effect.Aff (Aff, attempt, launchAff, throwError)
import Effect.Class (liftEffect)
import Effect.Now (now)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import React.Basic.Hooks (type (/\), (/\))
import React.Basic.Hooks.Suspense (Suspended(..), SuspenseResult(..))

-- | Simple key-based cache.
mkSuspenseStore ::
  forall k v.
  Ord k =>
  Maybe Milliseconds ->
  (k -> Aff v) ->
  Effect (SuspenseStore k v)
mkSuspenseStore defaultMaxAge backend = do
  ref <- Ref.new mempty
  let
    tryFromCache itemMaxAge k = do
      rMaybe <- Map.lookup k <$> Ref.read ref
      case rMaybe of
        Nothing -> pure Nothing
        Just (r /\ d) -> do
          case itemMaxAge <|> defaultMaxAge of
            Nothing -> pure (Just r)
            Just maxAge -> do
              now' <- now
              if unInstant now' < unInstant d <> maxAge then
                pure (Just r)
              else
                pure Nothing

    getCacheOrBackend itemMaxAge k = do
      c <- tryFromCache itemMaxAge k
      case c of
        Just v -> pure v
        Nothing -> do
          fiber <-
            launchAff do
              r <- attempt do backend k
              liftEffect do
                let
                  v = case r of
                    Left e -> Failed e
                    Right v' -> Complete v'
                d <- now
                _ <-
                  ref
                    # Ref.modify
                        ( k
                            # Map.alter case _ of
                                Nothing -> Just (v /\ d)
                                Just r'@(v' /\ d') ->
                                  if d > d' then
                                    Just (v /\ d)
                                  else
                                    Just r'
                        )
                case r of
                  Left e -> throwError e
                  Right v' -> pure v'
          let
            v = InProgress fiber
          d <- now
          _ <- ref # Ref.modify (Map.insert k (v /\ d))
          pure v
  pure
    $ SuspenseStore
        { cache: ref
        , get: \k -> Suspended do getCacheOrBackend Nothing k
        , get': \d k -> Suspended do getCacheOrBackend (Just d) k
        }

newtype SuspenseStore k v
  = SuspenseStore
  { cache :: Ref (Map k (SuspenseResult v /\ Instant))
  , get :: k -> Suspended v
  , get' :: Milliseconds -> k -> Suspended v
  }

get :: forall k v. SuspenseStore k v -> k -> Suspended v
get (SuspenseStore s) = s.get

get' :: forall k v. SuspenseStore k v -> Milliseconds -> k -> Suspended v
get' (SuspenseStore s) = s.get'
