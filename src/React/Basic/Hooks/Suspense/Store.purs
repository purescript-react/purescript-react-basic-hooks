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
import Data.Int (ceil)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Aff, attempt, launchAff, throwError)
import Effect.Class (liftEffect)
import Effect.Console (warn)
import Effect.Exception (try)
import Effect.Now (now)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import React.Basic.Hooks (type (/\), (/\))
import React.Basic.Hooks.Suspense (Suspended(..), SuspenseResult(..))
import Web.HTML (window)
import Web.HTML.Window (requestIdleCallback)

--| Simple key-based cache.
mkSuspenseStore ::
  forall k v.
  Ord k =>
  Maybe Milliseconds ->
  (k -> Aff v) ->
  Effect (SuspenseStore k v)
mkSuspenseStore defaultMaxAge backend = do
  ref <- Ref.new Map.empty
  let
    isExpired maxAge now' (_ /\ savedTime) = unInstant savedTime <> maxAge < unInstant now'

    pruneCache = do
      case defaultMaxAge of
        Nothing -> pure unit
        Just maxAge -> do
          now' <- now
          void $ Ref.modify (Map.filter (not isExpired maxAge now')) ref
          void
            $ window
            >>= requestIdleCallback
                { timeout: ceil $ un Milliseconds maxAge
                }
                pruneCache

    tryFromCache itemMaxAge k = do
      rMaybe <- Map.lookup k <$> Ref.read ref
      case rMaybe of
        Nothing -> pure Nothing
        Just v@(r /\ _) -> do
          case itemMaxAge <|> defaultMaxAge of
            Nothing -> pure (Just r)
            Just maxAge -> do
              now' <- now
              if isExpired maxAge now' v then do
                _ <- Ref.modify (Map.delete k) ref
                pure Nothing
              else
                pure (Just r)

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
                                Just r'@(_ /\ d') ->
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
  do
    r <- try pruneCache
    case r of
      Left _ -> warn "Failed to initialize the suspense store cleanup task. Ensure you're using it in a browser with `requestIdleCallback` support."
      Right _ -> pure unit
  pure
    $ SuspenseStore
        { cache: ref
        , get: map Suspended <<< getCacheOrBackend
        }

newtype SuspenseStore k v
  = SuspenseStore
  { cache :: Ref (Map k (SuspenseResult v /\ Instant))
  , get :: Maybe Milliseconds -> k -> Suspended v
  }

get :: forall k v. SuspenseStore k v -> k -> Suspended v
get (SuspenseStore s) = s.get Nothing

get' :: forall k v. SuspenseStore k v -> Milliseconds -> k -> Suspended v
get' (SuspenseStore s) d = s.get (Just d)
