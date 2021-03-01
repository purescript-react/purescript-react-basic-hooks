module React.Basic.Hooks.Suspense.Store
  ( SuspenseStore
  , mkSuspenseStore
  , get
  , get'
  , class HasBackend
  , fromKey
  , backend
  ) where

import Prelude
import Control.Alt ((<|>))
import Data.DateTime.Instant (unInstant)
import Data.Either (Either(..))
import Data.Function (on)
import Data.Int (ceil)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Ord (greaterThan)
import Data.String (joinWith)
import Data.Symbol (class IsSymbol, SProxy(..), reflectSymbol)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (fst, snd)
import Effect (Effect)
import Effect.Aff (Aff, attempt, launchAff, throwError)
import Effect.Class (liftEffect)
import Effect.Console (warn)
import Effect.Exception (try)
import Effect.Now (now)
import Effect.Ref as Ref
import React.Basic.Hooks ((/\))
import React.Basic.Hooks.Suspense (Suspended(..), SuspenseResult(..))
import Unsafe.Coerce (unsafeCoerce)
import Web.HTML (window)
import Web.HTML.Window (requestIdleCallback)

get :: forall k v s. HasBackend k v s => SuspenseStore -> k -> Suspended v
get s k = _get s Nothing k

get' :: forall k v s. HasBackend k v s => SuspenseStore -> Milliseconds -> k -> Suspended v
get' s d k = _get s (Just d) k

class
  IsSymbol s <= HasBackend k v (s :: Symbol) | k -> v s where
  fromKey :: k -> String
  backend :: k -> Aff v

mkSuspenseStore ::
  Maybe Milliseconds ->
  Effect SuspenseStore
mkSuspenseStore defaultMaxAge = do
  ref <- Ref.new mempty
  let
    isExpired maxAge now' (_ /\ d) = unInstant now' < unInstant d <> maxAge

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
        Just v@(r /\ d) -> do
          case itemMaxAge <|> defaultMaxAge of
            Nothing -> pure (Just r)
            Just maxAge -> do
              now' <- now
              if isExpired maxAge now' v then do
                _ <- Ref.modify (Map.delete k) ref
                pure Nothing
              else
                pure (Just r)

    insertIfNewer =
      Map.insertWith \r' r ->
        let
          gt = greaterThan `on` snd
        in
          if r `gt` r' then r else r'

    getCacheOrBackend :: Maybe Milliseconds -> StoreKey -> Effect (SuspenseResult Opaque)
    getCacheOrBackend itemMaxAge storable = do
      let
        k = toKey storable
      c <- tryFromCache itemMaxAge k
      case c of
        Just v -> pure v
        Nothing -> do
          fiber <-
            launchAff do
              r <- attempt do toAff storable
              liftEffect do
                let
                  v = case r of
                    Left e -> Failed e
                    Right v' -> Complete v'
                d <- now
                _ <- ref # Ref.modify (insertIfNewer k (v /\ d))
                case r of
                  Left e -> throwError e
                  Right v' -> pure v'
          syncV <- map fst <$> Map.lookup k <$> Ref.read ref
          case syncV of
            -- `Just v` means the backend `Aff` ran synchronously so
            -- we just return that result
            Just v -> pure v
            Nothing -> do
              let
                v = InProgress fiber
              d <- now
              _ <- ref # Ref.modify (insertIfNewer k (v /\ d))
              pure v
  do
    r <- try pruneCache
    case r of
      Left _ -> warn "Failed to initialize the suspense store cleanup task. Ensure you're using it in a browser with `requestIdleCallback` support."
      Right _ -> pure unit
  pure $ SuspenseStore { get: getCacheOrBackend }

newtype SuspenseStore
  = SuspenseStore
  { get :: Maybe Milliseconds -> StoreKey -> Effect (SuspenseResult Opaque)
  }

_get :: forall k v s. HasBackend k v s => SuspenseStore -> Maybe Milliseconds -> k -> Suspended v
_get (SuspenseStore s) d k =
  Suspended do
    let
      storable = mkStorable k
    r <- s.get d storable
    pure (map (fromOpaque k) r)

-- An opaque "cacheable". `StoreKey` packages up a `HasBackend` instance
-- so the cache can use its `k -> String` and `k -> Aff v` functions
-- without knowing about the internal types stored within the cache.
data StoreKey
  = StoreKey
    (forall x. (forall k v s. HasBackend k v s => k -> x) -> x)

mkStorable :: forall k v s. HasBackend k v s => k -> StoreKey
mkStorable k = StoreKey \f -> f k

class Storable k where
  toKey :: k -> String
  toAff :: k -> Aff Opaque

instance storableStoreKey :: Storable StoreKey where
  toKey (StoreKey impl) = impl \k -> joinWith "" [ typeKey k, "[ ", fromKey k, " ]" ]
    where
    typeKey :: forall k v s. HasBackend k v s => k -> String
    typeKey _ = reflectSymbol (SProxy :: _ s)
  toAff (StoreKey impl) = impl \k -> map (toOpaque k) (backend k)

data Opaque

class HasOpaque k v | k -> v where
  toOpaque :: k -> v -> Opaque
  fromOpaque :: k -> Opaque -> v

instance hasOpaque :: HasBackend k v s => HasOpaque k v where
  toOpaque _ = unsafeCoerce
  fromOpaque _ = unsafeCoerce
