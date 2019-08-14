module React.Basic.Hooks.Internal
  ( Render
  , coerceHook
  , unsafeHook
  , unsafeRenderEffect
  , Pure
  , Hook
  , bind
  , discard
  ) where

import Prelude hiding (bind)
import Control.Applicative.Indexed (class IxApplicative)
import Control.Apply.Indexed (class IxApply)
import Control.Bind.Indexed (class IxBind, ibind)
import Data.Functor.Indexed (class IxFunctor)
import Data.Newtype (class Newtype)
import Effect (Effect)
import Prelude (bind) as Prelude
import Type.Equality (class TypeEquals)

-- | Render represents the effects allowed within a React component's
-- | body, i.e. during "render". This includes hooks and ends with
-- | returning JSX (see `pure`), but does not allow arbitrary side
-- | effects.
newtype Render x y a
  = Render (Effect a)

-- | Alias a chain of hooks. Useful for exposing a single "clean"
-- | type when creating a hook to improve error messages
-- | and hide implementation details.
-- |
-- | For example, the following alias is technically correct but
-- | when inspecting types or error messages the alias is expanded
-- | to the full original type:
-- |
-- | ```purs
-- | type UseNodeDistance hooks = UseEffect Unit (UseState Int (UseRef (Nullable Node) hooks))
-- |
-- | useNodeDistanceFromMouse :: Hook UseNodeDistance (Int /\ (Ref (Nullable Node)))
-- | ```
-- |
-- | `aliasHook` allows TODO!
-- | when inspecting types or error messages the alias is expanded
-- | to the full original type:
-- |
-- | ```purs
-- | type UseNodeDistance hooks = UseEffect Unit (UseState Int (UseRef (Nullable Node) hooks))
-- |
-- | useNodeDistanceFromMouse :: Hook UseNodeDistance (Int /\ (Ref (Nullable Node)))
-- | ```
-- |
-- |
-- |
coerceHook ::
  forall hooks oldHook newHook a.
  Newtype newHook oldHook =>
  Render hooks oldHook a ->
  Render hooks newHook a
coerceHook (Render a) = Render a

-- | Promote an arbitrary Effect to a Hook.
-- |
-- | This is unsafe because it allows arbitrary
-- | effects to be performed during a render, which
-- | may cause them to be run many times by React.
-- | This function is primarily for constructing
-- | new hooks using the FFI. If you just want to
-- | alias a safe hook's effects, prefer `coerceHook`.
unsafeHook ::
  forall newHook a.
  Effect a -> Hook newHook a
unsafeHook = Render

-- | Promote an arbitrary Effect to a Pure render effect.
-- |
-- | This is unsafe because it allows arbitrary
-- | effects to be performed during a render, which
-- | may cause them to be run many times by React.
-- | You should almost always prefer `useEffect`!
unsafeRenderEffect :: forall a. Effect a -> Pure a
unsafeRenderEffect = Render

-- | Discards
type Pure a
  = forall hooks. Render hooks hooks a

type Hook (newHook :: Type -> Type) a
  = forall hooks. Render hooks (newHook hooks) a

instance ixFunctorRender :: IxFunctor Render where
  imap f (Render a) = Render (map f a)

instance ixApplyRender :: IxApply Render where
  iapply (Render f) (Render a) = Render (apply f a)

instance ixApplicativeRender :: IxApplicative Render where
  ipure a = Render (pure a)

instance ixBindRender :: IxBind Render where
  ibind (Render m) f = Render (Prelude.bind m \a -> case f a of Render b -> b)

-- | Exported for use with qualified-do syntax
bind :: forall a b x y z m. IxBind m => m x y a -> (a -> m y z b) -> m x z b
bind = ibind

-- | Exported for use with qualified-do syntax
discard :: forall a b x y z m. IxBind m => m x y a -> (a -> m y z b) -> m x z b
discard = ibind

instance functorRender :: Functor (Render x y) where
  map f (Render a) = Render (map f a)

instance applyRender :: TypeEquals x y => Apply (Render x y) where
  apply (Render f) (Render a) = Render (apply f a)

instance applicativeRender :: TypeEquals x y => Applicative (Render x y) where
  pure a = Render (pure a)

instance bindRender :: TypeEquals x y => Bind (Render x y) where
  bind (Render m) f = Render (Prelude.bind m \a -> case f a of Render b -> b)

instance semigroupRender :: (TypeEquals x y, Semigroup a) => Semigroup (Render x y a) where
  append (Render a) (Render b) = Render (append a b)

instance monoidRender :: (TypeEquals x y, Monoid a) => Monoid (Render x y a) where
  mempty = Render mempty
