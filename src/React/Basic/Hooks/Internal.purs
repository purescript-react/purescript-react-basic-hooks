module React.Basic.Hooks.Internal
  ( Render
  , coerceHook
  , unsafeHook
  , unsafeRenderEffect
  , Pure
  , Hook
  , bind
  , discard
  , HookApply
  , type (&)
  ) where

import Prelude hiding (bind)

import Control.Applicative.Indexed (class IxApplicative)
import Control.Apply.Indexed (class IxApply)
import Control.Bind.Indexed (class IxBind, ibind)
import Control.Monad.Indexed (class IxMonad)
import Data.Functor.Indexed (class IxFunctor)
import Data.Newtype (class Newtype)
import Effect (Effect)
import Prelude (bind) as Prelude
import Type.Equality (class TypeEquals)

--| Render represents the effects allowed within a React component's
--| body, i.e. during "render". This includes hooks and ends with
--| returning JSX (see `pure`), but does not allow arbitrary side
--| effects.
--|
--| The `x` and `y` type arguments represent the stack of effects that this
--| `Render` implements, with `x` being the stack at the start of this
--| `Render`, and `y` the stack at the end.
--|
--| See
--| [purescript-indexed-monad](https://pursuit.purescript.org/packages/purescript-indexed-monad)
--| to understand how the order of the stack is enforced at the type level.
newtype Render :: Type -> Type -> Type -> Type
newtype Render x y a
  = Render (Effect a)

--| Rename/alias a chain of hooks. Useful for exposing a single
--| "clean" type when creating a hook to improve error messages
--| and hide implementation details, particularly for libraries
--| hiding internal info.
--|
--| For example, the following alias is technically correct but
--| when inspecting types or error messages the alias is expanded
--| to the full original type and `UseAff` is never seen:
--|
--| ```purs
--| type UseAff deps a hooks
--|   = UseEffect deps (UseState (Result a) hooks)
--|
--| useAff :: ... -> Hook (UseAff deps a) (Result a)
--| useAff deps aff = React.do
--|   ...
--| ```
--|
--| `coerceHook` allows the same code to safely export a newtype
--| instead, hiding the internal implementation:
--|
--| ```purs
--| newtype UseAff deps a hooks
--|   = UseAff (UseEffect deps (UseState (Result a) hooks))
--|
--| derive instance ntUseAff :: Newtype (UseAff deps a hooks) _
--|
--| useAff :: ... -> Hook (UseAff deps a) (Result a)
--| useAff deps aff = coerceHook React.do
--|   ...
--| ```
--|
--|
--|
coerceHook ::
  forall hooks oldHook newHook a.
  Newtype newHook oldHook =>
  Render hooks oldHook a ->
  Render hooks newHook a
coerceHook (Render a) = Render a

--| Promote an arbitrary Effect to a Hook.
--|
--| This is unsafe because it allows arbitrary
--| effects to be performed during a render, which
--| may cause them to be run many times by React.
--| This function is primarily for constructing
--| new hooks using the FFI. If you just want to
--| alias a safe hook's effects, prefer `coerceHook`.
--|
--| It's also unsafe because the author of the hook
--| type (the `newHook` type variable used here) _MUST_
--| contain all relevant types. For example, `UseState`
--| has a phantom type to track the type of the value contained.
--| `useEffect` tracks the type used as the deps. `useAff` tracks
--| both the deps and the resulting response's type. Forgetting
--| to do this allows the consumer to reorder hook effects. If
--| `useState` didn't track the return type the following
--| extremely unsafe code would be allowed:
--|
--| ```purs
--| React.do
--|   if xyz then
--|     _ <- useState 0
--|     useState Nothing
--|   else
--|     s <- useState Nothing
--|     _ <- useState 0
--|     pure s
--|   ...
--| ```
--|
--| The same applies to `deps` in these examples as they use
--| `Eq` and a reorder would allow React to pass incorrect
--| types into the `eq` function!
unsafeHook ::
  forall newHook a.
  Effect a -> Hook newHook a
unsafeHook = Render

--| Promote an arbitrary Effect to a Pure render effect.
--|
--| This is unsafe because it allows arbitrary
--| effects to be performed during a render, which
--| may cause them to be run many times by React.
--| You should almost always prefer `useEffect`!
unsafeRenderEffect :: forall a. Effect a -> Pure a
unsafeRenderEffect = Render

--| Type alias used to lift otherwise pure functionality into the Render type.
--| Not commonly used.
type Pure a
  = forall hooks. Render hooks hooks a

--| Type alias for Render representing a hook.
--|
--| The `newHook` argument is a type constructor which takes a set of existing
--| effects and generates a type with a new set of effects (produced by this
--| hook) stacked on top.
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

instance ixMonadRender :: IxMonad Render

--| Exported for use with qualified-do syntax
bind :: forall a b x y z m. IxBind m => m x y a -> (a -> m y z b) -> m x z b
bind = ibind

--| Exported for use with qualified-do syntax
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

instance monadRender :: TypeEquals x y => Monad (Render x y)

instance semigroupRender :: (TypeEquals x y, Semigroup a) => Semigroup (Render x y a) where
  append (Render a) (Render b) = Render (append a b)

instance monoidRender :: (TypeEquals x y, Monoid a) => Monoid (Render x y a) where
  mempty = Render mempty

type HookApply hooks (newHook :: Type -> Type)
  = newHook hooks

--| Applies a new hook to a hook chain, with the innermost hook as the left argument.
--| This allows hook chains to be written in reverse order, aligning them with the
--| order they appear when actually used in do-notation.
--| ```purescript
--| type UseCustomHook hooks = UseEffect String (UseState Int hooks)
--| type UseCustomHook' hooks = hooks & UseState Int & UseEffect String
--| ```
infixl 0 type HookApply as &