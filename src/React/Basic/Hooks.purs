-- | This is an experimental implementation of React hooks on [react-basic](https://github.com/lumihq/purescript-react-basic).
-- |
-- | *Warning:* This API is *experimental* and relies on alpha-release React versions.
-- | It's here to allow experimentation while we get feedback on the API and wait for an official React release which supports hooks.
-- | For more info on hooks, see [React's documentation](https://reactjs.org/docs/hooks-intro.html).
-- |
-- | It's also recommended while using this module to use PureScript's new "qualified do" syntax (it's used in the examples, `React.do`).
-- | It's available in the  `0.12.2` release.
-- |
-- | If we prefer this API over the existing react-basic API, we may eventually replace it with this.
-- |
-- | *A note on Refs:* The `Ref` type is useful for passing to DOM nodes, but while this module remains a small extension to the existing react-basic library it won't be possible to pass a `ref` prop to the native DOM components.
-- | In the meantime, use `element (unsafeCreateDOMComponent "div") { ref: elementRef }`.
module React.Basic.Hooks
  ( CreateComponent
  , component
  , memo
  , UseState
  , useState
  , UseEffect
  , useEffect
  , UseLayoutEffect
  , useLayoutEffect
  , UseReducer
  , useReducer
  , UseRef
  , Ref
  , readRef
  , readRefMaybe
  , writeRef
  , renderRef
  , renderRefMaybe
  , useRef
  , UseContext
  , Context
  , useContext
  , createContext
  , contextProvider
  , UseMemo
  , useMemo
  , UseMemoLazy
  , useMemoLazy
  , UnsafeReference(..)
  , Render
  , Pure
  , Hook
  , bind
  , discard
  , displayName
  , module React.Basic
  , module Data.Tuple
  , module Data.Tuple.Nested
  ) where

import Prelude hiding (bind, discard)

import Control.Applicative.Indexed (class IxApplicative)
import Control.Apply.Indexed (class IxApply)
import Control.Bind.Indexed (class IxBind, ibind)
import Data.Function.Uncurried (Fn2, mkFn2)
import Data.Functor.Indexed (class IxFunctor)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Nullable (Nullable, toMaybe)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (tuple2, (/\))
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, mkEffectFn1, runEffectFn1, runEffectFn2, runEffectFn3)
import Prelude (bind) as Prelude
import React.Basic (JSX, ReactComponent, empty, keyed, fragment, element, elementKeyed)
import Type.Equality (class TypeEquals)
import Unsafe.Coerce (unsafeCoerce)
import Unsafe.Reference (unsafeRefEq)

-- | Alias for convenience.
type CreateComponent props = Effect (ReactComponent props)

-- | Create a React component given a display name and render function.
-- | Creating components is effectful because React uses the function
-- | instance as the component's "identity" or "type". Components should
-- | be created during a bootstrap phase and not within component
-- | lifecycles or render functions.
component
  :: forall hooks props
   . String
  -> ({ | props } -> Render Unit hooks JSX)
  -> CreateComponent { | props }
component name renderFn =
  let c = unsafeReactFunctionComponent (mkEffectFn1 (\props -> case renderFn props of Render a -> a))
   in runEffectFn2 unsafeSetDisplayName name c

unsafeReactFunctionComponent :: forall props. EffectFn1 props JSX -> ReactComponent props
unsafeReactFunctionComponent = unsafeCoerce

memo
  :: forall props
   . CreateComponent props
  -> CreateComponent props
memo = flip Prelude.bind (runEffectFn1 memo_)

foreign import data UseState :: Type -> Type -> Type

useState
  :: forall state
   . state
  -> Hook (UseState state) (Tuple state ((state -> state) -> Effect Unit))
useState initialState = Render do
  runEffectFn2 useState_ (mkFn2 Tuple) initialState

foreign import data UseEffect :: Type -> Type -> Type

useEffect
  :: forall key
   . Eq key
  => key
  -> Effect (Effect Unit)
  -> Hook (UseEffect key) Unit
useEffect key effect = Render (runEffectFn3 useEffect_ (mkFn2 eq) key effect)

foreign import data UseLayoutEffect :: Type -> Type -> Type

useLayoutEffect
  :: forall key
   . Eq key
  => key
  -> Effect (Effect Unit)
  -> Hook (UseLayoutEffect key) Unit
useLayoutEffect keys effect = Render (runEffectFn3 useLayoutEffect_ (mkFn2 eq) keys effect)

foreign import data UseReducer :: Type -> Type -> Type -> Type

useReducer
  :: forall state action
   . state
  -> (state -> action -> state)
  -> Hook (UseReducer state action) (Tuple state (action -> Effect Unit))
useReducer initialState reducer = Render do
  runEffectFn3 useReducer_
    (mkFn2 Tuple)
    (mkFn2 reducer)
    initialState

foreign import data UseRef :: Type -> Type -> Type

foreign import data Ref :: Type -> Type

useRef :: forall a . a -> Hook (UseRef a) (Ref a)
useRef initialValue = Render do
  runEffectFn1 useRef_ initialValue

readRef :: forall a. Ref a -> Effect a
readRef = runEffectFn1 readRef_

readRefMaybe :: forall a. Ref (Nullable a) -> Effect (Maybe a)
readRefMaybe a = map toMaybe (readRef a)

writeRef :: forall a. Ref a -> a -> Effect Unit
writeRef = runEffectFn2 writeRef_

renderRef :: forall a. Ref a -> Pure a
renderRef ref = Render (readRef ref)

renderRefMaybe :: forall a. Ref (Nullable a) -> Pure (Maybe a)
renderRefMaybe a = Render (readRefMaybe a)

foreign import data UseContext :: Type -> Type -> Type

foreign import data Context :: Type -> Type

useContext :: forall a . Context a -> Hook (UseContext a) (Maybe a)
useContext context = Render (map toMaybe (runEffectFn1 useContext_ context))

foreign import createContext :: forall a. Effect (Context a)

contextProvider :: forall a. Context a -> a -> JSX -> JSX
contextProvider context a child = element (contextProvider_ context) { value: a, children: child }

foreign import data UseMemo :: Type -> Type -> Type

useMemo
  :: forall a
   . Eq a
  => a
  -> a
  -> Hook (UseMemo a) a
useMemo key a = Render (runEffectFn2 useMemo_ (mkFn2 eq) a)

foreign import data UseMemoLazy :: Type -> Type -> Type

useMemoLazy
  :: forall key a
   . Eq key
  => key
  -> (Unit -> a)
  -> Hook (UseMemoLazy a) a
useMemoLazy key computeA = Render (runEffectFn3 useMemoLazy_ (mkFn2 eq) key computeA)

newtype UnsafeReference a = UnsafeReference a
derive instance newtypeUnsafeReference :: Newtype (UnsafeReference a) _
instance eqUnsafeReference :: Eq (UnsafeReference a) where
  eq = unsafeRefEq

-- | Render represents the effects allowed within a React component's
-- | body, i.e. during "render". This includes hooks and ends with
-- | returning JSX (see `pure`), but does not allow arbitrary side
-- | effects.
newtype Render x y a = Render (Effect a)

type Pure a = forall hooks. Render hooks hooks a

type Hook (newHook :: Type -> Type) a = forall hooks. Render hooks (newHook hooks) a

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

-- | Retrieve the Display Name from a `ReactComponent`. Useful for debugging and improving
-- | error messages in logs.
-- |
-- | __*See also:* `component`__
foreign import displayName
  :: forall props
   . ReactComponent props
  -> String


-- |
-- | Internal utility or FFI functions
-- |

foreign import memo_
  :: forall props
   . EffectFn1
       (ReactComponent props)
       (ReactComponent props)

foreign import unsafeSetDisplayName
  :: forall props
   . EffectFn2 String (ReactComponent props) (ReactComponent props)

foreign import useState_
  :: forall state
   . EffectFn2
       (forall a b. Fn2 a b (Tuple a b))
       state
       (Tuple state ((state -> state) -> Effect Unit))

foreign import useEffect_
  :: forall key
   . EffectFn3
       (Fn2 key key Boolean)
       key
       (Effect (Effect Unit))
       Unit

foreign import useLayoutEffect_
  :: forall key
   . EffectFn3
       (Fn2 key key Boolean)
       key
       (Effect (Effect Unit))
       Unit

foreign import useReducer_
  :: forall state action
   . EffectFn3
       (forall a b. Fn2 a b (Tuple a b))
       (Fn2 state action state)
       state
       (Tuple state (action -> Effect Unit))

foreign import readRef_
  :: forall a
   . EffectFn1
       (Ref a)
       a

foreign import writeRef_
  :: forall a
   . EffectFn2
       (Ref a)
       a
       Unit

foreign import useRef_
  :: forall a
   . EffectFn1
       a
       (Ref a)

foreign import useContext_
  :: forall a
   . EffectFn1
       (Context a)
       (Nullable a)

foreign import contextProvider_
  :: forall a
   . Context a
  -> ReactComponent { value :: a, children :: JSX }

foreign import useMemo_
  :: forall a
   . EffectFn2
       (Fn2 a a Boolean)
       a
       a

foreign import useMemoLazy_
  :: forall key a
   . EffectFn3
       (Fn2 key key Boolean)
       key
       (Unit -> a)
       a