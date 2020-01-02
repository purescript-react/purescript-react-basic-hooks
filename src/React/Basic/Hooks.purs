module React.Basic.Hooks
  ( component
  , componentWithChildren
  , componentFromHook
  , ReactChildren
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
  , readRef
  , readRefMaybe
  , writeRef
  , renderRef
  , renderRefMaybe
  , useRef
  , UseContext
  , useContext
  , UseMemo
  , useMemo
  , UseCallback
  , useCallback
  , UseEqCache
  , useEqCache
  , UnsafeReference(..)
  , displayName
  , module React.Basic.Hooks.Internal
  , module React.Basic
  , module Data.Tuple.Nested
  ) where

import Prelude hiding (bind, discard)
import Data.Function.Uncurried (Fn2, mkFn2)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Nullable (Nullable, toMaybe)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, mkEffectFn1, runEffectFn1, runEffectFn2, runEffectFn3)
import Prelude (bind) as Prelude
import Prim.Row (class Lacks)
import React.Basic (JSX, ReactComponent, ReactContext, Ref, consumer, contextConsumer, contextProvider, createContext, element, elementKeyed, empty, keyed, fragment, provider)
import React.Basic.Hooks.Internal (Hook, Pure, Render, bind, discard, coerceHook, unsafeHook, unsafeRenderEffect)
import Unsafe.Coerce (unsafeCoerce)
import Unsafe.Reference (unsafeRefEq)

-- | Create a React component given a display name and render function.
-- | Creating components is effectful because React uses the function
-- | instance as the component's "identity" or "type". Components should
-- | be created during a bootstrap phase and not within component
-- | lifecycles or render functions. See `componentWithChildren` if
-- | you need to use the `children` prop.
component ::
  forall hooks props.
  Lacks "children" props =>
  Lacks "key" props =>
  Lacks "ref" props =>
  String ->
  ({ | props } -> Render Unit hooks JSX) ->
  Effect (ReactComponent { | props })
component = unsafeComponent

-- | Create a React component given a display name and render function.
-- | This is the same as `component` but allows the use of the `children`
-- | prop.
componentWithChildren ::
  forall hooks props children.
  Lacks "key" props =>
  Lacks "ref" props =>
  String ->
  ({ children :: ReactChildren children | props } -> Render Unit hooks JSX) ->
  Effect (ReactComponent { children :: ReactChildren children | props })
componentWithChildren = unsafeComponent

-- | Convert a hook to a render-prop component. The value returned from the
-- | hook will be passed to the `render` prop, a function from that value
-- | to `JSX`.
-- |
-- | This function is useful for consuming a hook within a non-hook component.
componentFromHook ::
  forall hooks props r.
  Lacks "children" props =>
  Lacks "key" props =>
  Lacks "ref" props =>
  String ->
  ({ render :: r -> JSX | props } -> Hook hooks r) ->
  Effect (ReactComponent { render :: r -> JSX | props })
componentFromHook name propsToHook = do
  component name \props -> map props.render $ propsToHook props

unsafeComponent ::
  forall hooks props.
  Lacks "key" props =>
  Lacks "ref" props =>
  String ->
  ({ | props } -> Render Unit hooks JSX) ->
  Effect (ReactComponent { | props })
unsafeComponent name renderFn =
  let
    c =
      unsafeReactFunctionComponent
        ( mkEffectFn1
            ( \props ->
                unsafeDiscardRenderEffects (renderFn props)
            )
        )
  in
    runEffectFn2 unsafeSetDisplayName name c

unsafeDiscardRenderEffects :: forall x y a. Render x y a -> Effect a
unsafeDiscardRenderEffects = unsafeCoerce

unsafeReactFunctionComponent :: forall props. EffectFn1 props JSX -> ReactComponent props
unsafeReactFunctionComponent = unsafeCoerce

data ReactChildren a

foreign import reactChildrenToArray :: forall a. ReactChildren a -> Array a

reactChildrenFromArray :: forall a. Array a -> ReactChildren a
reactChildrenFromArray = unsafeCoerce

memo ::
  forall props.
  Effect (ReactComponent props) ->
  Effect (ReactComponent props)
memo = flip Prelude.bind (runEffectFn1 memo_)

foreign import data UseState :: Type -> Type -> Type

useState ::
  forall state.
  state ->
  Hook (UseState state) (state /\ ((state -> state) -> Effect Unit))
useState initialState =
  unsafeHook do
    runEffectFn2 useState_ (mkFn2 Tuple) initialState

foreign import data UseEffect :: Type -> Type -> Type

-- | The effect will be run when the component is mounted, and the effect
-- | returned from the function will be run on cleanup
useEffect ::
  forall key.
  Eq key =>
  key ->
  Effect (Effect Unit) ->
  Hook (UseEffect key) Unit
useEffect key effect = unsafeHook (runEffectFn3 useEffect_ (mkFn2 eq) key effect)

foreign import data UseLayoutEffect :: Type -> Type -> Type

useLayoutEffect ::
  forall key.
  Eq key =>
  key ->
  Effect (Effect Unit) ->
  Hook (UseLayoutEffect key) Unit
useLayoutEffect keys effect = unsafeHook (runEffectFn3 useLayoutEffect_ (mkFn2 eq) keys effect)

foreign import data UseReducer :: Type -> Type -> Type -> Type

useReducer ::
  forall state action.
  state ->
  (state -> action -> state) ->
  Hook (UseReducer state action) (state /\ (action -> Effect Unit))
useReducer initialState reducer =
  unsafeHook do
    runEffectFn3 useReducer_
      (mkFn2 Tuple)
      (mkFn2 reducer)
      initialState

foreign import data UseRef :: Type -> Type -> Type

useRef :: forall a. a -> Hook (UseRef a) (Ref a)
useRef initialValue =
  unsafeHook do
    runEffectFn1 useRef_ initialValue

readRef :: forall a. Ref a -> Effect a
readRef = runEffectFn1 readRef_

readRefMaybe :: forall a. Ref (Nullable a) -> Effect (Maybe a)
readRefMaybe a = map toMaybe (readRef a)

writeRef :: forall a. Ref a -> a -> Effect Unit
writeRef = runEffectFn2 writeRef_

renderRef :: forall a. Ref a -> Pure a
renderRef ref = unsafeRenderEffect (readRef ref)

renderRefMaybe :: forall a. Ref (Nullable a) -> Pure (Maybe a)
renderRefMaybe a = unsafeRenderEffect (readRefMaybe a)

foreign import data UseContext :: Type -> Type -> Type

useContext :: forall a. ReactContext a -> Hook (UseContext a) a
useContext context = unsafeHook (runEffectFn1 useContext_ context)

foreign import data UseMemo :: Type -> Type -> Type -> Type

useMemo ::
  forall key a.
  Eq key =>
  key ->
  (Unit -> a) ->
  Hook (UseMemo key a) a
useMemo key computeA = unsafeHook (runEffectFn3 useMemo_ (mkFn2 eq) key computeA)

foreign import data UseCallback :: Type -> Type -> Type -> Type

useCallback ::
  forall key a.
  Eq key =>
  key ->
  a ->
  Hook (UseCallback key a) a
useCallback key computeA = unsafeHook (runEffectFn3 useCallback_ (mkFn2 eq) key computeA)

foreign import data UseEqCache :: Type -> Type -> Type

useEqCache ::
  forall a.
  Eq a =>
  a ->
  Hook (UseCallback a a) a
useEqCache a = unsafeHook (runEffectFn2 useEqCache_ (mkFn2 eq) a)

newtype UnsafeReference a
  = UnsafeReference a

derive instance newtypeUnsafeReference :: Newtype (UnsafeReference a) _

instance eqUnsafeReference :: Eq (UnsafeReference a) where
  eq = unsafeRefEq

-- | Retrieve the Display Name from a `ReactComponent`. Useful for debugging and improving
-- | error messages in logs.
-- |
-- | __*See also:* `component`__
foreign import displayName ::
  forall props.
  ReactComponent props ->
  String

-- |
-- | Internal utility or FFI functions
-- |
foreign import memo_ ::
  forall props.
  EffectFn1
    (ReactComponent props)
    (ReactComponent props)

foreign import unsafeSetDisplayName ::
  forall props.
  EffectFn2 String (ReactComponent props) (ReactComponent props)

foreign import useState_ ::
  forall state.
  EffectFn2
    (forall a b. Fn2 a b (a /\ b))
    state
    (state /\ ((state -> state) -> Effect Unit))

foreign import useEffect_ ::
  forall key.
  EffectFn3
    (Fn2 key key Boolean)
    key
    (Effect (Effect Unit))
    Unit

foreign import useLayoutEffect_ ::
  forall key.
  EffectFn3
    (Fn2 key key Boolean)
    key
    (Effect (Effect Unit))
    Unit

foreign import useReducer_ ::
  forall state action.
  EffectFn3
    (forall a b. Fn2 a b (a /\ b))
    (Fn2 state action state)
    state
    (state /\ (action -> Effect Unit))

foreign import readRef_ ::
  forall a.
  EffectFn1
    (Ref a)
    a

foreign import writeRef_ ::
  forall a.
  EffectFn2
    (Ref a)
    a
    Unit

foreign import useRef_ ::
  forall a.
  EffectFn1
    a
    (Ref a)

foreign import useContext_ ::
  forall a.
  EffectFn1
    (ReactContext a)
    a

foreign import useMemo_ ::
  forall key a.
  EffectFn3
    (Fn2 key key Boolean)
    key
    (Unit -> a)
    a

foreign import useCallback_ ::
  forall key a.
  EffectFn3
    (Fn2 key key Boolean)
    key
    a
    a

foreign import useEqCache_ ::
  forall a.
  EffectFn2
    (Fn2 a a Boolean)
    a
    a
