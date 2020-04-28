module React.Basic.Hooks
  ( Component
  , component
  , reactComponent
  , reactComponentWithChildren
  , reactComponentFromHook
  , ReactChildren
  , memo
  , useState
  , useState'
  , UseState
  , useEffect
  , useEffectOnce
  , useEffectAlways
  , UseEffect
  , useLayoutEffect
  , useLayoutEffectOnce
  , useLayoutEffectAlways
  , UseLayoutEffect
  , useReducer
  , UseReducer
  , readRef
  , readRefMaybe
  , writeRef
  , reactChildrenFromArray
  , reactChildrenToArray
  , useRef
  , UseRef
  , useContext
  , UseContext
  , useMemo
  , UseMemo
  , useLazy
  , UseLazy
  , UnsafeReference(..)
  , displayName
  , module React.Basic.Hooks.Internal
  , module React.Basic
  , module Data.Tuple.Nested
  ) where

import Prelude hiding (bind, discard)
import Data.Bifunctor (rmap)
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

-- | A simple type alias to clean up component definitions.
type Component props
  = Effect (props -> JSX)

-- | Create a component function given a display name and render function.
-- | Creating components is effectful because React uses the function
-- | instance as the component's "identity" or "type". Components should
-- | be created during a bootstrap phase and not within component
-- | lifecycles or render functions.
component ::
  forall hooks props.
  String ->
  (props -> Render Unit hooks JSX) ->
  Component props
component name renderFn = Prelude.do
  c <- reactComponent name (renderFn <<< _.nested)
  pure (element c <<< { nested: _ })

-- | Create a React component given a display name and render function.
-- | Creating components is effectful because React uses the function
-- | instance as the component's "identity" or "type". Components should
-- | be created during a bootstrap phase and not within component
-- | lifecycles or render functions. See `componentWithChildren` if
-- | you need to use the `children` prop.
reactComponent ::
  forall hooks props.
  Lacks "children" props =>
  Lacks "key" props =>
  Lacks "ref" props =>
  String ->
  ({ | props } -> Render Unit hooks JSX) ->
  Effect (ReactComponent { | props })
reactComponent = unsafeReactComponent

-- | Create a React component given a display name and render function.
-- | This is the same as `component` but allows the use of the `children`
-- | prop.
reactComponentWithChildren ::
  forall hooks props children.
  Lacks "key" props =>
  Lacks "ref" props =>
  String ->
  ({ children :: ReactChildren children | props } -> Render Unit hooks JSX) ->
  Effect (ReactComponent { children :: ReactChildren children | props })
reactComponentWithChildren = unsafeReactComponent

-- | Convert a hook to a render-prop component. The value returned from the
-- | hook will be passed to the `render` prop, a function from that value
-- | to `JSX`.
-- |
-- | This function is useful for consuming a hook within a non-hook component.
reactComponentFromHook ::
  forall hooks props r.
  Lacks "children" props =>
  Lacks "key" props =>
  Lacks "ref" props =>
  String ->
  ({ render :: r -> JSX | props } -> Hook hooks r) ->
  Effect (ReactComponent { render :: r -> JSX | props })
reactComponentFromHook name propsToHook = do
  reactComponent name \props -> map props.render $ propsToHook props

unsafeReactComponent ::
  forall hooks props.
  Lacks "key" props =>
  Lacks "ref" props =>
  String ->
  ({ | props } -> Render Unit hooks JSX) ->
  Effect (ReactComponent { | props })
unsafeReactComponent name renderFn =
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

-- | Prevents a component from re-rendering if its new props are referentially
-- | equal to its old props (not value-based equality -- this is due to the underlying
-- | React implementation).
memo ::
  forall props.
  Effect (ReactComponent props) ->
  Effect (ReactComponent props)
memo = flip Prelude.bind (runEffectFn1 memo_)

useState ::
  forall state.
  state ->
  Hook (UseState state) (state /\ ((state -> state) -> Effect Unit))
useState initialState =
  unsafeHook do
    runEffectFn2 useState_ (mkFn2 Tuple) initialState

useState' ::
  forall state.
  state ->
  Hook (UseState state) (state /\ (state -> Effect Unit))
useState' initialState =
  useState initialState <#> rmap (_ <<< const)

foreign import data UseState :: Type -> Type -> Type

-- | Runs the given effect when the component is mounted and any time the given
-- | dependencies change. The effect should return its cleanup function. For
-- | example, if the effect registers a global event listener, it should return
-- | and Effect which removes the listener.
useEffect ::
  forall deps.
  Eq deps =>
  deps ->
  Effect (Effect Unit) ->
  Hook (UseEffect deps) Unit
useEffect deps effect = unsafeHook (runEffectFn3 useEffect_ (mkFn2 eq) deps effect)

-- | Like `useEffect`, but the effect is only performed a single time per component
-- | instance. Prefer `useEffect` with a proper dependency list whenever possible!
useEffectOnce :: Effect (Effect Unit) -> Hook (UseEffect Unit) Unit
useEffectOnce effect = unsafeHook (runEffectFn3 useEffect_ (mkFn2 \_ _ -> true) unit effect)

-- | Like `useEffect`, but the effect is performed on every render. Prefer `useEffect`
-- | with a proper dependency list whenever possible!
useEffectAlways :: Effect (Effect Unit) -> Hook (UseEffect Unit) Unit
useEffectAlways effect = unsafeHook (runEffectFn3 useEffect_ (mkFn2 \_ _ -> false) unit effect)

foreign import data UseEffect :: Type -> Type -> Type

-- | Like `useEffect`, but the effect is performed on every render. Prefer `useEffect`
-- | with a proper dependency list whenever possible!
useLayoutEffect ::
  forall deps.
  Eq deps =>
  deps ->
  Effect (Effect Unit) ->
  Hook (UseLayoutEffect deps) Unit
useLayoutEffect keys effect = unsafeHook (runEffectFn3 useLayoutEffect_ (mkFn2 eq) keys effect)

-- | Like `useLayoutEffect`, but the effect is only performed a single time per component
-- | instance. Prefer `useLayoutEffect` with a proper dependency list whenever possible!
useLayoutEffectOnce :: Effect (Effect Unit) -> Hook (UseLayoutEffect Unit) Unit
useLayoutEffectOnce effect = unsafeHook (runEffectFn3 useLayoutEffect_ (mkFn2 \_ _ -> true) unit effect)

-- | Like `useLayoutEffect`, but the effect is performed on every render. Prefer `useLayoutEffect`
-- | with a proper dependency list whenever possible!
useLayoutEffectAlways :: Effect (Effect Unit) -> Hook (UseLayoutEffect Unit) Unit
useLayoutEffectAlways effect = unsafeHook (runEffectFn3 useLayoutEffect_ (mkFn2 \_ _ -> false) unit effect)

foreign import data UseLayoutEffect :: Type -> Type -> Type

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

foreign import data UseReducer :: Type -> Type -> Type -> Type

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

foreign import data UseRef :: Type -> Type -> Type

useContext :: forall a. ReactContext a -> Hook (UseContext a) a
useContext context = unsafeHook (runEffectFn1 useContext_ context)

foreign import data UseContext :: Type -> Type -> Type

-- | Use this hook to memoize a value based on a set of deps. This is
-- | useful when you need to take advantage of `memo` and need to pass
-- | referentially equal values to a child component. This is purely
-- | a performance optimization and shouldn't change the behavior of
-- | your component.
-- |
-- | If building a value of `a` is expensive, try `useLazy`.
useMemo ::
  forall a.
  Eq a =>
  a ->
  Hook (UseMemo a) a
useMemo a = unsafeHook (runEffectFn2 useMemo_ (mkFn2 eq) a)

foreign import data UseMemo :: Type -> Type -> Type

-- | Lazily compute a value. The result is cached in the component
-- | instance until the deps change.
useLazy ::
  forall deps a.
  Eq deps =>
  deps ->
  (Unit -> a) ->
  Hook (UseLazy deps a) a
useLazy deps computeA = unsafeHook (runEffectFn3 useLazy_ (mkFn2 eq) deps computeA)

foreign import data UseLazy :: Type -> Type -> Type -> Type

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
  forall deps.
  EffectFn3
    (Fn2 deps deps Boolean)
    deps
    (Effect (Effect Unit))
    Unit

foreign import useLayoutEffect_ ::
  forall deps.
  EffectFn3
    (Fn2 deps deps Boolean)
    deps
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
  forall a.
  EffectFn2
    (Fn2 a a Boolean)
    a
    a

foreign import useLazy_ ::
  forall deps a.
  EffectFn3
    (Fn2 deps deps Boolean)
    deps
    (Unit -> a)
    a
