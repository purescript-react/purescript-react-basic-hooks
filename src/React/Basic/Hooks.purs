module React.Basic.Hooks
  ( Component
  , component
  , reactComponent
  , reactComponentWithChildren
  , reactComponentFromHook
  , ReactChildren
  , reactChildrenToArray
  , reactChildrenFromArray
  , memo
  , memo'
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
  , useInsertionEffect
  , useInsertionEffectOnce
  , useInsertionEffectAlways
  , UseInsertionEffect
  , Reducer
  , mkReducer
  , runReducer
  , useReducer
  , UseReducer
  , readRef
  , readRefMaybe
  , writeRef
  , useRef
  , UseRef
  , useContext
  , UseContext
  , useEqCache
  , UseEqCache
  , useMemo
  , UseMemo
  , useDebugValue
  , UseDebugValue
  , useId
  , UseId
  , useTransition
  , UseTransition
  , useDeferredValue
  , UseDeferredValue
  , useSyncExternalStore
  , useSyncExternalStore'
  , UseSyncExternalStore
  , useOptimistic
  , UseOptimistic
  , useActionState
  , useActionStateWithPermalink
  , UseActionState
  , useEffectEvent
  , UseEffectEvent
  , UnsafeReference(..)
  , displayName
  , module React.Basic.Hooks.Internal
  , module React.Basic
  , module Data.Tuple.Nested
  ) where

import Prelude hiding (bind, discard)

import Data.Bifunctor (rmap)
import Data.Function.Uncurried (Fn2, Fn3, mkFn2, mkFn3, runFn2, runFn3)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, EffectFn4, mkEffectFn1, mkEffectFn2, runEffectFn1, runEffectFn2, runEffectFn3, runEffectFn4)
import Prelude (bind) as Prelude
import Prim.Row (class Lacks)
import React.Basic (JSX, ReactComponent, ReactContext, Ref, consumer, contextConsumer, contextProvider, createContext, element, elementKeyed, empty, keyed, fragment, provider)
import React.Basic.Hooks.Internal (Hook, HookApply, Pure, Render, bind, discard, coerceHook, unsafeHook, unsafeRenderEffect, type (&))
import Unsafe.Coerce (unsafeCoerce)
import Unsafe.Reference (unsafeRefEq)

--| A simple type alias to clean up component definitions.
type Component props
  = Effect (props -> JSX)

--| Create a component function given a display name and render function.
--| Creating components is effectful because React uses the function
--| instance as the component's "identity" or "type". Components should
--| be created during a bootstrap phase and not within component
--| lifecycles or render functions.
component ::
  forall hooks props.
  String ->
  (props -> Render Unit hooks JSX) ->
  Component props
component name renderFn = Prelude.do
  c <- reactComponent name (renderFn <<< _.nested)
  pure (element c <<< { nested: _ })

--| Create a React component given a display name and render function.
--| Creating components is effectful because React uses the function
--| instance as the component's "identity" or "type". Components should
--| be created during a bootstrap phase and not within component
--| lifecycles or render functions. See `componentWithChildren` if
--| you need to use the `children` prop.
reactComponent ::
  forall hooks props.
  Lacks "children" props =>
  Lacks "key" props =>
  Lacks "ref" props =>
  String ->
  ({ | props } -> Render Unit hooks JSX) ->
  Effect (ReactComponent { | props })
reactComponent = unsafeReactComponent

--| Create a React component given a display name and render function.
--| This is the same as `component` but allows the use of the `children`
--| prop.
reactComponentWithChildren ::
  forall hooks props children.
  Lacks "key" props =>
  Lacks "ref" props =>
  String ->
  ({ children :: ReactChildren children | props } -> Render Unit hooks JSX) ->
  Effect (ReactComponent { children :: ReactChildren children | props })
reactComponentWithChildren = unsafeReactComponent

--| Convert a hook to a render-prop component. The value returned from the
--| hook will be passed to the `render` prop, a function from that value
--| to `JSX`.
--|
--| This function is useful for consuming a hook within a non-hook component.
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

data ReactChildren :: forall k. k -> Type
data ReactChildren a

foreign import reactChildrenToArray :: forall a. ReactChildren a -> Array a

reactChildrenFromArray :: forall a. Array a -> ReactChildren a
reactChildrenFromArray = unsafeCoerce

--| Prevents a component from re-rendering if its new props are referentially
--| equal to its old props (not value-based equality -- this is due to the
--| underlying React implementation).
--| Prefer `memo'` for more PureScript-friendldy behavior.
memo ::
  forall props.
  Effect (ReactComponent props) ->
  Effect (ReactComponent props)
memo = flip Prelude.bind (runEffectFn1 memo_)

--| Similar to `memo` but takes a function to compare previous and new props.
--| For example:
--|
--| ```purs
--| mkMyComponent :: Effect (ReactComponent { id :: Int })
--| mkMyComponent =
--|   memo' eq do
--|     reactComponent "MyComponent" \{ id } -> React.do
--|       ...
--| ```
memo' ::
  forall props.
  (props -> props -> Boolean) ->
  Effect (ReactComponent props) ->
  Effect (ReactComponent props)
memo' arePropsEqual comp = Prelude.do
  c <- comp
  runEffectFn2 memoEq_ c (mkFn2 arePropsEqual)

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
useState' initialState = useState initialState <#> rmap (_ <<< const)

foreign import data UseState :: Type -> Type -> Type

--| Runs the given effect when the component is mounted and any time the given
--| dependencies change. The effect should return its cleanup function. For
--| example, if the effect registers a global event listener, it should return
--| an Effect which removes the listener.
--|
--| ```purs
--| useEffect deps do
--|   timeoutId <- setTimeout 1000 (logShow deps)
--|   pure (clearTimeout timeoutId)
--| ```
--|
--| If no cleanup is needed, use `pure (pure unit)` or `pure mempty` to return
--| a no-op Effect
--|
--| ```purs
--| useEffect deps do
--|   logShow deps
--|   pure mempty
--| ```
useEffect ::
  forall deps.
  Eq deps =>
  deps ->
  Effect (Effect Unit) ->
  Hook (UseEffect deps) Unit
useEffect deps effect =
  unsafeHook do
    runEffectFn3 useEffect_ (mkFn2 eq) deps effect

--| Like `useEffect`, but the effect is only performed a single time per component
--| instance. Prefer `useEffect` with a proper dependency list whenever possible!
useEffectOnce :: Effect (Effect Unit) -> Hook (UseEffect Unit) Unit
useEffectOnce effect = unsafeHook (runEffectFn3 useEffect_ (mkFn2 \_ _ -> true) unit effect)

--| Like `useEffect`, but the effect is performed on every render. Prefer `useEffect`
--| with a proper dependency list whenever possible!
useEffectAlways :: Effect (Effect Unit) -> Hook (UseEffect Unit) Unit
useEffectAlways effect = unsafeHook (runEffectFn1 useEffectAlways_ effect)

foreign import data UseEffect :: Type -> Type -> Type

--| Like `useEffect`, but the effect is performed synchronously after the browser has
--| calculated layout. Useful for reading properties from the DOM that are not available
--| before layout, such as element sizes and positions. Prefer `useEffect` whenever
--| possible to avoid blocking browser painting.
useLayoutEffect ::
  forall deps.
  Eq deps =>
  deps ->
  Effect (Effect Unit) ->
  Hook (UseLayoutEffect deps) Unit
useLayoutEffect deps effect = unsafeHook (runEffectFn3 useLayoutEffect_ (mkFn2 eq) deps effect)

--| Like `useLayoutEffect`, but the effect is only performed a single time per component
--| instance. Prefer `useLayoutEffect` with a proper dependency list whenever possible!
useLayoutEffectOnce :: Effect (Effect Unit) -> Hook (UseLayoutEffect Unit) Unit
useLayoutEffectOnce effect = unsafeHook (runEffectFn3 useLayoutEffect_ (mkFn2 \_ _ -> true) unit effect)

--| Like `useLayoutEffect`, but the effect is performed on every render. Prefer `useLayoutEffect`
--| with a proper dependency list whenever possible!
useLayoutEffectAlways :: Effect (Effect Unit) -> Hook (UseLayoutEffect Unit) Unit
useLayoutEffectAlways effect = unsafeHook (runEffectFn1 useLayoutEffectAlways_ effect)

foreign import data UseLayoutEffect :: Type -> Type -> Type

useInsertionEffect ::
  forall deps.
  Eq deps =>
  deps ->
  Effect (Effect Unit) ->
  Hook (UseInsertionEffect deps) Unit
useInsertionEffect deps effect = unsafeHook (runEffectFn3 useInsertionEffect_ (mkFn2 eq) deps effect)

--| Like `useInsertionEffect`, but the effect is only performed a single time per component
--| instance. Prefer `useInsertionEffect` with a proper dependency list whenever possible!
useInsertionEffectOnce :: Effect (Effect Unit) -> Hook (UseInsertionEffect Unit) Unit
useInsertionEffectOnce effect = unsafeHook (runEffectFn3 useInsertionEffect_ (mkFn2 \_ _ -> true) unit effect)

--| Like `useInsertionEffect`, but the effect is performed on every render. Prefer `useInsertionEffect`
--| with a proper dependency list whenever possible!
useInsertionEffectAlways :: Effect (Effect Unit) -> Hook (UseInsertionEffect Unit) Unit
useInsertionEffectAlways effect = unsafeHook (runEffectFn1 useInsertionEffectAlways_ effect)

foreign import data UseInsertionEffect :: Type -> Type -> Type

newtype Reducer state action
  = Reducer (Fn2 state action state)

--| Creating reducer functions for React is effectful because
--| React uses the function instance's reference to optimize
--| rendering behavior.
mkReducer :: forall state action. (state -> action -> state) -> Effect (Reducer state action)
mkReducer = pure <<< Reducer <<< mkFn2

--| Run a wrapped `Reducer` function as a normal function (like `runFn2`).
--| Useful for testing, simulating actions, or building more complicated
--| hooks on top of `useReducer`
runReducer :: forall state action. Reducer state action -> state -> action -> state
runReducer (Reducer reducer) = runFn2 reducer

--| Use `mkReducer` to construct a reducer function.
useReducer ::
  forall state action.
  state ->
  Reducer state action ->
  Hook (UseReducer state action) (state /\ (action -> Effect Unit))
useReducer initialState (Reducer reducer) =
  unsafeHook do
    runEffectFn3 useReducer_
      (mkFn2 Tuple)
      reducer
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

--| Cache an instance of a value, replacing it when `eq` returns `false`.
--|
--| This is a low-level performance optimization tool. It can be useful
--| for optimizing a component's props for use with `memo`, where
--| JavaScript instance equality matters.
useEqCache ::
  forall a.
  Eq a =>
  a ->
  Hook (UseEqCache a) a
useEqCache a =
  unsafeHook do
    runEffectFn2 useEqCache_ (mkFn2 eq) a

foreign import data UseEqCache :: Type -> Type -> Type

--| Lazily compute a value. The result is cached until the `deps` change.
useMemo ::
  forall deps a.
  Eq deps =>
  deps ->
  (Unit -> a) ->
  Hook (UseMemo deps a) a
useMemo deps computeA =
  unsafeHook do
    runEffectFn3 useMemo_ (mkFn2 eq) deps computeA

foreign import data UseMemo :: Type -> Type -> Type -> Type

--| Use this hook to display a label for custom hooks in React DevTools
useDebugValue :: forall a. a -> (a -> String) -> Hook (UseDebugValue a) Unit
useDebugValue debugValue display = unsafeHook (runEffectFn2 useDebugValue_ debugValue display)

foreign import data UseDebugValue :: Type -> Type -> Type

foreign import data UseId :: Type -> Type
useId :: Hook UseId String
useId = unsafeHook useId_

foreign import data UseTransition :: Type -> Type
useTransition ::
  Hook UseTransition (Boolean /\ ((Effect Unit) -> Effect Unit))
useTransition = unsafeHook $ runEffectFn1 useTransition_ (mkFn2 Tuple)

foreign import data UseDeferredValue :: Type -> Type -> Type
useDeferredValue :: forall a. a -> Hook (UseDeferredValue a) a
useDeferredValue a = unsafeHook $ runEffectFn1 useDeferredValue_ a

foreign import data UseSyncExternalStore :: Type -> Type -> Type
useSyncExternalStore :: forall a.
  ((Effect Unit) -> Effect (Effect Unit))
  -> (Effect a)
  -> (Effect a)
  -> Hook (UseSyncExternalStore a) a
useSyncExternalStore subscribe getSnapshot getServerSnapshot =
  unsafeHook $
    runEffectFn3 useSyncExternalStore3_
      (mkEffectFn1 subscribe)
      getSnapshot
      getServerSnapshot
useSyncExternalStore' :: forall a.
  ((Effect Unit) -> Effect (Effect Unit))
  -> (Effect a)
  -> Hook (UseSyncExternalStore a) a
useSyncExternalStore' subscribe getSnapshot =
  unsafeHook $
    runEffectFn2 useSyncExternalStore2_ (mkEffectFn1 subscribe) getSnapshot

foreign import data UseOptimistic :: Type -> Type -> Type -> Type

--| Optimistically update state before an async action completes.
--| The optimistic state automatically reverts when the action finishes.
--|
--| ```purs
--| optimisticMessages /\ addOptimisticMessage <- useOptimistic messages \state newMessage ->
--|   Array.snoc state newMessage
--|
--| let handleSend = do
--|       addOptimisticMessage newMessage
--|       sendToServer newMessage
--| ```
useOptimistic ::
  forall state action.
  state ->
  (state -> action -> state) ->
  Hook (UseOptimistic state action) (state /\ (action -> Effect Unit))
useOptimistic state updateFn =
  unsafeHook do
    runEffectFn3 useOptimistic_
      (mkFn2 Tuple)
      state
      (mkFn2 updateFn)

foreign import data UseActionState :: Type -> Type -> Type -> Type

--| Manage form actions with built-in pending state and error handling.
--| The action function receives the previous state and form data.
--|
--| ```purs
--| state /\ formAction /\ isPending <- useActionState initialState updateFn
--|
--| pure $ R.form
--|   { children:
--|       [ R.button
--|           { disabled: isPending
--|           , onClick: handler_ (formAction myFormData)
--|           }
--|       ]
--|   }
--| ```
useActionState ::
  forall state formData.
  state ->
  (state -> formData -> Effect state) ->
  Hook (UseActionState state formData) (state /\ ((formData -> Effect Unit) /\ Boolean))
useActionState initialState fn =
  unsafeHook do
    runEffectFn3 useActionState_
      mkTuple3
      (mkEffectFn2 fn)
      initialState
  where
  mkTuple3 :: forall a b c. Fn3 a b c (a /\ (b /\ c))
  mkTuple3 = mkFn3 \a b c -> Tuple a (Tuple b c)

--| Like `useActionState` but with a permalink for progressive enhancement.
--| The form will submit to this URL if JavaScript is disabled.
--|
--| ```purs
--| state /\ formAction /\ isPending <- useActionStateWithPermalink initialState updateFn "/api/submit"
--|
--| pure $ R.form
--|   { action: formAction
--|   , children: [ ... ]
--|   }
--| ```
useActionStateWithPermalink ::
  forall state formData.
  state ->
  (state -> formData -> Effect state) ->
  String ->
  Hook (UseActionState state formData) (state /\ ((formData -> Effect Unit) /\ Boolean))
useActionStateWithPermalink initialState fn permalink =
  unsafeHook do
    runEffectFn4 useActionStateWithPermalink_
      mkTuple3
      (mkEffectFn2 fn)
      initialState
      permalink
  where
  mkTuple3 :: forall a b c. Fn3 a b c (a /\ (b /\ c))
  mkTuple3 = mkFn3 \a b c -> Tuple a (Tuple b c)

foreign import data UseEffectEvent :: Type -> Type -> Type

--| Extract non-reactive logic from Effects. The returned function can access
--| the latest props and state without causing the Effect to re-run.
--|
--| ```purs
--| onClick <- useEffectEvent handleClick
--|
--| useEffect url do
--|   -- onClick can use latest state without re-running when state changes
--|   onClick unit
--|   pure mempty
--| ```
useEffectEvent ::
  forall a b.
  (a -> Effect b) ->
  Hook (UseEffectEvent a) (a -> Effect b)
useEffectEvent callback =
  unsafeHook (runEffectFn1 useEffectEvent_ callback)

newtype UnsafeReference a
  = UnsafeReference a

derive instance newtypeUnsafeReference :: Newtype (UnsafeReference a) _

instance eqUnsafeReference :: Eq (UnsafeReference a) where
  eq = unsafeRefEq

--| Retrieve the Display Name from a `ReactComponent`. Useful for debugging and improving
--| error messages in logs.
--|
--| __*See also:* `component`__
foreign import displayName ::
  forall props.
  ReactComponent props ->
  String

--|
--| Internal utility or FFI functions
--|
foreign import memo_ ::
  forall props.
  EffectFn1
    (ReactComponent props)
    (ReactComponent props)

foreign import memoEq_ ::
  forall props.
  EffectFn2
    (ReactComponent props)
    (Fn2 props props Boolean)
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

foreign import useEffectAlways_ ::
  EffectFn1
    (Effect (Effect Unit))
    Unit

foreign import useLayoutEffect_ ::
  forall deps.
  EffectFn3
    (Fn2 deps deps Boolean)
    deps
    (Effect (Effect Unit))
    Unit

foreign import useLayoutEffectAlways_ ::
  EffectFn1
    (Effect (Effect Unit))
    Unit

foreign import useInsertionEffect_ ::
  forall deps.
  EffectFn3
    (Fn2 deps deps Boolean)
    deps
    (Effect (Effect Unit))
    Unit

foreign import useInsertionEffectAlways_ ::
  EffectFn1
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

foreign import useEqCache_ ::
  forall a.
  EffectFn2
    (Fn2 a a Boolean)
    a
    a

foreign import useMemo_ ::
  forall deps a.
  EffectFn3
    (Fn2 deps deps Boolean)
    deps
    (Unit -> a)
    a

foreign import useDebugValue_ ::
  forall a.
  EffectFn2
    a
    (a -> String)
    Unit

foreign import useId_ :: Effect String

foreign import useTransition_
  :: forall a b. EffectFn1 (Fn2 a b (a /\ b))
    (Boolean /\ ((Effect Unit) -> Effect Unit))

foreign import useDeferredValue_ :: forall a. EffectFn1 a a

foreign import useSyncExternalStore2_ :: forall a. EffectFn2
  (EffectFn1 (Effect Unit) (Effect Unit)) -- subscribe
  (Effect a) -- getSnapshot
  a

foreign import useSyncExternalStore3_ :: forall a. EffectFn3
  (EffectFn1 (Effect Unit) (Effect Unit)) -- subscribe
  (Effect a) -- getSnapshot
  (Effect a) -- getServerSnapshot
  a

foreign import useOptimistic_ ::
  forall state action.
  EffectFn3
    (forall a b. Fn2 a b (a /\ b))
    state
    (Fn2 state action state)
    (state /\ (action -> Effect Unit))

foreign import useActionState_ ::
  forall state formData.
  EffectFn3
    (forall a b c. Fn3 a b c (a /\ (b /\ c)))
    (EffectFn2 state formData state)
    state
    (state /\ ((formData -> Effect Unit) /\ Boolean))

foreign import useActionStateWithPermalink_ ::
  forall state formData.
  EffectFn4
    (forall a b c. Fn3 a b c (a /\ (b /\ c)))
    (EffectFn2 state formData state)
    state
    String
    (state /\ ((formData -> Effect Unit) /\ Boolean))

foreign import useEffectEvent_ ::
  forall a b.
  EffectFn1
    (a -> Effect b)
    (a -> Effect b)