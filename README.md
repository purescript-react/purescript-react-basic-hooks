# react-basic-hooks [![Build Status](https://github.com/spicydonuts/purescript-react-basic-hooks/actions/workflows/node.js.yml/badge.svg)](https://github.com/spicydonuts/purescript-react-basic-hooks/actions/workflows/node.js.yml)

`react-basic-hooks` is a React hook API for [react-basic](https://github.com/lumihq/purescript-react-basic).

_Note:_ This library supports React `>=16.8.0` with full React 19 support. For more info on hooks, see [React's documentation](https://react.dev/reference/react).

I recommend using PureScript's "qualified do" syntax whilst using this library (it's used in the examples, the `React.do` bits).
It became available in the `0.12.2` compiler release.

This library provides the `React.Basic.Hooks` module, which can completely replace the `React.Basic` module.
It borrows a few types from the current `React.Basic` module like `ReactComponent` and `JSX` to make it easy to use both versions in the same project.
If we prefer this API over the existing react-basic API, we may eventually replace `React.Basic` with this implementation.

## React Version Support

- **React 16.8+**: Core hooks (useState, useEffect, useReducer, useRef, useContext, useMemo, useDebugValue, useLayoutEffect)
- **React 18+**: useId, useTransition, useDeferredValue, useSyncExternalStore, useInsertionEffect
- **React 19+**: useOptimistic, useActionState, useEffectEvent (experimental)

## Example

```purs
mkCounter :: Component Int
mkCounter = do
  component "Counter" \initialValue -> React.do
    counter /\ setCounter <- useState initialValue

    pure
      $ R.button
          { onClick: handler_ do
              setCounter (_ + 1)
          , children:
              [ R.text $ "Increment: " <> show counter ]
          }
```

## React 19 Hooks

### useOptimistic

Optimistically update the UI whilst waiting for an async action to complete. The optimistic state automatically reverts to the actual state when the action finishes.

```purs
mkMessageList :: Component Props
mkMessageList = do
  component "MessageList" \{ messages } -> React.do
    optimisticMessages /\ addOptimisticMessage <- useOptimistic messages \state newMessage ->
      Array.snoc state newMessage
    
    isPending /\ startTransition <- useTransition
    
    let handleSend message = startTransition do
          addOptimisticMessage message
          -- Async operation to send message to server
          sendToServer message
    
    pure $ R.div_ (map renderMessage optimisticMessages)
```

### useActionState

Manage form actions with built-in pending state. The action function receives the previous state and form data, making it ideal for form submissions. Uses Effect for synchronous operations.

```purs
mkForm :: Component Unit
mkForm = do
  component "Form" \_ -> React.do
    state /\ (formAction /\ isPending) <- useActionState initialState updateFn
      where
        updateFn prevState formData = do
          -- Process form submission (Effect version)
          result <- submitToServer formData
          pure (Result.fromEither result)
    
    pure $ R.button
      { disabled: isPending
      , onClick: handler_ (formAction myFormData)
      , children: [ R.text if isPending then "Submitting..." else "Submit" ]
      }
```

For progressive enhancement (form works without JavaScript), use `useActionStateWithPermalink`:

```purs
state /\ (formAction /\ isPending) <- useActionStateWithPermalink initialState updateFn "/api/submit"

pure $ R.form
  { action: formAction  -- Falls back to /api/submit without JS
  , children: [ ... ]
  }
```

### useAffActionState

Aff version of `useActionState` for async operations. Available in `React.Basic.Hooks.Aff`. Uses Aff for natural async handling.

```purs
import React.Basic.Hooks.Aff (useAffActionState)

mkForm :: Component Unit
mkForm = do
  component "Form" \_ -> React.do
    state /\ (formAction /\ isPending) <- useAffActionState initialState affFn
      where
        affFn prevState formData = do
          -- Process form submission (Aff version - natural async!)
          result <- Aff.submitToServer formData
          pure (Result.fromEither result)
    
    pure $ R.button
      { disabled: isPending
      , onClick: handler_ (formAction myFormData)
      , children: [ R.text if isPending then "Submitting..." else "Submit" ]
      }
```

With permalink: `useAffActionStateWithPermalink initialState affFn "/api/submit"`

### useEffectEvent

Extract non-reactive logic from Effects. The returned function can access the latest props and state without causing the Effect to re-run when those values change.

```purs
mkComponent :: Component Props
mkComponent = do
  component "Component" \{ url, onSuccess } -> React.do
    count /\ setCount <- useState 0
    
    -- onSuccess can use the latest count without re-running the effect
    onSuccessEvent <- useEffectEvent \data -> do
      onSuccess data count
    
    -- Effect only re-runs when url changes, not when count changes
    useEffect url do
      response <- fetchData url
      onSuccessEvent response
      pure mempty
    
    pure $ R.div_ [ ... ]
```

## Available Hooks

### Core Hooks (React 16.8+)
- `useState` / `useState'` — State management
- `useEffect` / `useEffectOnce` / `useEffectAlways` — Side effects
- `useLayoutEffect` — Synchronous layout effects
- `useReducer` — State management with reducers
- `useRef` — Mutable refs
- `useContext` — Context consumption
- `useMemo` — Memoised computation
- `useDebugValue` — DevTools debugging labels

### React 18 Hooks
- `useId` — Unique ID generation
- `useTransition` — Concurrent transitions
- `useDeferredValue` — Deferred value updates
- `useSyncExternalStore` — External store synchronisation
- `useInsertionEffect` — DOM mutation effects

### React 19 Hooks
- `useOptimistic` — Optimistic UI updates
- `useActionState` — Form action management
- `useEffectEvent` — Non-reactive effect logic (experimental)

### Additional Features
- `memo` / `memo'` — Component memoisation
- `component` — Component creation
- Custom hooks via `React.Basic.Hooks.Aff` for async effects
- `React.Basic.Hooks.Suspense` for Suspense support
- `React.Basic.Hooks.ErrorBoundary` for error boundaries
```
