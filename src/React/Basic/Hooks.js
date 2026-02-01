import React from "react";

const useEqCache = (eq, a) => {
  const memoRef = React.useRef(a);
  if (memoRef.current !== a && !eq(memoRef.current, a)) {
    memoRef.current = a;
  }
  return memoRef.current;
};

export function reactChildrenToArray(children) {
  return React.Children.toArray(children);
}

export const memo_ = React.memo;
export const memoEq_ = React.memo;

export function useState_(tuple, initialState) {
  const [state, setState] = React.useState(
    typeof initialState === "function" ? () => initialState : initialState
  );
  if (!setState.hasOwnProperty("$$reactBasicHooks$$cachedSetState")) {
    setState.$$reactBasicHooks$$cachedSetState = (update) => () =>
      setState(update);
  }
  return tuple(state, setState.$$reactBasicHooks$$cachedSetState);
}

export function useEffect_(eq, deps, effect) {
  const memoizedKey = useEqCache(eq, deps);
  React.useEffect(effect, [memoizedKey]);
}

export function useEffectAlways_(effect) {
  return React.useEffect(effect);
}

export function useLayoutEffect_(eq, deps, effect) {
  const memoizedKey = useEqCache(eq, deps);
  React.useLayoutEffect(effect, [memoizedKey]);
}

export function useLayoutEffectAlways_(effect) {
  return React.useLayoutEffect(effect);
}

export function useInsertionEffect_(eq, deps, effect) {
  const memoizedKey = useEqCache(eq, deps);
  React.useInsertionEffect(effect, [memoizedKey]);
}

export function useInsertionEffectAlways_(effect) {
  React.useInsertionEffect(effect);
}

export function useReducer_(tuple, reducer, initialState) {
  const [state, dispatch] = React.useReducer(reducer, initialState);
  if (!dispatch.hasOwnProperty("$$reactBasicHooks$$cachedDispatch")) {
    dispatch.$$reactBasicHooks$$cachedDispatch = (action) => () =>
      dispatch(action);
  }
  return tuple(state, dispatch.$$reactBasicHooks$$cachedDispatch);
}

export const useRef_ = React.useRef;

export function readRef_(ref) {
  return ref.current;
}

export function writeRef_(ref, a) {
  ref.current = a;
}

export const useContext_ = React.useContext;
export { useEqCache as useEqCache_ };

export function useMemo_(eq, deps, computeA) {
  const memoizedKey = useEqCache(eq, deps);
  return React.useMemo(computeA, [memoizedKey]);
}

export const useDebugValue_ = React.useDebugValue;

export const useId_ = React.useId

export function useTransition_(tuple) {
  const [isPending, startTransitionImpl] = React.useTransition()
  const startTransition = (update) => () => startTransitionImpl(update)
  return tuple(isPending, startTransition);
}

export const useDeferredValue_ = React.useDeferredValue

export const useSyncExternalStore2_ = React.useSyncExternalStore
export const useSyncExternalStore3_ = React.useSyncExternalStore

export function unsafeSetDisplayName(displayName, component) {
  component.displayName = displayName;
  component.toString = () => displayName;
  return component;
}

export function displayName(component) {
  return typeof component === "string"
    ? component
    : component.displayName || "[unknown]";
}

export function useOptimistic_(tuple, state, updateFn) {
  const [optimisticState, addOptimistic] = React.useOptimistic(state, updateFn);
  if (!addOptimistic.hasOwnProperty("$$reactBasicHooks$$cachedAddOptimistic")) {
    addOptimistic.$$reactBasicHooks$$cachedAddOptimistic = (action) => () =>
      addOptimistic(action);
  }
  return tuple(optimisticState, addOptimistic.$$reactBasicHooks$$cachedAddOptimistic);
}

export function useActionState_(tuple3, fn, initialState) {
  const [state, formAction, isPending] = React.useActionState(
    fn,
    initialState
  );
  if (!formAction.hasOwnProperty("$$reactBasicHooks$$cachedFormAction")) {
    formAction.$$reactBasicHooks$$cachedFormAction = (formData) => () =>
      formAction(formData);
  }
  return tuple3(state, formAction.$$reactBasicHooks$$cachedFormAction, isPending);
}

export function useActionStateWithPermalink_(tuple3, fn, initialState, permalink) {
  const [state, formAction, isPending] = React.useActionState(
    fn,
    initialState,
    permalink
  );
  if (!formAction.hasOwnProperty("$$reactBasicHooks$$cachedFormAction")) {
    formAction.$$reactBasicHooks$$cachedFormAction = (formData) => () =>
      formAction(formData);
  }
  return tuple3(state, formAction.$$reactBasicHooks$$cachedFormAction, isPending);
}

export const useEffectEvent_ = React.useEffectEvent || React.experimental_useEffectEvent;
