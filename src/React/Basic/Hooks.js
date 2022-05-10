"use strict";

const React = require("react");

const useEqCache = (eq, a) => {
  const memoRef = React.useRef(a);
  if (memoRef.current !== a && !eq(memoRef.current, a)) {
    memoRef.current = a;
  }
  return memoRef.current;
};

exports.reactChildrenToArray = (children) => React.Children.toArray(children);

exports.memo_ = React.memo;
exports.memoEq_ = React.memo;

exports.useState_ = (tuple, initialState) => {
  const r = React.useState(
    typeof initialState === 'function'
      ? (() => initialState)
      : initialState
  );
  const state = r[0];
  const setState = r[1];
  if (!setState.hasOwnProperty("$$reactBasicHooks$$cachedSetState")) {
    setState.$$reactBasicHooks$$cachedSetState = (update) => () =>
      setState(update);
  }
  return tuple(state, setState.$$reactBasicHooks$$cachedSetState);
};

exports.useEffect_ = (eq, deps, effect) => {
  const memoizedKey = useEqCache(eq, deps);
  React.useEffect(effect, [memoizedKey]);
};

exports.useEffectAlways_ = (effect) => React.useEffect(effect);

exports.useLayoutEffect_ = (eq, deps, effect) => {
  const memoizedKey = useEqCache(eq, deps);
  React.useLayoutEffect(effect, [memoizedKey]);
};

exports.useLayoutEffectAlways_ = (effect) => React.useLayoutEffect(effect);

exports.useReducer_ = (tuple, reducer, initialState) => {
  const r = React.useReducer(reducer, initialState);
  const state = r[0];
  const dispatch = r[1];
  if (!dispatch.hasOwnProperty("$$reactBasicHooks$$cachedDispatch")) {
    dispatch.$$reactBasicHooks$$cachedDispatch = (action) => () =>
      dispatch(action);
  }
  return tuple(state, dispatch.$$reactBasicHooks$$cachedDispatch);
};

exports.useRef_ = React.useRef;

exports.readRef_ = (ref) => ref.current;

exports.writeRef_ = (ref, a) => {
  ref.current = a;
};

exports.useContext_ = React.useContext;

exports.useEqCache_ = useEqCache;

exports.useMemo_ = (eq, deps, computeA) => {
  const memoizedKey = useEqCache(eq, deps);
  return React.useMemo(computeA, [memoizedKey]);
};

exports.useDebugValue_ = React.useDebugValue;

exports.unsafeSetDisplayName = (displayName, component) => {
  component.displayName = displayName;
  component.toString = () => displayName;
  return component;
};

exports.displayName = (component) =>
  typeof component === "string"
    ? component
    : component.displayName || "[unknown]";
