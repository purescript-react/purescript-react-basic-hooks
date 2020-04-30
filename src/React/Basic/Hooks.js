"use strict";

var React = require("react");

exports.reactChildrenToArray = function (children) {
  return React.Children.toArray(children);
};

exports.memo_ = React.memo;

exports.useState_ = function (tuple, initialState) {
  var r = React.useState(initialState);
  var state = r[0];
  var setState = r[1];
  if (!setState.hasOwnProperty("$$reactBasicHooks$$cachedSetState")) {
    setState.$$reactBasicHooks$$cachedSetState = function (update) {
      return function () {
        return setState(update);
      };
    };
  }
  return tuple(state, setState.$$reactBasicHooks$$cachedSetState);
};

exports.useEffect_ = function (eq, deps, effect) {
  var memoizedKey = exports.useMemo_(eq, deps);
  React.useEffect(effect, [memoizedKey]);
};

exports.useEffectAlways_ = function (effect) {
  React.useEffect(effect);
};

exports.useLayoutEffect_ = function (eq, deps, effect) {
  var memoizedKey = exports.useMemo_(eq, deps);
  React.useLayoutEffect(effect, [memoizedKey]);
};

exports.useLayoutEffectAlways_ = function (effect) {
  React.useLayoutEffect(effect);
};

exports.useReducer_ = function (tuple, reducer, initialState, initialAction) {
  var r = React.useReducer(reducer, initialState, initialAction);
  var state = r[0];
  var dispatch = r[1];
  if (!dispatch.hasOwnProperty("$$reactBasicHooks$$cachedDispatch")) {
    dispatch.$$reactBasicHooks$$cachedDispatch = function (action) {
      return function () {
        return dispatch(action);
      };
    };
  }
  return tuple(state, dispatch.$$reactBasicHooks$$cachedDispatch);
};

exports.useRef_ = React.useRef;

exports.readRef_ = function (ref) {
  return ref.current;
};

exports.writeRef_ = function (ref, a) {
  ref.current = a;
};

exports.useContext_ = React.useContext;

exports.useMemo_ = function (eq, a) {
  var memoRef = React.useRef(a);
  if (memoRef.current !== a && !eq(memoRef.current, a)) {
    memoRef.current = a;
  }
  return memoRef.current;
};

exports.useLazy_ = function (eq, deps, computeA) {
  var memoizedKey = exports.useMemo_(eq, deps);
  return React.useMemo(computeA, [memoizedKey]);
};

exports.useDebugValue_ = React.useDebugValue;

exports.unsafeSetDisplayName = function (displayName, component) {
  component.displayName = displayName;
  component.toString = function () {
    return displayName;
  };
  return component;
};

exports.displayName = function (component) {
  return typeof component === "string"
    ? component
    : component.displayName || "[unknown]";
};
