"use strict";

var React = require("react");

exports.memo_ = React.memo;

exports.useState_ = function(tuple, initialState) {
  var r = React.useState(initialState);
  var state = r[0];
  var setState = r[1];
  if (!setState.hasOwnProperty("$$reactBasicHooks$$cachedSetState")) {
    setState.$$reactBasicHooks$$cachedSetState = function(update) {
      return function() {
        return setState(update);
      };
    };
  }
  return tuple(state, setState.$$reactBasicHooks$$cachedSetState);
};

exports.useEffect_ = function(eq, key, effect) {
  var memoizedKey = exports.useEqCache_(eq, key);
  React.useEffect(effect, [memoizedKey]);
};

exports.useLayoutEffect_ = function(eq, key, effect) {
  var memoizedKey = exports.useEqCache_(eq, key);
  React.useLayoutEffect(effect, [memoizedKey]);
};

exports.useReducer_ = function(tuple, reducer, initialState, initialAction) {
  var r = React.useReducer(reducer, initialState, initialAction);
  var state = r[0];
  var dispatch = r[1];
  if (!dispatch.hasOwnProperty("$$reactBasicHooks$$cachedDispatch")) {
    dispatch.$$reactBasicHooks$$cachedDispatch = function(action) {
      return function() {
        return dispatch(action);
      };
    };
  }
  return tuple(state, dispatch.$$reactBasicHooks$$cachedDispatch);
};

exports.useRef_ = React.useRef;

exports.readRef_ = function(ref) {
  return ref.current;
};

exports.writeRef_ = function(ref, a) {
  ref.current = a;
};

exports.useContext_ = React.useContext;

exports.useMemo_ = function(eq, key, computeA) {
  var memoizedKey = exports.useEqCache_(eq, key);
  return React.useMemo(computeA, [memoizedKey]);
};

exports.useCallback_ = function(eq, key, cb) {
  var memoizedKey = exports.useEqCache_(eq, key);
  return React.useCallback(cb, [memoizedKey]);
};

exports.useEqCache_ = function(eq, a) {
  var memoRef = React.useRef(a);
  if (memoRef.current !== a && !eq(memoRef.current, a)) {
    memoRef.current = a;
  }
  return memoRef.current;
};

exports.unsafeSetDisplayName = function(displayName, component) {
  component.displayName = displayName;
  component.toString = function() {
    return displayName;
  };
  return component;
};

exports.displayName = function(component) {
  return typeof component === "string"
    ? component
    : component.displayName || "[unknown]";
};
