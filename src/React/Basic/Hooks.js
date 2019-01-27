"use strict";

var React = require("react");

exports.memo_ = React.memo;

exports.useState_ = function(tuple, initialState) {
  var r = React.useState(initialState);
  var state = r[0];
  var setState = r[1];
  return tuple(state, function(update) {
    return function() {
      return setState(update);
    };
  });
};

exports.useEffect_ = function(eq, key, effect) {
  var memoizedKey = exports.useMemo_(eq, key);
  React.useEffect(effect, [memoizedKey]);
};

exports.useLayoutEffect_ = function(eq, key, effect) {
  var memoizedKey = exports.useMemo_(eq, key);
  React.useLayoutEffect(effect, [memoizedKey]);
};

exports.useReducer_ = function(tuple, reducer, initialState, initialAction) {
  var r = React.useReducer(reducer, initialState, initialAction);
  var state = r[0];
  var dispatch = r[1];
  return tuple(state, function(action) {
    return function() {
      return dispatch(action);
    };
  });
};

exports.useRef_ = React.useRef;

exports.readRef_ = function(ref) {
  return ref.current;
};

exports.writeRef_ = function(ref, a) {
  ref.current = a;
};

exports.useContext_ = React.useContext;

exports.createContext_ = React.createContext;

exports.contextProvider_ = function(context) {
  return context.Provider;
};

exports.useMemo_ = function(eq, a) {
  var memoRef = React.useRef(a);
  if (memoRef.current !== a && !eq(memoRef.current, a)) {
    memoRef.current = a;
  }
  return memoRef.current;
};

exports.useMemoLazy_ = function(eq, key, computeA) {
  var memoizedKey = exports.useMemo_(eq, key);
  return React.useMemo(computeA, [memoizedKey]);
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
