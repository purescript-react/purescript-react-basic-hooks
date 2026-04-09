import React from "react";

export const viewTransition_ = React.unstable_ViewTransition;

export const mkClassName = (str) => str;

export const mkAnimationMap = (obj) => obj;

export const toCallback_ = (fn) => (instance, types) => {
  const cleanup = fn(instance)(types)();
  return cleanup;
};
