import React from "react";

export const viewTransition_ = React.ViewTransition;

export const mkClassName = (str) => str;

export const mkAnimationMap = (obj) => obj;

export const toCallback_ = (fn) => (element, types) => fn(element)(types);
