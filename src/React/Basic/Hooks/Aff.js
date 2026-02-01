import React from "react";

// Aff version - accepts a function that returns a Promise
export function useAffActionState_(tuple3, affFn, initialState) {
  const [state, formAction, isPending] = React.useActionState(
    // affFn returns a Promise, React will await it
    async (prevState, formData) => {
      return await affFn(prevState, formData);
    },
    initialState
  );
  if (!formAction.hasOwnProperty("$$reactBasicHooks$$cachedFormAction")) {
    formAction.$$reactBasicHooks$$cachedFormAction = (formData) => () =>
      formAction(formData);
  }
  return tuple3(state, formAction.$$reactBasicHooks$$cachedFormAction, isPending);
}

export function useAffActionStateWithPermalink_(tuple3, affFn, initialState, permalink) {
  const [state, formAction, isPending] = React.useActionState(
    // affFn returns a Promise, React will await it
    async (prevState, formData) => {
      return await affFn(prevState, formData);
    },
    initialState,
    permalink
  );
  if (!formAction.hasOwnProperty("$$reactBasicHooks$$cachedFormAction")) {
    formAction.$$reactBasicHooks$$cachedFormAction = (formData) => () =>
      formAction(formData);
  }
  return tuple3(state, formAction.$$reactBasicHooks$$cachedFormAction, isPending);
}

