# react-basic-hooks

This is an experimental implementation of React hooks on [react-basic](https://github.com/lumihq/purescript-react-basic).

*Warning:* This API is *experimental* and relies on alpha-release React versions.
It's here to allow experimentation while we get feedback on the API and wait for an official React release which supports hooks.
For more info on hooks, see [React's documentation](https://reactjs.org/docs/hooks-intro.html).

It's also recommended while using this module to use PureScript's new "qualified do" syntax (it's used in the examples, `React.do`).
It's available in the  `0.12.2` release.

If we prefer this API over the existing react-basic API, we may eventually replace it with this.

*A note on Refs:* The `Ref` type is useful for passing to DOM nodes, but while this module remains a small extension to the existing react-basic library it won't be possible to pass a `ref` prop to the native DOM components.
In the meantime, use `element (unsafeCreateDOMComponent "div") { ref: elementRef }`.
