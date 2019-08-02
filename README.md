# react-basic-hooks [![CircleCI](https://circleci.com/gh/spicydonuts/purescript-react-basic-hooks.svg?style=svg)](https://circleci.com/gh/spicydonuts/purescript-react-basic-hooks)

`react-basic-hooks` adds React hook support to [react-basic](https://github.com/lumihq/purescript-react-basic)!

_Note:_ This API relies on recent React versions (>= 16.8.0).
For more info on hooks, see [React's documentation](https://reactjs.org/docs/hooks-intro.html).

I recommend using PureScript's "qualified do" syntax while using this library (it's used in the examples, the `React.do` bits).
It became available in the `0.12.2` compiler release.

This library provides the `React.Basic.Hooks` module, which can completely replace the `React.Basic` module.
It borrows a few types from the current `React.Basic` module like `ReactComponent` and `JSX` to make it easy to use both versions in the same project.
If we prefer this API over the existing react-basic API, we may eventually replace `React.Basic` with this implementation.

## Example

```purs
mkCounter :: CreateComponent {}
mkCounter = do
  component "Counter" \props -> React.do
    counter /\ setCounter <- useState 0

    pure
      $ R.button
        { onClick: handler_ do
            setCounter (_ + 1)
        , children:
            [ R.text $ "Increment: " <> show counter ]
        }
```

More examples:

- [Counter with an effect](./examples/counter/src/Counter.purs)
- [Reducer/action-style](./examples/reducer/src/Reducer.purs)
- [Controlled inputs](./examples/controlled-input/src/ControlledInput.purs)
- Components: [Parent](./examples/component/src/Container.purs) and [Child](./examples/component/src/ToggleButton.purs)
- [Refs to DOM nodes](./examples/refs/src/Refs.purs) (and extracting hook logic from a component for reuse)
- [A Todo App](./examples/todo-app/src/TodoApp.purs) (components, inputs, state)
- [Context](./examples/context/src/Context.purs) (creating and consuming React context)
- [Aff helper](./examples/aff/src/AffEx.purs) (async state management)
