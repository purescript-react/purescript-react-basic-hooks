# react-basic-hooks [![Build Status](https://travis-ci.com/spicydonuts/purescript-react-basic-hooks.svg?branch=main)](https://travis-ci.com/spicydonuts/purescript-react-basic-hooks)

`react-basic-hooks` is a React hook API for [react-basic](https://github.com/lumihq/purescript-react-basic).

_Note:_ This API relies on React `>=16.8.0`. For more info on hooks, see [React's documentation](https://reactjs.org/docs/hooks-intro.html).

I recommend using PureScript's "qualified do" syntax while using this library (it's used in the examples, the `React.do` bits).
It became available in the `0.12.2` compiler release.

This library provides the `React.Basic.Hooks` module, which can completely replace the `React.Basic` module.
It borrows a few types from the current `React.Basic` module like `ReactComponent` and `JSX` to make it easy to use both versions in the same project.
If we prefer this API over the existing react-basic API, we may eventually replace `React.Basic` with this implementation.

## Example

```purs
mkCounter :: Component Int
mkCounter = do
  component "Counter" \initialValue -> React.do
    counter /\ setCounter <- useState initialValue

    pure
      $ R.button
          { onClick: handler_ do
              setCounter (_ + 1)
          , children:
              [ R.text $ "Increment: " <> show counter ]
          }
```
