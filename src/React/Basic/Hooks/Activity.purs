module React.Basic.Hooks.Activity
  ( activity
  , ActivityMode(..)
  ) where

import Prelude
import React.Basic.Hooks (JSX, ReactComponent, element)

-- | Represents the visibility mode of an Activity boundary
data ActivityMode
  = Visible
  | Hidden

instance Show ActivityMode where
  show Visible = "visible"
  show Hidden = "hidden"

-- | The Activity component lets you hide and restore the UI and internal
-- | state of its children while preserving their state and DOM.
-- |
-- | When hidden, children are hidden using CSS `display: none`, but their
-- | component state and DOM structure are preserved. Effects are destroyed
-- | when hidden and re-created when made visible again.
-- |
-- | Example:
-- | ```purescript
-- | activity { mode: Hidden, children: [ myComponent ] }
-- | ```
-- |
-- | Note: Activity is available in React 19.2+ experimental/canary channels
activity :: { mode :: ActivityMode, children :: Array JSX } -> JSX
activity props = element activity_
  { mode: show props.mode
  , children: props.children
  }

foreign import activity_ :: ReactComponent { mode :: String, children :: Array JSX }
