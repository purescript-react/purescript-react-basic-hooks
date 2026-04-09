module React.Basic.Hooks.Activity
  ( activity
  , ActivityMode(..)
  ) where

import React.Basic.Hooks (JSX, ReactComponent, element)

data ActivityMode
  = Visible
  | Hidden

activity :: { mode :: ActivityMode, children :: Array JSX } -> JSX
activity props = element activity_ { mode: modeToString props.mode, children: props.children }
  where
  modeToString Visible = "visible"
  modeToString Hidden = "hidden"

foreign import activity_ :: ReactComponent { mode :: String, children :: Array JSX }
