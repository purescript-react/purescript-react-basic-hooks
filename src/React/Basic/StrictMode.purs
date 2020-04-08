-- TODO: move this file to `react-basic`
module React.Basic.StrictMode where

import Prelude
import React.Basic (JSX, ReactComponent, element)

strictMode :: JSX -> JSX
strictMode = element strictMode_ <<< { children: _ }

foreign import strictMode_ :: ReactComponent { children :: JSX }
