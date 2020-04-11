-- TODO: move this file to `react-basic`
module React.Basic.DOM.Concurrent where

import Prelude
import Effect (Effect)
import React.Basic (JSX)
import Web.DOM (Element)

foreign import renderConcurrentMode :: JSX -> Element -> Effect Unit
