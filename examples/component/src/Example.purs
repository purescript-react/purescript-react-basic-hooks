module Example where

import Prelude
import React.Basic.DOM as R
import React.Basic.Hooks (Component, component)
import ToggleButton (mkToggleButton)

mkExample :: Component Unit
mkExample = do
  -- create the child components this parent will use
  toggleButton <- mkToggleButton
  -- create the parent component
  component "Container" \_ ->
    pure
      $ R.div
          { children:
              [ toggleButton { label: "A" }
              , toggleButton { label: "B" }
              ]
          }
