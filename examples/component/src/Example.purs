module Example where

import Prelude
import Effect (Effect)
import React.Basic.DOM as R
import React.Basic.Hooks (ReactComponent, component, element)
import ToggleButton (mkToggleButton)

mkExample :: Effect (ReactComponent {})
mkExample = do
  -- create the child components this parent will use
  toggleButton <- mkToggleButton
  -- create the parent component
  component "Container" \_ ->
    pure
      $ R.div
          { children:
              [ element toggleButton { label: "A" }
              , element toggleButton { label: "B" }
              ]
          }
