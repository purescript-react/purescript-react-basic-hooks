module Container where

import Prelude
import Effect (Effect)
import React.Basic.DOM as R
import React.Basic.Hooks (ReactComponent, component, element)
import ToggleButton (mkToggleButton)

mkToggleButtonContainer :: Effect (ReactComponent {})
mkToggleButtonContainer = do
  toggleButton <- mkToggleButton
  component "Container" \_ ->
    pure
      $ R.div
          { children:
            [ element toggleButton { label: "A" }
            , element toggleButton { label: "B" }
            ]
          }
