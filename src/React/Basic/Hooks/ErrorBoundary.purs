module React.Basic.Hooks.ErrorBoundary
  ( mkErrorBoundary
  ) where

import Prelude
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Effect.Aff (Error)
import React.Basic.Hooks (JSX, ReactComponent, element)

--| Create a React error boundary with the given name. The resulting
--| component takes a render callback which exposes the error if one
--| exists and an effect for dismissing the error.
mkErrorBoundary ::
  String ->
  Effect
    ( ({ error :: Maybe Error, dismissError :: Effect Unit } -> JSX) ->
      JSX
    )
mkErrorBoundary name = do
  c <- errorBoundary_ name
  pure $ element c <<< mapProps
  where
  mapProps render =
    { render:
        \{ error, dismissError } ->
          render { error: toMaybe error, dismissError }
    }

foreign import errorBoundary_ ::
  String ->
  Effect
    ( ReactComponent
        { render ::
            { error :: Nullable Error
            , dismissError :: Effect Unit
            } ->
            JSX
        }
    )
