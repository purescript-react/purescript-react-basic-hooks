module Base64 where

import Prelude
import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Data.Array (singleton)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Effect (Effect)
import Effect.Aff (Aff)
import React.Basic.DOM as R
import React.Basic.DOM.Events (preventDefault, stopPropagation, targetValue)
import React.Basic.Events (handler)
import React.Basic.Hooks (ReactComponent, component, element, useState, (/\))
import React.Basic.Hooks as React
import React.Basic.Hooks.Aff (useAffReducer)

data Action
  = Decode String
  | ReceiveDecoded String

type State
  = { encoded :: String
    , decoded :: String
    }

reducer :: State -> Action -> { state :: State, effects :: Array (Aff (Array Action)) }
reducer state (ReceiveDecoded content) = { state: state { decoded = content }, effects: mempty }

reducer state (Decode content) = { state: state { encoded = content }, effects: singleton effect }
  where
  effect = do
    maybeResponse <- AX.get ResponseFormat.string ("http://localhost:9000/base64/" <> content) <#> lmap (AX.printError)
    pure $ singleton
      $ case maybeResponse of
          Left err -> ReceiveDecoded ("Failed to decode \"" <> content <> "\"")
          Right response -> ReceiveDecoded response.body

mkUI :: Effect (ReactComponent {})
mkUI = do
  let
    initialState = { encoded: "", decoded: "" }
  inputComponent <- mkInput
  outputComponent <- mkOutput
  component "Main" \props -> React.do
    state /\ dispatch <- useAffReducer initialState reducer
    pure
      $ R.div
          { children:
              [ element inputComponent { dispatch }
              , element outputComponent state
              ]
          }

mkInput :: Effect (ReactComponent { dispatch :: Action -> Effect Unit })
mkInput = do
  component "Base64Input" \props -> React.do
    input /\ setInput <- useState "UHVyZXNjcmlwdCDinaTvuI8gUmVhY3QgaG9va3MK"
    let
      sendInput =
        handler (preventDefault >>> stopPropagation) \_ -> do
          props.dispatch $ Decode input
          setInput $ const ""

      updateInput =
        handler (preventDefault >>> stopPropagation >>> targetValue)
          $ traverse_ (setInput <<< const)
    pure
      $ R.form
          { onSubmit: sendInput
          , children:
              [ R.text "Enter base64 value:"
              , R.input
                  { value: input
                  , onChange: updateInput
                  }
              ]
          }

mkOutput :: Effect (ReactComponent State)
mkOutput =
  component "Base64Output" \props -> React.do
    pure
      $ R.div
          { children:
              [ R.div
                  { children:
                      [ R.text "Base64 encoded:"
                      , R.text props.encoded
                      ]
                  }
              , R.div 
                  { children:
                      [ R.text "Base64 decoded:"
                      , R.text props.decoded
                      ]
                  }
              ]
          }
