module AffEx where

import Prelude
import Data.Either (either)
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay, error, message, throwError)
import React.Basic.DOM as R
import React.Basic.Events (handler_)
import React.Basic.Hooks (type (/\), ReactComponent, Hook, JSX, component, element, fragment, useState, (/\))
import React.Basic.Hooks as React
import React.Basic.Hooks.Aff (useAff)

mkAffEx :: Effect (ReactComponent {})
mkAffEx = do
  -- A component for fetching and rendering a Cat entity.
  catDetails <- mkCatDetails
  component "AffEx" \props -> React.do
    catKey /\ catChooser <- useCatKeyChooser
    pure
      $ R.div
          { style: R.css { display: "flex", flexFlow: "column" }
          , children:
            [ R.h2_ [ R.text "Cat chooser" ]
            , R.p_
                [ R.text
                    $ "Select a key to fetch! If you get bored (how would you even!?) "
                    <> "try holding your arrow keys to select really fast! The result "
                    <> "always matches the chosen key."
                ]
            , catChooser
            , R.p_
                [ case catKey of
                    Nothing -> mempty
                    Just k -> element catDetails { catKey: k }
                ]
            ]
          }
  where
  -- This hook packages up some interactive UI and the current
  -- selection the user has made via that UI.
  useCatKeyChooser :: Hook _ ((Maybe (Key Cat)) /\ JSX)
  useCatKeyChooser = React.do
    catKey /\ setCatKey <- useState Nothing
    let
      catChoice key =
        R.label_
          [ R.input
              { type: "radio"
              , name: "cat-key"
              , checked: Just key == catKey
              , onChange:
                handler_ do
                  setCatKey \_ -> Just key
              }
          , R.text $ showCatKey key
          ]

      showCatKey :: Key Cat -> String
      showCatKey (Key key) = "Cat " <> key
    pure $ catKey
      /\ fragment
          [ catChoice $ Key "abc"
          , catChoice $ Key "def"
          , catChoice $ Key "ghi"
          , catChoice $ Key "xyz"
          ]

  -- Hooks can't be used conditionally but components can!
  -- Not needing to deal with a `Maybe` key simplifies this
  -- compoennt a bit.
  mkCatDetails :: Effect (ReactComponent { catKey :: Key Cat })
  mkCatDetails = do
    component "CatDetails" \{ catKey } -> React.do
      cat <- useAff catKey $ fetch catKey
      pure $ R.text
        $ maybe "Loading..." (either message showCat) cat
    where
    showCat (Cat { name }) = "A cat named " <> name

-- Typed keys are a great way to tie entity-specific behavior
-- to an ID. We can use this phantom type to write a class
-- for generic, type-safe data fetching.
newtype Key entity
  = Key String

derive instance eqKey :: Eq (Key entity)

class Fetch entity where
  fetch :: Key entity -> Aff entity

-- An example entity
newtype Cat
  = Cat { name :: String }

instance fetchCat :: Fetch Cat where
  fetch = case _ of
    Key "abc" -> do
      delay $ Milliseconds 300.0
      pure $ Cat { name: "Herb" }
    Key "def" -> do
      delay $ Milliseconds 600.0
      pure $ Cat { name: "Maxi" }
    Key "ghi" -> do
      delay $ Milliseconds 900.0
      pure $ Cat { name: "Chloe" }
    _ -> do
      delay $ Milliseconds 900.0
      throwError $ error "Cat not found (intended example behavior 😅)"
