module Example where

import Prelude
import Data.Foldable (find)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, un)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay, error, throwError)
import React.Basic.DOM as R
import React.Basic.Events (handler_)
import React.Basic.Hooks (type (/\), ReactComponent, Hook, JSX, component, element, fragment, useState, (/\))
import React.Basic.Hooks as React
import React.Basic.Hooks.Aff (useAff)

mkExample :: Effect (ReactComponent {})
mkExample = do
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
      catChoice k =
        R.label_
          [ R.input
              { type: "radio"
              , name: "cat-key"
              , checked: Just k == catKey
              , onChange:
                  handler_ do
                    setCatKey \_ -> Just k
              }
          , R.text $ " " <> showCatKey k
          ]

      showCatKey :: Key Cat -> String
      showCatKey (Key k) = "Cat " <> k
    pure $ catKey /\ fragment (map (catChoice <<< key) fakeDb)

  -- Hooks can't be used conditionally but components can!
  -- Not needing to deal with a `Maybe` key simplifies this
  -- compoennt a bit.
  mkCatDetails :: Effect (ReactComponent { catKey :: Key Cat })
  mkCatDetails = do
    component "CatDetails" \{ catKey } -> React.do
      catState <- useAff catKey $ fetch catKey
      pure case map entity catState of
        Nothing -> R.text "Loading..."
        Just (Cat { name }) -> R.text $ "A cat named " <> name

-- Typed keys are a great way to tie entity-specific behavior
-- to an ID. We can use this phantom type to write a class
-- for generic, type-safe data fetching.
newtype Key entity
  = Key String

derive instance eqKey :: Eq (Key entity)

derive instance ntKey :: Newtype (Key entity) _

-- An entity wrapper. In a real app this would hold other metadata
-- such as create and update dates.
data Entity entity
  = Entity (Key entity) entity

key :: forall entity. Entity entity -> Key entity
key (Entity k _) = k

entity :: forall entity. Entity entity -> entity
entity (Entity _ e) = e

class Fetch entity where
  fetch :: Key entity -> Aff (Entity entity)

-- An example entity
newtype Cat
  = Cat { name :: String }

fakeDb :: Array (Entity Cat)
fakeDb =
  [ Entity (Key "abc") (Cat { name: "Herb" })
  , Entity (Key "def") (Cat { name: "Maxi" })
  , Entity (Key "ghi") (Cat { name: "Chloe" })
  ]

instance fetchCat :: Fetch Cat where
  fetch k = do
    delay $ Milliseconds 300.0
    -- pretend this happens on the server
    case fakeDb # find (key >>> (_ == k)) of
      Nothing ->
        -- This should never happen in a normal application path
        -- if only the server can generate keys :)
        throwError
          $ error
          $ "DB error: Cat not found for key "
          <> un Key k
      Just e -> pure e
