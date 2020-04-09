module Example where

import Prelude
import Data.Foldable (find)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, un)
import Effect.Aff (Aff, Milliseconds(..), delay, error, message, throwError)
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture_)
import React.Basic.Events (handler_)
import React.Basic.Hooks (Component, component, fragment, useState, (/\))
import React.Basic.Hooks as React
import React.Basic.Hooks.Aff (useAff)
import React.Basic.Hooks.ErrorBoundary (mkErrorBoundary)

mkExample :: Component Unit
mkExample = do
  errorBoundary <- mkErrorBoundary "AffExErrorBoundary"
  catDetails <- mkCatDetails
  component "AffEx" \props -> React.do
    catKey /\ setCatKey <- useState Nothing
    let
      reset = setCatKey \_ -> Nothing
    pure
      $ R.div_
          [ R.h2_ [ R.text "Cat chooser" ]
          , errorBoundary \{ error, dismissError } -> case error of
              Just e -> renderAppError e (reset *> dismissError)
              Nothing ->
                fragment
                  [ catKeyList catKey setCatKey
                  , case catKey of
                      Nothing -> mempty
                      Just k -> catDetails k
                  ]
          ]
  where
  -- This component is the main `useAff` demo. It receives a key
  -- as a prop and renders both the loading state and the final
  -- result.
  mkCatDetails :: Component (Key Cat)
  mkCatDetails = do
    component "CatDetails" \catKey -> React.do
      catState <- useAff catKey $ fetch catKey
      pure
        $ R.p_
            [ case map entity catState of
                Nothing -> R.text "Loading..."
                Just (Cat { name }) -> R.text $ "A cat named " <> name
            ]

  renderAppError error resetApp =
    fragment
      [ R.p_ [ R.text "Error!" ]
      , R.p_ [ R.text $ message error ]
      , R.button
          { onClick: capture_ do resetApp
          , children: [ R.text "Reset" ]
          }
      ]

  catKeyList selectedCatKey setCatKey =
    let
      cats =
        fakeDb
          <> [ Entity
                (Key "error (choose to throw a React render error)")
                (Cat { name: "" })
            ]

      catKeyRadioButton k =
        R.div_
          [ R.label_
              [ R.input
                  { type: "radio"
                  , name: "cat-key"
                  , checked: Just k == selectedCatKey
                  , onChange:
                      handler_ do
                        setCatKey \_ -> Just k
                  }
              , R.text $ " Cat " <> un Key k
              ]
          ]
    in
      fragment $ map (catKeyRadioButton <<< key) cats

--
-- The bits below this point aren't directly relevant to the example,
-- just a slightly more interesting data model than returing a single
-- string.
--
--
--
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
