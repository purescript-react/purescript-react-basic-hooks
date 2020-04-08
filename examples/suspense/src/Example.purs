module Example where

import Prelude
import Data.Foldable (find)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, un)
import Data.Time.Duration (Seconds(..), fromDuration)
import Effect (Effect)
import Effect.Aff (Aff, Milliseconds(..), delay, error, message, throwError)
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture_)
import React.Basic.Events (handler_)
import React.Basic.Hooks (JSX, ReactComponent, component, element, fragment, useState, (/\))
import React.Basic.Hooks as React
import React.Basic.Hooks.ErrorBoundary (mkErrorBoundary)
import React.Basic.Hooks.Suspense (suspend, suspense)
import React.Basic.Hooks.Suspense.Store (SuspenseStore, get, mkSuspenseStore)

mkExample :: Effect (ReactComponent {})
mkExample = do
  errorBoundary <- mkErrorBoundary "SuspenseExErrorBoundary"
  catDetails <- mkCatDetails
  component "SuspenseEx" \props -> React.do
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
                      Just k ->
                        R.p_
                          [ suspense
                              { fallback: R.text "Loading..."
                              , children: [ catDetails { catKey: k } ]
                              }
                          ]
                  ]
          ]
  where
  -- This component is the main `suspense` demo (but don't forget the `suspense`
  -- element above!). It receives a key as a prop and renders the result as though
  -- it were synchronously available.
  mkCatDetails :: Effect ({ catKey :: Key Cat } -> JSX)
  mkCatDetails = do
    catStore :: SuspenseStore (Key Cat) _ <-
      mkSuspenseStore (Just $ fromDuration $ Seconds 10.0) fetch
    element
      <$> component "CatDetails" \{ catKey } -> React.do
          cat <- suspend $ get catStore catKey
          pure
            $ R.p_
                [ case entity cat of
                    Cat { name } -> R.text $ "A cat named " <> name
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

derive instance ordKey :: Ord (Key entity)

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
