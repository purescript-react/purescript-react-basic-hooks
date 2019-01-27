module Counter where

import Prelude

import Effect (Effect)
import React.Basic.DOM as R
import React.Basic.Events (handler_)
import React.Basic.Hooks (CreateComponent, component, useEffect, useState, (/\))
import React.Basic.Hooks as React

mkCounter :: CreateComponent {}
mkCounter = do
  component "Counter" \props -> React.do
    counter /\ setCounter <- useState 0

    useEffect counter do
      setDocumentTitle $ "Count: " <> show counter
      pure mempty

    React.pure $ R.button
      { onClick: handler_ $ setCounter (_ + 1)
      , children: [ R.text $ "Increment: " <> show counter ]
      }

foreign import setDocumentTitle :: String -> Effect Unit
