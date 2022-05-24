module React.Basic.Hooks.Suspense
  ( suspend
  , Suspended(..)
  , SuspenseResult(..)
  , suspense
  ) where

import Prelude
import Control.Promise (Promise)
import Control.Promise as Promise
import Effect (Effect)
import Effect.Aff (Error, Fiber, joinFiber, throwError)
import React.Basic.Hooks (JSX, Pure, ReactComponent, element, unsafeRenderEffect)
import Unsafe.Coerce (unsafeCoerce)

--| Suspend rendering until a result exists.
--|
--| *Note: Error and loading states are thrown to React! Don't forget
--|   to implement a React error boundary and ensure `suspend` is
--|   only called from a child of at least one `suspense` parent!*
--|
--| *Note: You probably shouldn't be using this function directly. It's
--|   primarily for library authors to build abstractions on top of, as
--|   it requires things like caching mechanisms external to the
--|   component tree.*
--|
--| *Warning: React's Suspense API is still experimental. It requires
--|   some manual setup as well as specific versions of React. The API
--|   is also not final and these functions may change.*
suspend :: forall a. Suspended a -> Pure a
suspend (Suspended e) = React.do
  unsafeRenderEffect do
    result <- e
    case result of
      InProgress fiber ->
        unsafeThrowPromise
          =<< Promise.fromAff (joinFiber fiber)
      Failed err -> throwError err
      Complete a -> pure a

newtype Suspended a
  = Suspended (Effect (SuspenseResult a))

data SuspenseResult a
  = InProgress (Fiber a)
  | Failed Error
  | Complete a

suspense :: { fallback :: JSX, children :: Array JSX } -> JSX
suspense = element suspense_

foreign import suspense_ :: ReactComponent { children :: Array JSX, fallback :: JSX }

--| Dangerously throw a `Promise` as though it were an `Error`.
--| React's Suspense API catches thrown `Promise`s and suspends
--| rendering until they complete.
unsafeThrowPromise :: forall a. Promise a -> Effect a
unsafeThrowPromise = throwError <<< (unsafeCoerce :: Promise a -> Error)
