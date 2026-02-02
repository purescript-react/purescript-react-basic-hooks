module React.Basic.Hooks.ViewTransition
  ( viewTransition
  , ViewTransitionProps
  , AnimationValue(..)
  ) where

import Prelude
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toNullable)
import Effect (Effect)
import Foreign.Object (Object)
import React.Basic.Hooks (JSX, ReactComponent, element)
import Web.DOM (Element)

-- | Animation value can be a CSS class name, "none" to disable, or a map of
-- | transition types to class names
data AnimationValue
  = ClassName String
  | AnimationMap (Object String)

-- | Props for the ViewTransition component
type ViewTransitionProps =
  { children :: Array JSX
  , enter :: Maybe AnimationValue
  , exit :: Maybe AnimationValue
  , update :: Maybe AnimationValue
  , share :: Maybe AnimationValue
  , default :: Maybe AnimationValue
  , name :: Maybe String
  , onEnter :: Maybe (Element -> Array String -> Effect Unit)
  , onExit :: Maybe (Element -> Array String -> Effect Unit)
  , onUpdate :: Maybe (Element -> Array String -> Effect Unit)
  , onShare :: Maybe (Element -> Array String -> Effect Unit)
  }

-- | Internal props used for FFI
type ViewTransitionProps_ =
  { children :: Array JSX
  , enter :: Nullable ViewTransitionAnimationValue_
  , exit :: Nullable ViewTransitionAnimationValue_
  , update :: Nullable ViewTransitionAnimationValue_
  , share :: Nullable ViewTransitionAnimationValue_
  , default :: Nullable ViewTransitionAnimationValue_
  , name :: Nullable String
  , onEnter :: Nullable ViewTransitionCallback_
  , onExit :: Nullable ViewTransitionCallback_
  , onUpdate :: Nullable ViewTransitionCallback_
  , onShare :: Nullable ViewTransitionCallback_
  }

-- | The ViewTransition component animates DOM elements when they update
-- | inside a Transition.
-- |
-- | It uses the browser's View Transition API to create smooth animations
-- | for element enter/exit, updates, and shared element transitions.
-- |
-- | Example:
-- | ```purescript
-- | viewTransition
-- |   { children: [ myContent ]
-- |   , enter: Just (ClassName "slide-in")
-- |   , exit: Just (ClassName "slide-out")
-- |   , name: Just "my-element"
-- |   , default: Nothing
-- |   , update: Nothing
-- |   , share: Nothing
-- |   , onEnter: Nothing
-- |   , onExit: Nothing
-- |   , onUpdate: Nothing
-- |   , onShare: Nothing
-- |   }
-- | ```
-- |
-- | Note: ViewTransition only activates inside a Transition (via startTransition)
-- | and is available in React 19+ experimental/canary channels
viewTransition :: ViewTransitionProps -> JSX
viewTransition props = element viewTransition_
  { children: props.children
  , enter: toNullable $ toAnimationValue_ <$> props.enter
  , exit: toNullable $ toAnimationValue_ <$> props.exit
  , update: toNullable $ toAnimationValue_ <$> props.update
  , share: toNullable $ toAnimationValue_ <$> props.share
  , default: toNullable $ toAnimationValue_ <$> props.default
  , name: toNullable props.name
  , onEnter: toNullable $ toCallback_ <$> props.onEnter
  , onExit: toNullable $ toCallback_ <$> props.onExit
  , onUpdate: toNullable $ toCallback_ <$> props.onUpdate
  , onShare: toNullable $ toCallback_ <$> props.onShare
  }

toAnimationValue_ :: AnimationValue -> ViewTransitionAnimationValue_
toAnimationValue_ (ClassName str) = mkClassName str
toAnimationValue_ (AnimationMap obj) = mkAnimationMap obj

foreign import data ViewTransitionAnimationValue_ :: Type
foreign import data ViewTransitionCallback_ :: Type

foreign import mkClassName :: String -> ViewTransitionAnimationValue_
foreign import mkAnimationMap :: Object String -> ViewTransitionAnimationValue_
foreign import toCallback_ :: (Element -> Array String -> Effect Unit) -> ViewTransitionCallback_

foreign import viewTransition_ :: ReactComponent ViewTransitionProps_
