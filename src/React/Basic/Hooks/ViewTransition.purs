module React.Basic.Hooks.ViewTransition
  ( viewTransition
  , viewTransitionDefaults
  , ViewTransitionProps
  , AnimationValue(..)
  , ViewTransitionInstance
  ) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toNullable)
import Effect (Effect)
import Foreign.Object (Object)
import React.Basic.Hooks (JSX, ReactComponent, element)

data AnimationValue
  = ClassName String
  | AnimationMap (Object String)

foreign import data ViewTransitionInstance :: Type

type ViewTransitionProps =
  { children :: Array JSX
  , enter :: Maybe AnimationValue
  , exit :: Maybe AnimationValue
  , update :: Maybe AnimationValue
  , share :: Maybe AnimationValue
  , layout :: Maybe AnimationValue
  , fallback :: Maybe AnimationValue
  , name :: Maybe String
  , onEnter :: Maybe (ViewTransitionInstance -> Array String -> Effect (Effect Unit))
  , onExit :: Maybe (ViewTransitionInstance -> Array String -> Effect (Effect Unit))
  , onUpdate :: Maybe (ViewTransitionInstance -> Array String -> Effect (Effect Unit))
  , onShare :: Maybe (ViewTransitionInstance -> Array String -> Effect (Effect Unit))
  }

viewTransitionDefaults :: ViewTransitionProps
viewTransitionDefaults =
  { children: []
  , enter: Nothing
  , exit: Nothing
  , update: Nothing
  , share: Nothing
  , layout: Nothing
  , fallback: Nothing
  , name: Nothing
  , onEnter: Nothing
  , onExit: Nothing
  , onUpdate: Nothing
  , onShare: Nothing
  }

viewTransition :: ViewTransitionProps -> JSX
viewTransition props = element viewTransition_ (toProps_ props)
  where
  toProps_ p =
    { children: p.children
    , enter: toNullable $ toAnimationValue_ <$> p.enter
    , exit: toNullable $ toAnimationValue_ <$> p.exit
    , update: toNullable $ toAnimationValue_ <$> p.update
    , share: toNullable $ toAnimationValue_ <$> p.share
    , layout: toNullable $ toAnimationValue_ <$> p.layout
    , default: toNullable $ toAnimationValue_ <$> p.fallback
    , name: toNullable p.name
    , onEnter: toNullable $ toCallback_ <$> p.onEnter
    , onExit: toNullable $ toCallback_ <$> p.onExit
    , onUpdate: toNullable $ toCallback_ <$> p.onUpdate
    , onShare: toNullable $ toCallback_ <$> p.onShare
    }

toAnimationValue_ :: AnimationValue -> ViewTransitionAnimationValue_
toAnimationValue_ (ClassName str) = mkClassName str
toAnimationValue_ (AnimationMap obj) = mkAnimationMap obj

foreign import data ViewTransitionAnimationValue_ :: Type
foreign import data ViewTransitionCallback_ :: Type

foreign import mkClassName :: String -> ViewTransitionAnimationValue_
foreign import mkAnimationMap :: Object String -> ViewTransitionAnimationValue_
foreign import toCallback_ :: (ViewTransitionInstance -> Array String -> Effect (Effect Unit)) -> ViewTransitionCallback_

type ViewTransitionProps_ =
  { children :: Array JSX
  , enter :: Nullable ViewTransitionAnimationValue_
  , exit :: Nullable ViewTransitionAnimationValue_
  , update :: Nullable ViewTransitionAnimationValue_
  , share :: Nullable ViewTransitionAnimationValue_
  , layout :: Nullable ViewTransitionAnimationValue_
  , default :: Nullable ViewTransitionAnimationValue_
  , name :: Nullable String
  , onEnter :: Nullable ViewTransitionCallback_
  , onExit :: Nullable ViewTransitionCallback_
  , onUpdate :: Nullable ViewTransitionCallback_
  , onShare :: Nullable ViewTransitionCallback_
  }

foreign import viewTransition_ :: ReactComponent ViewTransitionProps_
