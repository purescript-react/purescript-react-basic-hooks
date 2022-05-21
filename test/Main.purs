module Test.Main where

import Prelude

import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Newtype (un)
import Data.Time.Duration (Seconds(..), fromDuration)
import Effect (Effect)
import Effect.Aff (delay, launchAff_)
-- import Test.Discovery (discover)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (defaultConfig, runSpecT)

main ∷ Effect Unit
main = do
  -- specs <- discover "\\.*Spec"
  launchAff_ do
    void do
      delay (1.0 # Seconds # fromDuration)
      runSpecT
        config
        [ consoleReporter ]
        (pure unit)
        # un Identity
  where
    config =
      defaultConfig
        { slow = 5.0 # Seconds # fromDuration
        , timeout = Nothing
        }
