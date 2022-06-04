module Test.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Time.Duration (Seconds(..), fromDuration)
import Effect (Effect)
import Effect.Aff (delay, launchAff_)
import Test.Spec.Discovery (discover)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (defaultConfig, runSpec')

main ∷ Effect Unit
main = launchAff_ do
  specs <- discover "\\.*Spec"
  delay (1.0 # Seconds # fromDuration)
  runSpec'
    config
    [ consoleReporter ]
    specs
  where
    config =
      defaultConfig
        { slow = 5.0 # Seconds # fromDuration
        , timeout = Nothing
        }
