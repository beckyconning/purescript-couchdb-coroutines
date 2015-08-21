module Test.Process where

import Prelude
import Control.Monad.Eff

foreign import data EXIT :: !

foreign import exit :: forall eff. Int -> Eff (exit :: EXIT | eff) Unit
