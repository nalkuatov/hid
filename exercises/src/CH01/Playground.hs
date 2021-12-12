module CH01.Playground where

import Data.Text
import Fmt

sample :: Text
sample = fmt $
  "Hi! I'm " +|(12 + 3::Integer)|+ " years old!"
