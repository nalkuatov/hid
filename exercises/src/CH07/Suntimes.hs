module CH07.Suntimes where

import Control.Monad.Reader
import Servant.Client

import CH07.Parser

type SuntimesM
  = ClientM

main :: IO ()
main = print "hello"
