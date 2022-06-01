{-# language RecordWildCards #-}
{-# language DerivingStrategies #-}

module CH07.Parser
  ( reqP

  -- * re-exports
  , runParser
  )
  where

import Data.Time
import Control.Monad (replicateM, void)
import Data.Text
import Data.Proxy
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

data Req = Req { date :: Maybe Day
               , address :: Text
               } deriving stock Show

dateP :: Parser Day
dateP = do
  a <- read <$> replicateM 4 digitChar
  void $ char '-'
  b <- read <$> replicateM 2 digitChar
  void $ char '-'
  c <- read <$> replicateM 2 digitChar
  pure $ fromGregorian a b c

reqP :: Parser Req
reqP = do
  date <- optional $ try $ dateP
  void (optional $ char '@')
  address <- pack <$> many printChar
  pure Req{..}
