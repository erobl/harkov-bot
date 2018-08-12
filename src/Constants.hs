{-# LANGUAGE OverloadedStrings          #-}

module Constants (Wrd, Sentence, ChatId, TeleUpdate, startword, endword, sep) where

import           Data.Int                  (Int64)
import           Data.Text.Internal        (Text)

type Wrd = String
type Sentence = [Wrd]
type ChatId = String
type TeleUpdate = (Int, Maybe Text, Int64)

startword = "$secretstartword$"
endword = "$secretendword$"

sep = "~"
