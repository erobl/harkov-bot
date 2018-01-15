{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.List.Split
import Database.Redis
import Control.Monad.IO.Class
import Data.ByteString.Char8 (pack, unpack, ByteString)
import Text.Read (readMaybe)
import Data.Maybe
import System.Random

import Write
import Constants
import Read


main :: IO ()
main = do
    conn <- checkedConnect defaultConnectInfo
    output <- markovRead conn startword
    print output

