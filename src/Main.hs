{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.List.Split
import Database.Redis
import Control.Monad.IO.Class
import qualified Data.ByteString.Char8

startword = "$secretstartword$"
endword = "$secretendword$"

main :: IO ()
main = do
    conn <- checkedConnect defaultConnectInfo
    s <- getLine
    insertSentence conn s
    print s

type Wrd = String
type Sentence = [Wrd]
type ChatId = String

segment :: String -> Sentence
segment s = startword : (splitOn " " s) ++ [endword]

reverseTail = reverse . tail . reverse

makeTriplet :: Sentence -> ChatId -> [(Wrd, Wrd, ChatId)]
makeTriplet s c = zip3 (reverseTail s) (tail s) (repeat c)

tripletToKey :: (Wrd, Wrd, ChatId) -> String
tripletToKey (k1, k2, c) = k1 ++ ":" ++ k2 ++ ":" ++ c

tripletToSumKey :: (Wrd, Wrd, ChatId) -> String
tripletToSumKey (k1, _, c) = k1 ++ ":" ++ c

stringToKeys :: String -> [String]
stringToKeys s = map tripletToKey $ makeTriplet (segment s) "0"

stringToSumKeys :: String -> [String]
stringToSumKeys s = map tripletToSumKey $ makeTriplet (segment s) "0"

keyToRedis :: Connection -> String -> IO ()
keyToRedis conn key = runRedis conn $ do
                    a <- incr (Data.ByteString.Char8.pack key)                    
                    liftIO $ print a

insertSentence :: Connection -> String -> IO ()
insertSentence conn s = do
    output <- runRedis conn $ sequence $ map incr keys
    print output
        where keys = map Data.ByteString.Char8.pack $ ((stringToKeys s) ++ (stringToSumKeys s))
