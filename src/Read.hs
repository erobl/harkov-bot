{-# LANGUAGE OverloadedStrings #-}
module Read where


import Data.List.Split
import Database.Redis
import Data.ByteString.Char8 (pack, unpack, ByteString)
import Text.Read (readMaybe)
import Data.Maybe
import System.Random

import Constants

-- code to read
eitherListDefault :: Either a [b] -> [b]
eitherListDefault (Left x) = []
eitherListDefault (Right x) = x

eitherBSDefault :: Either a (Maybe ByteString) -> (Maybe ByteString)
eitherBSDefault (Left x) = Nothing
eitherBSDefault (Right x) = x

-- this is horrible, I might rework later
maybeUnpack :: Maybe ByteString -> Maybe String
maybeUnpack (Nothing) = Nothing
maybeUnpack (Just x) = Just (unpack x)

resultToInt :: Maybe ByteString -> Int
resultToInt x = fromMaybe 0 (readMaybe =<< (maybeUnpack x))


pickNth :: [(Int, a)] -> Int -> Maybe a
pickNth [] _ = Nothing
pickNth [(_,x)] _ = Just x
pickNth ((m,x):xs) n
    | n > m = pickNth xs (n-m)
    | otherwise = Just x

getNext :: String -> String
getNext key = splitOn ":" key !! 1

zipValuesMatches :: Either a [Maybe ByteString] -> Either b [ByteString] -> [(Int, ByteString)]
zipValuesMatches values matches = zip (map resultToInt $ eitherListDefault values) (eitherListDefault matches)

getNextWord :: Connection -> String -> IO (String)
getNextWord conn s = do
    amount <- runRedis conn $ do
        get (pack (s ++ ":0"))
    randomN <- randomRIO (1, resultToInt $ eitherBSDefault amount)
    runRedis conn $ do
        matches <- keys (pack (s ++ ":*:0"))
        values <- mget $ eitherListDefault matches
        return $ processKey $ pickNth (zipValuesMatches values matches) randomN
            where processKey = getNext . unpack . fromMaybe ""
markovRead :: Connection -> String -> IO (String)
markovRead conn word
    | word == endword = return ""
    | otherwise = do
        nextWord <- getNextWord conn word
        restOfSentence <- markovRead conn nextWord
        return (nextWord ++ " " ++ restOfSentence)
