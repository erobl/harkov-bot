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
getNext key  
    | length keys < 3 = endword
    | otherwise = keys !! 1
    where keys = splitOn sep key

zipValuesMatches :: Either a [Maybe ByteString] -> Either b [ByteString] -> [(Int, ByteString)]
zipValuesMatches values matches = zip (map resultToInt $ eitherListDefault values) (eitherListDefault matches)

getNextWord :: Connection -> String -> String -> IO (String)
getNextWord conn s c = do
    amount <- runRedis conn $ do
        get (pack (s ++ sep ++ c))
    randomN <- randomRIO (1, resultToInt $ eitherBSDefault amount)
    runRedis conn $ do
        matches <- keys (pack (s ++ sep ++ "*" ++ sep ++ c))
        values <- mget $ eitherListDefault matches
        return $ processKey $ pickNth (zipValuesMatches values matches) randomN
            where processKey = getNext . unpack . fromMaybe ""

markovRead :: Connection -> String -> String -> IO (String)
markovRead conn word chat_id
    | word == endword = return ""
    | otherwise = do
        nextWord <- getNextWord conn word chat_id
        keepReading conn chat_id nextWord

keepReading :: Connection -> String -> String -> IO (String)
keepReading conn chat_id nextWord
    | nextWord == endword = return ""
    | otherwise = do
        restOfSentence <- markovRead conn nextWord chat_id
        return (nextWord ++ " " ++ restOfSentence)
