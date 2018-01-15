{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.List.Split
import Database.Redis
import Control.Monad.IO.Class
import Data.ByteString.Char8 (pack, unpack, ByteString)
import Text.Read (readMaybe)
import Data.Maybe
import System.Random

startword = "$secretstartword$"
endword = "$secretendword$"

main :: IO ()
main = do
    conn <- checkedConnect defaultConnectInfo
    output <- iterateWords conn startword
    print output


type Wrd = String
type Sentence = [Wrd]
type ChatId = String

-- code to write
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
                    a <- incr (pack key)                    
                    liftIO $ print a

insertSentence :: Connection -> String -> IO ()
insertSentence conn s = do
    output <- runRedis conn $ sequence $ map incr keys
    print output
        where keys = map pack $ ((stringToKeys s) ++ (stringToSumKeys s))

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
iterateWords :: Connection -> String -> IO (String)
iterateWords conn word
    | word == endword = return ""
    | otherwise = do
        nextWord <- getNextWord conn word
        restOfSentence <- iterateWords conn nextWord
        return (nextWord ++ " " ++ restOfSentence)
