module Write where

import Database.Redis
import Data.List.Split
import Data.List (isPrefixOf)
import Data.ByteString.Char8 (pack, unpack, ByteString)
import Control.Monad.IO.Class

import Constants

-- code to write
segment :: String -> Sentence
segment s = startword : (splitOn " " s) ++ [endword]

reverseTail = reverse . tail . reverse

makeTriplet :: Sentence -> ChatId -> [(Wrd, Wrd, ChatId)]
makeTriplet s c = zip3 (reverseTail s) (tail s) (repeat c)

tripletToKey :: (Wrd, Wrd, ChatId) -> String
tripletToKey (k1, k2, c) = k1 ++ sep ++ k2 ++ sep ++ c

tripletToSumKey :: (Wrd, Wrd, ChatId) -> String
tripletToSumKey (k1, _, c) = k1 ++ sep ++ c

cleanSep :: String -> String
cleanSep = filter (\x -> x /= '~')

cleanAt :: Sentence -> Sentence
cleanAt = filter (\x -> not $ isPrefixOf "@" x)

cleanLink :: Sentence -> Sentence
cleanLink = filter (\x -> not $ isPrefixOf "http" x)

cleanEmpty :: Sentence -> Sentence
cleanEmpty = filter (\x -> x /= "")

cleanSentence :: Sentence -> Sentence
cleanSentence = cleanAt . cleanLink . cleanEmpty

stringToKeys :: String -> String -> [String]
stringToKeys s c = map tripletToKey $ makeTriplet (cleanSentence $ segment (cleanSep s)) c

stringToSumKeys :: String -> String -> [String]
stringToSumKeys s c = map tripletToSumKey $ makeTriplet (segment s) c

keyToRedis :: Connection -> String -> IO ()
keyToRedis conn key = runRedis conn $ do
                    a <- incr (pack key)                    
                    liftIO $ print a

markovWrite :: Connection -> String -> String -> IO ()
markovWrite conn s chat_id = do
    runRedis conn $ sequence_ $ map incr keys
        where keys = map pack $ ((stringToKeys s chat_id) ++ (stringToSumKeys s chat_id))
