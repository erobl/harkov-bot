module Write where

import Database.Redis
import Data.List.Split
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

markovWrite :: Connection -> String -> IO ()
markovWrite conn s = do
    output <- runRedis conn $ sequence $ map incr keys
    print output
        where keys = map pack $ ((stringToKeys s) ++ (stringToSumKeys s))
