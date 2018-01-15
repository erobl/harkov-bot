module Main where

import Data.List.Split

main :: IO ()
main = do
    s <- getLine
    print $ makeTriplet (segment s) "0"

type Wrd = String
type Sentence = [Wrd]
type ChatId = String

segment :: String -> Sentence
segment = splitOn " "

reverseTail = reverse . tail . reverse

makeTriplet :: Sentence -> ChatId -> [(Wrd, Wrd, ChatId)]
makeTriplet s c = zip3 (reverseTail s) (tail s) (repeat c)
