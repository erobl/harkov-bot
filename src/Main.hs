{-# LANGUAGE OverloadedStrings          #-}
module Main where

import Database.Redis (Connection, checkedConnect, defaultConnectInfo)

import Write
import Constants
import Read

import           Network.HTTP.Client      (newManager, Manager)
import           Network.HTTP.Client.TLS  (tlsManagerSettings)
import           Web.Telegram.API.Bot
import           Data.Text.Internal        (Text)
import           Data.Int                  (Int64)
import           Data.Text                 (unpack, pack)

token = Token "secretsecret"

main :: IO ()
main = do
  conn <- checkedConnect defaultConnectInfo
  manager <- newManager tlsManagerSettings
  pollTelegram 0 manager conn

pollTelegram :: Int -> Manager -> Connection -> IO ()
pollTelegram height manager conn = do
  res <- runTelegramClient token manager $ do
    getUpdatesM $ GetUpdatesRequest (Just height) Nothing (Just 1000) (Just ["message", "channel_post"])
  case res of
    Left e -> do
        pollTelegram 0 manager conn
    Right Response { result = u } -> do
        print $ map cleanUpdates u
        sentences <- writeUpdates conn (map cleanUpdates u)
        runTelegramClient token manager $ sequence_ $ map sendMarkov sentences
        pollTelegram ((maximum $ map (getHeight . cleanUpdates) u) + 1) manager conn

cleanUpdates :: Update -> (Int, Maybe Text, Int64)
cleanUpdates (Update { update_id = id, message = u }) = (id, t, c)
  where (t, c) = cleanMessage u

cleanMessage :: Maybe Message -> (Maybe Text, Int64)
cleanMessage Nothing = (Nothing, -1)
cleanMessage (Just (Message { text = t, chat = Chat { chat_id = c } })) = (t, c)

getHeight :: (Int, Maybe Text, Int64) -> Int
getHeight (h, _, _) = h

writeUpdate :: Connection -> (Int, Maybe Text, Int64) -> IO (Maybe (Text, Int64))
writeUpdate _ (_, Nothing, _) = return Nothing
writeUpdate conn (_, Just s, c)  
    | s == "/markov" = do
        sentence <- markovRead conn startword (show c)
        return $ Just (pack $ dropLastChar sentence, c)
    | otherwise = do 
        markovWrite conn (unpack s) (show c)
        return Nothing

writeUpdates :: Connection -> [(Int, Maybe Text, Int64)] -> IO [Maybe (Text, Int64)]
writeUpdates conn u = sequence $ map (writeUpdate conn) u

dropLastChar :: String -> String
dropLastChar = reverse . tail . reverse

sendMarkov :: (Maybe (Text, Int64)) -> TelegramClient ()
sendMarkov Nothing = return ()
sendMarkov (Just (t, c)) = do
    sendMessageM $ SendMessageRequest (ChatId c) t Nothing Nothing Nothing Nothing Nothing
    return ()

