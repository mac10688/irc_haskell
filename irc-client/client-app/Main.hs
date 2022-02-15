{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
module Main
    ( main
    ) where
--------------------------------------------------------------------------------
import           Control.Concurrent  (forkIO)
import           Control.Monad       (forever, unless)
import           Control.Monad.Trans (liftIO)
import           Network.Socket      (withSocketsDo)
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import qualified Network.WebSockets  as WS
import           Payload
import           Data.Aeson


--------------------------------------------------------------------------------
app :: WS.ClientApp ()
app conn = do
    putStrLn "Connected!"

    let connect = encode $ Connect "Scoot"
    WS.sendTextData conn connect

    -- Fork a thread that writes WS data to stdout
    _ <- forkIO $ forever $ do
        msg <- WS.receiveData conn
        liftIO $ T.putStrLn msg

    -- Read from stdin and write to WS
    let loop = do
            line <- T.getLine
            case line of
                "1" -> let createRoomJson = encode $ CreateRoom "Room1" in
                       WS.sendTextData conn createRoomJson >> loop
                _ -> loop
            -- unless (T.null line) $ WS.sendTextData conn line >> loop

    loop
    WS.sendClose conn ("Bye!" :: Text)


--------------------------------------------------------------------------------
main :: IO ()
main = withSocketsDo $ WS.runClient "127.0.0.1" 9160 "/" app