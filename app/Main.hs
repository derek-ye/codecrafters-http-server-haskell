{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forever)
import qualified Data.ByteString.Char8 as BC
import Network.Socket
import System.IO (BufferMode (..), hSetBuffering, stdout)
import Network.Socket.ByteString (send, recv)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering

    -- You can use print statements as follows for debugging, they'll be visible when running tests.
    BC.putStrLn "Logs from your program will appear here"

    -- Uncomment this block to pass first stage
    let host = "127.0.0.1"
    let port = "4221"

    BC.putStrLn $ "Listening on " <> BC.pack host <> ":" <> BC.pack port

    -- Get address information for the given host and port
    addrInfo <- getAddrInfo Nothing (Just host) (Just port)

    serverSocket <- socket (addrFamily $ head addrInfo) Stream defaultProtocol
    bind serverSocket $ addrAddress $ head addrInfo
    listen serverSocket 5

    -- Accept connections and handle them forever
    forever $ do
        (clientSocket, clientAddr) <- accept serverSocket
        print clientSocket
        BC.putStrLn $ "Accepted connection from " <> BC.pack (show clientAddr) <> "."
        -- Handle the clientSocket as needed...
        path <- parseRequest <$> recv clientSocket 4096  -- max # of bytes that it's possible to read
        print path

        _ <- send clientSocket $ BC.pack $ getServerResponse path
        close clientSocket


-- most basic parsing:
-- break up a request by line number
-- take the first line, break it up by whitespace -> array
-- take second index of array from previous line
parseRequest :: BC.ByteString -> String
parseRequest unparsedReq = path
    where
        reqStr = BC.unpack unparsedReq
        reqArr = lines reqStr
        firstLineOfReq = head reqArr
        path = words firstLineOfReq !! 1

getServerResponse :: String -> String
getServerResponse path = case path of
    "/index.html" -> "HTTP/1.1 200 OK\r\n\r\n"
    _ -> "HTTP/1.1 404 Not Found\r\n\r\n"
