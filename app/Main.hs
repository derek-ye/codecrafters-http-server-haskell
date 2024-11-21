{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forever)
import qualified Data.ByteString.Char8 as BC
import Network.Socket
import System.IO (BufferMode (..), hSetBuffering, stdout)
import Network.Socket.ByteString (send, recv)
import Text.Printf
import Debug.Trace

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
    setSocketOption serverSocket ReuseAddr 1
    bind serverSocket $ addrAddress $ head addrInfo
    listen serverSocket 5

    -- Accept connections and handle them forever
    forever $ do
        (clientSocket, clientAddr) <- accept serverSocket
        print clientSocket
        BC.putStrLn $ "Accepted connection from " <> BC.pack (show clientAddr) <> "."
        -- Handle the clientSocket as needed...
        Just parsedReq <- parseRequest <$> recv clientSocket 4096  -- max # of bytes that it's possible to read
        
        _ <- send clientSocket $ BC.pack $ responseToString $ getServerResponse parsedReq
        close clientSocket


parseRequest :: BC.ByteString -> Maybe HttpRequest
parseRequest unparsedReq = do
    (m, t, v) <- maybeParsedRequestLine
    parsedHeader <- unsafeParsedHeader
    
    parsedBody <- unsafeParsedBody
    -- _ <- trace (show parsedBody) (Just ())
   
    pure MkHttpRequest {
            method=m,
            version=v,
            targetPath=t,
            reqheaders=parsedHeader,
            requestBody=parsedBody
        }
    where
        parseRequestLine :: String -> Maybe (String, String, String)
        parseRequestLine reqLine = case words reqLine of
            [method', targetPath', version'] -> Just (method', targetPath', version')
            _ -> Nothing

        parseHeaders :: String -> Maybe HttpRequestHeaders
        parseHeaders headerLine = case words headerLine of
            [_, host', _, userAgent', _, acceptedMedia'] -> Just MkHttpRequestHeaders {
                host=host',
                userAgent=userAgent',
                acceptedMedia=acceptedMedia'
            }
            _ -> Nothing
        
        parseBody :: String -> Maybe String
        parseBody bodyLine = Just bodyLine  -- always return the body string, even if it's the empty string
        
        reqArr = lines $ BC.unpack unparsedReq
        maybeParsedRequestLine = case safeHead reqArr of
            Just reqLine -> parseRequestLine reqLine
            _ -> Nothing

        -- i can't import safeIndex because of the CodeCrafters environment, so doing it unsafely here
        unsafeParsedBody = parseBody $ reqArr !! 4       -- TODO: not safe...
        unsafeParsedHeader = parseHeaders $ reqArr !! 1 <> reqArr !! 2 <> reqArr !! 3

data HttpRequestHeaders = MkHttpRequestHeaders {
    host :: String,
    userAgent :: String,
    acceptedMedia :: String
} deriving (Show)

data HttpResponseHeaders = MkHttpResponseHeaders {
    contentType :: String,
    contentLength :: String
} deriving (Show)

data HttpRequest = MkHttpRequest {
    method :: String,
    targetPath :: String,
    version :: String,
    reqheaders :: HttpRequestHeaders,
    requestBody :: String
} deriving (Show)
data HttpResponse = MkHttpResponse { 
    status :: String,
    respHeaders :: HttpResponseHeaders,
    responseBody :: String
    }
    deriving (Show)


newtype Path = MkPath String

getServerResponse :: HttpRequest -> HttpResponse
getServerResponse req
    | userAgentStr /= "" = response200 userAgentStr (length userAgentStr)
    | otherwise = pathHandler (MkPath $ targetPath req)
    where
        userAgentStr = userAgent (reqheaders req)

response200 :: String -> Int -> HttpResponse
response200 s cl = MkHttpResponse { 
    status="HTTP/1.1 200 OK",
    respHeaders=MkHttpResponseHeaders {
        contentType="Content-Type: text/plain",
        contentLength=printf "Content-Length: %d" cl
        },
    responseBody=s
    }

response404 :: HttpResponse
-- response404 = MkHttpResponseRaw "HTTP/1.1 404 Not Found\r\n\r\n"
response404 = MkHttpResponse { 
    status="HTTP/1.1 404 Not Found",
    respHeaders=MkHttpResponseHeaders {
        contentType="",
        contentLength=""
        },
    responseBody=""
    }

pathHandler :: Path -> HttpResponse
pathHandler (MkPath s) = case pathArr of
    ["", ""] -> response200 "" 0
    ["", "index.html"] -> response200 "" 0
    ["", "echo", strToEcho, []] -> echoHandler strToEcho    -- What about "/echo/abc/def" -> ["", "echo", "abc", "def"]
    _ -> response404
    where
        pathArr = split s
        
echoHandler :: String -> HttpResponse
echoHandler strToEcho = response200 strToEcho (length strToEcho)

-- pathToString :: Path -> String
-- pathToString (MkPath p) = intercalate "/" p

responseToString :: HttpResponse -> String
responseToString resp = printf "%s\r\n%s\r\n%s\r\n\r\n%s\r\n" (status resp) (contentType $ respHeaders resp) (contentLength $ respHeaders resp) (responseBody resp)

-- from SO https://stackoverflow.com/questions/46580924/haskell-splitting-a-string-by-delimiter
split :: String -> [String]
split [] = [""]
split (c:cs) | c == '/'  = "" : rest
             | otherwise = (c : head rest) : tail rest
    where rest = split cs

-- https://hoogle.internal.mercury.com/file/nix/store/vjldr9zzxh8i22gzr8v9pzild1v5qv1j-ghc-9.6.3-doc/share/doc/ghc/html/libraries/Cabal-syntax-3.10.1.0/src/Distribution.Utils.Generic.html#safeHead
safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

-- https://hoogle.internal.mercury.com/file/nix/store/vjldr9zzxh8i22gzr8v9pzild1v5qv1j-ghc-9.6.3-doc/share/doc/ghc/html/libraries/ghc-9.6.3/GHC-Data-Strict.html#v:fromMaybe
fromMaybe :: a -> Maybe a -> a
fromMaybe d Nothing = d
fromMaybe _ (Just x) = x