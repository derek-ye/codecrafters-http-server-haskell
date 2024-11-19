{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forever)
import qualified Data.ByteString.Char8 as BC
import Network.Socket
import System.IO (BufferMode (..), hSetBuffering, stdout)
import Network.Socket.ByteString (send, recv)
import Text.Printf

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
        path <- parseRequest <$> recv clientSocket 4096  -- max # of bytes that it's possible to read
        print path
        print $ split path
        _ <- send clientSocket $ BC.pack $ responseToString $ getServerResponse $ MkPath path
        close clientSocket


-- most basic parsing:
-- break up a request by line number
-- take the first line, break it up by whitespace -> array
-- take second index of array from previous line
-- if its a valid request, return Just + the path, otherwise Nothing
parseRequest :: BC.ByteString -> Maybe String
-- parseRequest unparsedReq = path
--     where
--         reqStr = BC.unpack unparsedReq
--         reqArr = lines reqStr
--         firstLineOfReq = fromMaybe [] (safeHead reqArr)
--         path = case words firstLineOfReq of
--             (_: x: _) -> Just x         -- take the second value
--             _ -> Nothing                -- all other cases go to the root
parseRequest unparsedReq = do
    firstLineOfReq <- safeHead $ lines $ BC.unpack unparsedReq      -- what happens 
    case words firstLineOfReq of 
        (_: path: _) -> Just path
        _ -> Nothing

data HttpRequestHeaders = HttpRequestHeaders {
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
    version :: String,
    target :: String,
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

getServerResponse :: Path -> HttpResponse
getServerResponse = validPaths

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

validPaths :: Path -> HttpResponse
validPaths (MkPath s)
    | (length pathArr == 2) && (head pathArr == "") && (pathArr !! 1 == "") = response200 "" 0
    | (length pathArr == 2) && (pathArr !! 1 == "index.html") = response200 "" 0
    | (length pathArr == 3) && (pathArr !! 1 == "echo") = echoHandler pathArr
    | otherwise = response404
    where
        pathArr = split s
        
echoHandler :: [String] -> HttpResponse
echoHandler pathArr = response200 strToEcho (length strToEcho)
    where
        strToEcho = pathArr !! 2    -- given the path /echo/abc we want to echo 'abc'

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