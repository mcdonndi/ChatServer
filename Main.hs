module Main where

import Network.Socket
import System.IO
import Control.Exception
import Control.Concurrent
import Control.Monad (when)
import Control.Monad.Fix (fix)
--import Lines

main :: IO ()
main = do
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet 4242 iNADDR_ANY)
    listen sock 2
    chan <- newChan
    _ <- forkIO $ fix $ \loop -> do
        (_,_) <- readChan chan
        loop
    mainLoop sock chan 0

type Msg = (Int, String)

mainLoop :: Socket -> Chan Msg -> Int -> IO ()
mainLoop sock chan msgNum = do
    conn <- accept sock
    forkIO (runConn conn chan msgNum)
    mainLoop sock chan $! msgNum + 1

runConn :: (Socket, SockAddr) -> Chan Msg -> Int -> IO ()
runConn (sock, _) chan msgNum = do
    let broadcast msg = writeChan chan (msgNum, msg)
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering

    hPutStrLn hdl "Listening..."
    join_chatroom <- fmap init (hGetLine hdl)
    client_ip <- fmap init (hGetLine hdl)
    port_ <- fmap init (hGetLine hdl)
    client_name <- fmap init (hGetLine hdl)

    let joinChatroom = dropWhile (/=' ') join_chatroom
    let clientIP = dropWhile (/=' ') client_ip
    let port = dropWhile (/=' ') port_
    let clientName = dropWhile (/=' ') client_name

    let chatrooms = addToList joinChatroom chatrooms
    let clients = addToList clientName clients

    hPutStrLn hdl ("JOINED_CHATROOM: " ++ joinChatroom)
    hPutStrLn hdl ("SERVER_IP: [IP address of chat room]")
    hPutStrLn hdl ("PORT: [port number of chat room]")
    hPutStrLn hdl ("ROOM_REF: 1")
    hPutStrLn hdl ("JOIN_ID: 99")
    broadcast ("--> " ++ clientName ++ " entered chat.")

    commLine <- dupChan chan

    -- fork off a thread for reading from the duplicated channel
    reader <- forkIO $ fix $ \loop -> do
        (nextNum, line) <- readChan commLine
        when (msgNum /= nextNum) $ hPutStrLn hdl line
        loop

    -- read lines from the socket and echo them back to the user
    handle (\(SomeException _) -> return()) $ fix $ \loop -> do
        chatroom_ <- fmap init (hGetLine hdl)
        join_id <- fmap init (hGetLine hdl)
        client_name <- fmap init (hGetLine hdl)
        message_ <- fmap init (hGetLine hdl)
        let chatroomKey = takeWhile (/=':') chatroom_
        let chatroom = dropWhile (/=' ') chatroom_
        let clientName = dropWhile (/=' ') client_name
        let message = dropWhile (/=' ') message_
        case chatroomKey of
            -- if an exception is caught, send a message and break the loop
            "LEAVE_CHATROOM"  -> hPutStrLn hdl ("DISCONNECT: 0\nPORT: 0\nCLIENT_NAME:" ++ clientName)
            -- else continue looping
            _       -> broadcast ("CHAT:" ++ chatroom ++ "\n\nCLIENT_NAME:" ++ clientName ++ "\n\nMESSAGE:" ++ message) >> loop

    killThread reader
    broadcast ("<-- " ++ clientName ++ " left.")
    hClose hdl

checkList :: String -> [(Int,String)] -> Bool
checkList _ [] = False
checkList item (x:xs) = if snd x == item then True else checkList item xs

addToList :: String -> [(Int,String)] -> [(Int,String)]
addToList item [] = [(1, item)]
addToList item xs = if checkList item xs == True then xs else ((length xs) + 1,item):xs