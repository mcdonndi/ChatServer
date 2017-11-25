module Main where

import Network.Socket
import System.Environment
import System.IO
import Control.Exception
import Control.Concurrent
import Control.Monad (when)
import Control.Monad.Fix (fix)
import Text.Printf

main :: IO ()
main = do
    args <- getArgs
    let serverPort = fromIntegral (read $ head args :: Int)
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bind sock (SockAddrInet serverPort iNADDR_ANY)
    listen sock 2
    chan <- newChan
    _ <- forkIO $ fix $ \loop -> do
        (_,_) <- readChan chan
        loop
    mainLoop sock serverPort chan 0

type Msg = (Int, String)

mainLoop :: Socket -> PortNumber -> Chan Msg -> Int -> IO ()
mainLoop sock serverPort chan msgNum  = do
    conn <- accept sock
    forkIO (runConn conn chan msgNum sock serverPort)
    mainLoop sock serverPort chan $! msgNum + 1

runConn :: (Socket, SockAddr) -> Chan Msg -> Int -> Socket -> PortNumber -> IO ()
runConn (sock, sockAddr) chan msgNum mainLoopSock serverPort = do
    let broadcast msg = writeChan chan (msgNum, msg)
    hdl <- socketToHandle sock ReadWriteMode
    let bufferSize = 1024
    let bufferSizeMaybe = Just bufferSize
    hSetBuffering hdl (BlockBuffering bufferSizeMaybe)

    commLine <- dupChan chan

    -- fork off a thread for reading from the duplicated channel
    reader <- forkIO $ fix $ \loop -> do
        (nextNum, line) <- readChan commLine
        when (msgNum /= nextNum) $ sendToClients hdl line
        loop

    -- read lines from the socket and echo them back to the user
    handle (\(SomeException _) -> return()) $ fix $ \loop -> do
        first_line <- hGetLine hdl
        printf "First Line: %s\n" first_line
        let messageType = takeWhile (/=':') first_line
        printf "MessageType: %s\n" messageType
        --let chatroom = dropWhile (/=' ') chatroom_
        case words first_line of
            -- if an exception is caught, send a message and break the loop
            "JOIN_CHATROOM:"    : a -> joinChatroom hdl first_line serverPort broadcast >> loop
            "KILL_SERVICE"      : a -> close mainLoopSock
            "HELO"              : a -> handleHeloText hdl serverPort sockAddr >> loop
            -- else continue looping
            _                   -> leaveOrMessageHandler hdl first_line reader broadcast loop


    hClose hdl

checkList :: String -> [(Int,String)] -> Bool
checkList _ [] = False
checkList item (x:xs) = if snd x == item then True else checkList item xs

addToList :: String -> [(Int,String)] -> [(Int,String)]
addToList item [] = [(1, item)]
addToList item xs = if checkList item xs == True then xs else ((length xs) + 1,item):xs

joinChatroom :: Handle -> String -> PortNumber -> (String -> IO()) -> IO()
joinChatroom hdl join_chatroom serverPort broadcast = do
    client_ip <- hGetLine hdl
    printf "JOIN LINE 2: %s\n" client_ip
    client_port <- hGetLine hdl
    printf "JOIN LINE 3: %s\n" client_port
    client_name <- hGetLine hdl
    printf "JOIN LINE 4: %s\n" client_name

    let joinChatroom = tail $ dropWhile (/=' ') join_chatroom
    let clientIP = tail $ dropWhile (/=' ') client_ip
    let clientPort = tail $ dropWhile (/=' ') client_port
    let clientName = tail $ dropWhile (/=' ') client_name
    printf "Client: %s\n" clientName

    hPutStrLn hdl ("JOINED_CHATROOM: " ++ joinChatroom)
    hFlush hdl
    hPutStrLn hdl ("SERVER_IP: 89.101.57.98")
    hFlush hdl
    hPutStrLn hdl ("PORT: " ++ show serverPort)
    hFlush hdl
    hPutStrLn hdl ("ROOM_REF: 1")
    hFlush hdl
    hPutStrLn hdl ("JOIN_ID: 99")
    hFlush hdl
    broadcast ("--> " ++ clientName ++ " entered chat.")
    printf "Message broadcast\n"


handleHeloText :: Handle -> PortNumber -> SockAddr -> IO()
handleHeloText hdl serverPort sockAddr = do
    (Just hostName, _) <- getNameInfo [] True False sockAddr
    hPutStrLn hdl ("HELO BASE_TEST\nIP: "++ hostName ++ "\nPort:" ++ show serverPort ++ "\nStudentID:13324902\n")
    hFlush hdl

leaveOrMessageHandler :: Handle -> String -> ThreadId -> (String -> IO()) -> IO() -> IO()
leaveOrMessageHandler hdl first_line reader broadcast loop = do
    join_id <- hGetLine hdl
    printf "MESSAGE LINE 2: %s\n" join_id
    client_name <- hGetLine hdl
    printf "MESSAGE LINE 2: %s\n" client_name
    let clientName = tail $ dropWhile (/=' ') client_name
    let chatroom = tail $ dropWhile (/=' ') first_line
    case first_line of
        "LEAVE_CHATROOM:"    -> leaveChatroom hdl clientName reader broadcast
        _                   -> messageHandler hdl chatroom clientName broadcast loop

leaveChatroom :: Handle -> String -> ThreadId -> (String -> IO()) -> IO()
leaveChatroom hdl clientName reader broadcast = do
    hPutStrLn hdl ("DISCONNECT: 0\nPORT: 0\nCLIENT_NAME:" ++ clientName)
    hFlush hdl
    killThread reader
    broadcast ("<-- " ++ clientName ++ " left.")

messageHandler :: Handle -> String -> String -> (String -> IO()) -> IO() -> IO()
messageHandler hdl chatroom clientName broadcast loop = do
    message_ <- hGetLine hdl
    let message = dropWhile (/=' ') message_
    broadcast ("CHAT: " ++ chatroom ++ "\nCLIENT_NAME: " ++ clientName ++ "\nMESSAGE: " ++ message) >> loop

sendToClients :: Handle -> String -> IO()
sendToClients hdl line = do
    hPutStrLn hdl line
    hFlush hdl