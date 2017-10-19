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

    --hPutStrLn hdl "Hi, what's your name?"
    joinRequest <- fmap init (hGetLine hdl)
    let requestSplit = lines joinRequest

    broadcast ("--> " ++ head (requestSplit) ++ " entered chat.")
    hPutStrLn hdl ("Welcome " ++ head (requestSplit) ++ "!")

    commLine <- dupChan chan

    -- fork off a thread for reading from the duplicated channel
    reader <- forkIO $ fix $ \loop -> do
        (nextNum, line) <- readChan commLine
        when (msgNum /= nextNum) $ hPutStrLn hdl line
        loop

    -- read lines from the socket and echo them back to the user
    handle (\(SomeException _) -> return()) $ fix $ \loop -> do
        line <- fmap init (hGetLine hdl)
        case line of
            -- if an exception is caught, send a message and break the loop
            "quit"  -> hPutStrLn hdl "Bye!"
            --else continue looping
            _       -> broadcast (head (requestSplit) ++ ": " ++ line) >> loop

    killThread reader
    broadcast ("<-- " ++ head (requestSplit) ++ " left.")
    hClose hdl