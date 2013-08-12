{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-type-defaults -fno-warn-unused-do-bind #-}

{-| Testing basic interaction between

  - StateT
  - Pipes, and
  - tcp server concepts
  - using inner pipes (pipes within another pipe component with a wider context)
-}

module Main where

import           Control.Monad (forever)
import           Control.Monad.Trans.State.Strict
    (StateT, get, put, evalStateT)
import qualified Data.ByteString.Char8 as C

import qualified Network.Simple.TCP as NST
import qualified Network.Socket as NS
    (ServiceName, Socket, SockAddr, sClose)
import           Pipes
import           Pipes.Concurrent
import           Pipes.Network.TCP

-- helpers
host1p = NST.Host "127.0.0.1"
port  = show 35660

logger = putStrLn

server :: NST.HostPreference -> NS.ServiceName -> IO ()
server hp p = NST.serve hp p $ \(sock, _laddr) -> do
    logger "TCP server up"
    run (fromSocket sock 4096 >-> toSocket sock)

-- event, state

type Plug = Maybe (NS.Socket, NS.SockAddr)

data Event = Connect     -- initially connect to port
	   | Quit        -- disconnect
	   | Receive     -- print to stdout
	   | Send String -- send a stream

help :: IO ()
help = putStrLn "(c)onnect (q)uit (r)eceive (s)end"

-- keypress events
user :: IO Event
user = do
    command <- getLine
    case command of
	"c" -> return Connect
	"q" -> return Quit
	"r" -> return Receive
	('s':xs) -> return $ Send xs
	_   -> do
	       help
	       user

handler :: StateT Plug (Consumer Event IO) ()
handler = forever $ do
        event  <- lift await
        case event of
            Connect -> do
                h <- get
                case h of
                    Just _ -> do
                        lift . lift $ logger "handle already exists in state"
                        return ()
                    Nothing -> do
                        hp <- lift . lift $
                              NST.connectSock "127.0.0.1" port
                        put $ Just hp
            Quit -> do
                h <- get
                lift . lift $ logger "Is this the right way to quit?"
                case h of
                    Nothing -> return ()
                    Just hp -> lift . lift $ NS.sClose $ fst hp
            Receive -> do
                h <- get
                case h of
                    Nothing -> do
                        lift . lift $ logger "need a Connect Event here"
                        return ()
                    Just hp -> do
                        lift . lift $ logger "getting everything"
                        lift . lift $ run $ fromSocket (fst hp) 4096 >->
                            (await >>= lift . C.putStrLn)
            Send x -> do
                h <- get
                case h of
                     Nothing -> return ()
                     Just hp -> lift . lift $ run
                                (lift $ NST.send (fst hp) $ C.pack x)

-- uses Pipes.Concurrent gratuitously
main :: IO ()
main = do
    help
    forkIO $ server host1p port
    (input, output) <- spawn Unbounded
    forkIO $ do run $ lift user >~ toInput input
		performGC
    run $ fromOutput output >-> evalStateT handler Nothing
    return ()
