{-# LANGUAGE OverloadedStrings, TupleSections #-}
module Server (server) where

import Game
import Control.Monad
import Data.Function (fix)
import qualified Data.Map as M
import Data.IORef
import qualified Data.ByteString as B
import Data.Char
import Network.Socket
import System.IO
import Control.Concurrent

type GameID = Int
type Player = Handle
type Move   = ((Int, Int), Piece)
data Res = Win | Loss | Tie deriving (Eq, Show)

data State = State
  { counter :: GameID
  , lobby   :: Maybe Player
  }

server :: IO ()
server = do
  ref <- newIORef $ State 0 Nothing
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 4242 iNADDR_ANY)
  listen sock 2
  mainLoop sock ref

mainLoop sock ref = do
  conn <- accept sock
  forkIO $ handle ref $ fst conn
  mainLoop sock ref


-- Connect two players
handle ref s = do
  p1 <- socketToHandle s ReadWriteMode
  hSetBuffering p1 NoBuffering
  st <- readIORef ref
  case lobby st of
    Just p2 -> do
      writeIORef ref st { lobby = Nothing }
      startGame (counter st) p2 p1
    Nothing -> do
      putStrLn "Player in lobby"
      writeIORef ref st { counter = counter st + 1, lobby = Just p1 }

-- Inform players about the game
startGame gid p1 p2 = do
  putStrLn "Starting game!"
  -- hPutStrLn p1 $ show gid ++ " 1"
  -- hPutStrLn p2 $ show gid ++ " 2"
  hPutStr p1 "1"
  hPutStr p2 "2"
  play p1 p2 board

-- Aaaand action!
play p1 p2 b = do
  m@(l,x) <- askMove p1
  let (r, b') = getRes m b
  case r of
    Just Win  -> hPutStr p1 "W" >> hPutStr p2 "L"
    Just Loss -> hPutStr p2 "W" >> hPutStr p1 "L"
    Just Tie  -> hPutStr p2 "T" >> hPutStr p1 "T"
    Nothing -> do
      tellMove p2 m
      putStrLn $ printBoard b'
      play p2 p1 b'

getRes (l,x) b = case put b l x of
  Nothing -> (pure Loss, b)
  Just b' ->
    case winner b' of
      Just y | x==y -> (pure Win, b')
          | x/=y -> (pure Loss, b')
      Nothing  | isFull b' -> (pure Tie, b')
         | otherwise -> (Nothing, b')


askMove :: Player -> IO Move
askMove p = do
  m <- parseMove <$> replicateM 3 (hGetChar p)
  case m of
    Nothing -> askMove p
    Just x -> return x

parseMove ln = do
  [p,x,y] <- pure $ pure <$> ln
  return ((read x, read y), read p)

tellMove :: Player -> Move -> IO ()
tellMove p ((x,y),piece) =
  -- hPutStrLn p $ show piece ++ " " ++ show x ++ " " ++ show y
  hPutStr p $ show piece ++ show x ++ show y

