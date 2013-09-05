{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures -fno-warn-type-defaults -fno-warn-unused-do-bind -fno-warn-unused-imports -fno-warn-orphans #-}

--
-- Random walk emitter with go and stop button
--

module Main where

import           Control.Applicative
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (async, wait)
import qualified Control.Foldl as L
import           Control.Monad (forever)
import           Data.Random.Normal (mkNormals)

import           Pipes
import           Pipes.Concurrent
import qualified Pipes.Prelude as P

-- Buttons
data Buttons = Go
             | Stop
             | Reset
             | Quit
             deriving (Show, Eq)

help = putStrLn "(g)o (s)top (r)eset (q)uit"

ui :: IO Buttons
ui = loop
  where
    loop = do
        command <- getLine
        case command of
            "q" -> return Quit
            "s" -> return Stop
            "g" -> return Go
            "r" -> return Reset
            _ -> do
                help
                loop


-- simulation
seed      = 42  -- random seed
maxStream = 1000  -- maximum number of random values
delay     = 0.1 -- delay in seconds
start     = 0   -- random walk start
drift     = 0   -- random walk drift
sigma     = 1   -- volatility
dt        = 1   -- time grain

--adding a time dimension
delayer :: Double -> Pipe a a IO ()
delayer d = forever $ do
    a <- await
    lift $ threadDelay $ floor $ 1000000 * d
    yield a

-- turns a random stream into a random walk stream
walker :: Double -> Double -> Double -> Double -> Pipe Double Double IO ()
walker st dr sgma t = go st
  where
    go s = do
        n <- await
        let ss = s + dr * t + sgma * sqrt t * n
        yield ss
        go ss

-- takes a Button and pauses the b stream
pauser :: Producer Buttons IO ()
       -> Producer b IO ()
       -> Producer b IO ()
pauser = go
  where
    go btn stream = do
        e1 <- lift $ next btn
        case e1 of
            Left  _          -> return ()
            Right (a, btn') ->
                case a of
                    Quit -> return ()
                    Stop -> go btn' stream
                    Go   -> do
                        e2 <- lift $ next stream
                        case e2 of
                            Left _             -> return ()
                            Right (s, stream') -> do
                                yield s
                                go btn' stream'


-- exponential moving average
data Ema = Ema
       { numerator   :: {-# UNPACK #-} !Double
       , denominator :: {-# UNPACK #-} !Double
       }

ema :: Double -> L.Fold Double Double
ema alpha = L.Fold step (Ema 0 0) (\(Ema n d) -> n / d)
  where
    step (Ema n d) n' = Ema ((1 - alpha) * n + n') ((1 - alpha) * d + 1)

emaSq :: Double -> L.Fold Double Double
emaSq alpha = L.Fold step (Ema 0 0) (\(Ema n d) -> n / d)
  where
    step (Ema n d) n' = Ema ((1 - alpha) * n + n' * n') ((1 - alpha) * d + 1)

estd :: Double -> L.Fold Double Double
estd alpha = (\s ss -> sqrt (ss - s**2)) <$> ema alpha <*> emaSq alpha

stats :: L.Fold Double (Double, Double, Double, Double)
stats = (,,,) <$> ema 0.5 <*> estd 0.5 <*> ema 0 <*> ema 1

scan :: (Monad m) => L.Fold a b -> Pipe a b m r
scan (L.Fold step begin done) = P.scan step begin done 

main :: IO ()
main = do
    (input,output) <- spawn (Latest Stop)
    _ <- async $ do
	runEffect $ lift ui >~ toOutput input
	performGC
    a2 <- async $ do
        runEffect $
            for (pauser (fromInput output) $
                 (each . mkNormals) seed
                 >-> P.take maxStream
                 >-> delayer delay
                 >-> walker start drift sigma dt
                 >-> scan stats) $
            lift . print
        performGC
    wait a2
