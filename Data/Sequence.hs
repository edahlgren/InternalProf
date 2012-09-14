module Sequence where

import Data.Sequence
import Control.Monad
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Console.CmdArgs
import System.IO

-- partition
-- fold
-- take
-- drop
-- concat
-- nubBy
-- sortBy

class Test a where
    builder :: Int -> a

nowInDouble :: IO Double
nowInDouble = fmap realToFrac getPOSIXTime

showTime :: Int -> Double -> IO ()
showTime step time = hPutStrLn stdout $ (show step) ++ "\t" ++ (show time)

stepCommand :: (Test a, Show a) => [Int] -> Int -> (a -> a) -> IO ()
stepCommand steps runBuffer f = mapM_ showT steps
  where 
    showT i = do
        r <- runBuffered runBuffer f $ builder i
        showTime i r

runBuffered :: (Test a, Show a) => Int -> (a -> a) -> a -> IO Double
runBuffered runBuffer f xs = do
    sumBuffered <- foldM incTime 0 [1..runBuffer]
    return $ sumBuffered / fromIntegral runBuffer
  where 
    incTime x _ = timeCommand f xs >>= (\t -> return $ t + x)

timeCommand :: (Test a, Show a) => (a -> a) -> a -> IO Double
timeCommand f xs = do
    before <- nowInDouble
    let !res = f xs
    nowInDouble >>= (\after -> return $ after - before)
           
