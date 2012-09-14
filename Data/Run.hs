import System.Console.CmdArgs
import System.IO

import Data.Sequence as S
import Tests


data Config = Config {
    test :: String
  , seqMode :: Bool
  , runBuffer :: Int
  } deriving (Show, Data, Typeable, Eq)
              
argspec = Config {
    test = "partition"
  , seqMode = False
  , runBuffer = 50
  }
          
main :: IO ()
main = do
    cfg <- cmdArgs argspec
    let doTest = case lookup (test cfg) tests of
          Just t -> if (seqMode cfg) then snd t else fst t
          Nothing -> error $ (test cfg) ++ " not a test"
    
    hPutStrLn stdout "#n\tt (ms)"
    doTest [10, 100, 1000, 10000, 100000, 1000000] (runBuffer cfg)
