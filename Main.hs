module Main where

import           Anagrams
import qualified Data.Text          as T
import           System.Environment (getArgs)

data Options = Options { input :: String
                       , dictPath :: FilePath
                       }

opts :: [String] -> Options
opts args = case args of
  [inp] -> Options { input = inp, dictPath = "/usr/share/dict/words" }
  ["-w", path, inp] -> Options { input = inp, dictPath = path }
  _ -> error "specify at least a word or phrase to search for"

main :: IO ()
main = do
  args <- getArgs
  let o = opts args
  dict <- readDict' $ dictPath o
  mapM_ (putStrLn . T.unpack) $ anagrams dict (T.pack $ input o)
