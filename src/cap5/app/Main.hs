module Main (main) where

import SimpleJSON
import Prettify
import PrettyJSON


value = renderJValue $ JObject [("f", JNumber 1), ("q", JBool True)]
main :: IO ()
main = do
    putStrLn (pretty 10 value)
    putStrLn (pretty 20 value)
    putStrLn (pretty 30 value)
