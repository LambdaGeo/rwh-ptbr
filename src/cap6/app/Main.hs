{-# LANGUAGE FlexibleInstances #-}

module Main (main) where

import SimpleJSON


main :: IO ()
main = print $ toJValue (10::Integer)
