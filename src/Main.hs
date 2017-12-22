module Main where

import EVM.Instructions

main :: IO ()
main = mapM_ print $ readProgram "\x61\x0f\x00\x00" 0
