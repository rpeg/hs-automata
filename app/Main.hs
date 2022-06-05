module Main where

import           Game                           ( simulate )
import           Parser                         ( parseOptions )
import           System.Environment

main :: IO ()
main = do
    opts <- parseOptions
    let states = simulate opts
    print states

