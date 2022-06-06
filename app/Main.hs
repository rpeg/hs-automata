module Main where

import           Configuration                  ( parseConfiguration )
import           Game                           ( simulate )
import           System.Environment

main :: IO ()
main = do
    config <- parseConfiguration
    print config

