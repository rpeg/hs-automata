module Lib
    ( makeAutomataConfig
    , AutomataConfig
    ) where

import Text.Regex.TDFA


data AutomataRule = AutomataRule
    { birth    :: Int
    , survival :: Int
    }
    deriving (Eq, Show)

data AutomataConfig = AutomataConfig
    { rule          :: AutomataRule
    , seed          :: Int
    , density       :: Double
    , numIterations :: Int
    , output        :: String
    }
    deriving (Eq, Show)

data ValidationError
    = InvalidRuleString String | InvalidDensity Double | InvalidFilepath String
     deriving (Eq, Show)

bsRuleRegex :: String
bsRuleRegex = "B([0-9]+)/S([0-9]+)"

-- Rules have format "B{number}/S{number}"
-- https://conwaylife.com/wiki/Rulestring
makeAutomataRule :: String -> Either ValidationError AutomataRule
makeAutomataRule r = do 
    let matches = r =~ bsRuleRegex :: [[String]]
    if length matches == 2
        then Right(AutomataRule{ birth = read (matches!!0!!1) :: Int, survival = read (matches!!1!!1) :: Int })
        else Left(InvalidRuleString r)

makeAutomataConfig :: String -> Int -> Double -> Int -> String -> Either ValidationError AutomataConfig
makeAutomataConfig ruleStr seed density numIterations output = do
    case makeAutomataRule ruleStr of 
        Left err -> Left(err)
        Right rule -> Right(AutomataConfig{ rule=rule, seed=seed, density=density, numIterations=numIterations, output=output })