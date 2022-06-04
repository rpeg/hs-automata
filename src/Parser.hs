module Parser
    ( showUsage
    ) where

import           Options.Applicative
import Text.Regex.TDFA

data Options = Options
    { optRule          :: String
    , optNumIterations :: Int
    , optSeed          :: Int
    , optDensity       :: Double
    }
    deriving (Eq, Show)

validRule :: String -> Bool
validRule r = r =~ "B[0-9]+/S[0-9]+"

ruleValidator :: ReadM String
ruleValidator = do
    r <- auto
    if validRule r
        then return r
        else readerError "Rule is not of the format \"B{number list}/S{number list}\""

ruleParser :: Parser String
ruleParser = option ruleValidator (long "rule" <> short 'r')

numIterationsParser :: Parser Int
numIterationsParser = option auto (long "numIterations" <> short 'n')

seedParser :: Parser Int
seedParser = option auto (long "seed" <> short 's')

validDensity :: Double -> Bool
validDensity d | d < 0     = False
               | d > 1     = False
               | otherwise = True

densityValidator :: ReadM Double
densityValidator = do
    d <- auto
    if validDensity d
        then return d
        else readerError "Density is not in the range [0,1]"

densityParser :: Parser Double
densityParser = option densityValidator (long "density" <> short 'd')

optionsParser :: Parser Options
optionsParser =
    Options
        <$> ruleParser
        <*> numIterationsParser
        <*> seedParser
        <*> densityParser

showUsage :: IO ()
showUsage = do
    opts <- execParser opts
    print opts
  where
    opts = info
        (optionsParser <**> helper)
        (fullDesc <> progDesc "Generate cellular automata animations" <> header
            "hs-automata"
        )
