{-# LANGUAGE ScopedTypeVariables #-}

module Configuration
    ( Configuration
    , Neighborhood
    , Birth
    , Survival
    , Seed
    , Density
    , parseConfiguration
    , birthList
    , survivalList
    , numIterations
    , neighborhood
    , seed
    , density
    , width
    , height
    ) where

import           Data.Char                      ( digitToInt )
import           Options.Applicative
import           Text.Regex.TDFA

data Neighborhood = Moore | VonNeumann deriving (Eq, Enum, Show)

type Birth = Int
type Survival = Int
type Seed = Int
type Density = Double

data Configuration = Configuration
    { birthList     :: [Birth]
    , survivalList  :: [Survival]
    , numIterations :: Int
    , neighborhood  :: Neighborhood
    , seed          :: Seed
    , density       :: Double
    , width         :: Int
    , height        :: Int
    }
    deriving (Eq, Show)

data Arguments = Options
    { argRuleString    :: String
    , argNumIterations :: Int
    , argSeed          :: Int
    , argDensity       :: Double
    , argWidth         :: Int
    , argHeight        :: Int
    }
    deriving (Eq, Show)

ruleStringRegex = "B([0-9]+)/S([0-9]+)(V?)"

isValidRuleString :: String -> Bool
isValidRuleString r = r =~ ruleStringRegex

ruleStringValidator :: ReadM String
ruleStringValidator = do
    r <- auto
    if isValidRuleString r
        then return r
        else
            readerError
                "Rule is not of the format \"B{number list}/S{number list}{optional 'V' suffix}, e.g. \"B3/S23\", \"B2/S14V\"\""

ruleStringParser :: Parser String
ruleStringParser = option ruleStringValidator (long "rule" <> short 'r')

numIterationsParser :: Parser Int
numIterationsParser =
    option auto (long "numIterations" <> short 'n' <> value 50)

-- TODO support runtime seed generation via Random
seedParser :: Parser Int
seedParser = option auto (long "seed" <> short 's')

validDensity :: Double -> Bool
validDensity d | d <= 0    = False
               | d >= 1    = False
               | otherwise = True

densityValidator :: ReadM Double
densityValidator = do
    d <- auto
    if validDensity d
        then return d
        else readerError "Density is not in the range (0,1)"

densityParser :: Parser Double
densityParser =
    option densityValidator (long "density" <> short 'd' <> value 0.25)

widthParser :: Parser Int
widthParser = option auto (long "width" <> short 'w' <> value 100)

heightParser :: Parser Int
heightParser = option auto (long "height" <> short 'h' <> value 100)

argsParser :: Parser Arguments
argsParser =
    Options
        <$> ruleStringParser
        <*> numIterationsParser
        <*> seedParser
        <*> densityParser
        <*> widthParser
        <*> heightParser

safeIndex :: [a] -> Int -> a -> Maybe a
safeIndex xs i d | i >= 0 && length xs > i = Just (xs !! i)
                 | otherwise               = Nothing

parseRuleConfiguration :: String -> ([Birth], [Survival], Neighborhood)
parseRuleConfiguration ruleString = (b, s, n)
  where
    ms = ruleString =~ ruleStringRegex :: (String, String, String, [String])
    (_, _, _, gs) = ms
    b             = map digitToInt $ head gs
    s             = map digitToInt $ gs !! 1
    suf           = gs !! 2
    n             = case suf of
        "V" -> VonNeumann
        _   -> Moore


parseConfiguration :: IO Configuration
parseConfiguration = do
    args <- execParser parserInfo
    let (birthList, survivalList, neighborhood) =
            parseRuleConfiguration $ argRuleString args
    return Configuration { birthList     = birthList
                         , survivalList  = survivalList
                         , numIterations = argNumIterations args
                         , neighborhood  = neighborhood
                         , seed          = argSeed args
                         , density       = argDensity args
                         , width         = argWidth args
                         , height        = argHeight args
                         }
  where
    parserInfo = info
        (argsParser <**> helper)
        (fullDesc <> progDesc "Generate cellular automata animations" <> header
            "hs-automata"
        )

