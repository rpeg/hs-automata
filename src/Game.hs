module Game
    ( simulate
    , State
    ) where

import           Parser
import           System.Random

data Cell = Alive | Dead deriving (Show, Enum)

type State = [[Cell]]

initialize :: Options -> State
initialize o = replicate h $ replicate w $ genCell seed density
  where
    seed    = optSeed o
    density = optDensity o
    h       = optHeight o
    w       = optWidth o

genCell :: Int -> Double -> Cell
genCell seed density | value >= threshold = Alive
                     | otherwise          = Dead
  where
    value     = fst $ randomR (0, 1000) gen
    gen       = mkStdGen seed
    threshold = 1000 - (1000 * density)

stepf :: (State, Options) -> (State, Options)
stepf (s, o) = (s, o)

simulate :: Options -> [State]
simulate o = map fst $ take n $ iterate stepf (initState, o)
  where
    initState = initialize o
    n         = optNumIterations o

