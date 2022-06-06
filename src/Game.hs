module Game
    ( simulate
    , State
    ) where

import           Configuration
import System.Random ( mkStdGen, Random(randomR) )

data Cell = Alive | Dead deriving (Show, Enum)

type State = [[Cell]]

initialize :: Configuration -> State
initialize con = replicate h $ replicate w $ genCell s d
  where
    s = seed con
    d = density con
    h = height con
    w = width con

genCell :: Seed -> Density -> Cell
genCell s d | value >= threshold = Alive
            | otherwise          = Dead
  where
    value     = fst $ randomR (0, 1000) gen
    gen       = mkStdGen s
    threshold = 1000 - (1000 * d)

stepf :: (State, Configuration) -> (State, Configuration)
stepf (s, o) = (s, o)

simulate :: Configuration -> [State]
simulate con = map fst $ take n $ iterate stepf (initState, con)
  where
    initState = initialize con
    n         = numIterations con

