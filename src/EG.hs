import EGPowerSeries
import Checks
import Combinations

main = do
    putStrLn "E?"
    e <- getLine
    putStrLn "R?"
    r <- getLine
    print(pullChi(egStarGraph (read e) (read r)) (read r))
