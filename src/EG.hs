import EGPowerSeries
import Checks
import Combinations

main = do
    n <- getLine
    print(starChecks (read n))
