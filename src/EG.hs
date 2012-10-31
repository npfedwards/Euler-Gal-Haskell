import EGPowerSeries

main = do
    putStrLn    "How many edges in the star graph?"
    e <- getLine
    putStrLn    "How many robots on the graph?"
    r <- getLine
    print(pullChi (egStarGraph (read e) (read r)) (read r))
