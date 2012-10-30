import Combinations
import IntegerMaths as IM

main = do
    putStrLn "n?"
    n <- getLine
    putStrLn "r?"
    r <- getLine
    putStrLn "nPr ="
    print (Combinations.nPr (read n) (read r))
    putStrLn "nCr ="
    print (Combinations.nCr (read n) (read r))
