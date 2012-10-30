import Combinations
import Polynomials

main = do
    putStrLn "Edges?"
    e <- getLine
    putStrLn "Taylor ="
    print (taylorExpand 1 (read e) 10)
