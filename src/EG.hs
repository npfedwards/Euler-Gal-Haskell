import EGPowerSeries
import Checks
import Combinations
import Polynomials
import System.Environment
import Complexes

dispatch :: [(String, [String] -> IO())]
dispatch =  [ ("star", showStar)
            , ("graph", showGraph)
            , ("taylor", showTaylor)
            , ("multbrackets", showMultiplyBrackets)
            , ("2D", run2D)
            ]

main = do
    (command:args) <- getArgs
    let (Just action) = lookup command dispatch
    action args

showStar :: [String] -> IO()
showStar [e, r] = do
    print(pullChi (egStarGraph (read e) (read r)) (read r))

showGraph :: [String] -> IO()
showGraph [v, r] = do
    print(pullChi (egGraph (read v) (read r)) (read r))

showTaylor :: [String] -> IO()
showTaylor [a, b, bound, lowerbound] = do
    print(pullChi (taylorExpand (read a) (read b) (read bound) (read lowerbound)) (read bound))

showMultiplyBrackets :: [String] -> IO()
showMultiplyBrackets [a, b, bound] = do
    print(pullChi(multiplyBrackets  (taylorExpand (read a) (read b) (read bound) ((read bound)-2)) [1, 1-(read b)]) (read bound))

run2D :: [String] -> IO()
run2D [v, e, f, r] = do
    print(pullChi(eg2D (verticesMake (read v)) (read e) (read f) (read r)) (read r))
