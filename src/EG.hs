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
            ]

main = do
    (command:args) <- getArgs
    let (Just action) = lookup command dispatch
    action args
    print(linkofedge2D [1,2] [[1,2,3],[1,3,2],[],[5,4,3],[1,3,5],[2,3,4]])

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
