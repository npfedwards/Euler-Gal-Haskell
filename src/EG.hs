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
            , ("onerobot", oneR)
            ]

main = do
    (command:args) <- getArgs
    let (Just action) = lookup command dispatch
    action args

showStar :: [String] -> IO() -- Takes edges and robots and gives Chi of the star graph
showStar [e, r] = do
    print(pullChi (egStarGraph (read e) (read r)) (read r))

showGraph :: [String] -> IO() -- Takes a list of vertices and #robots and gives Chi
showGraph [v, r] = do
    print(pullChi (egGraph (read v) (read r)) (read r))

showTaylor :: [String] -> IO() -- Takes (1+ax)^-b and expands [0,...,0 (lowerbound)]++ expand to upper bound. Giving coeff of bound.
showTaylor [a, b, bound, lowerbound] = do
    print(pullChi (taylorExpand (read a) (read b) (read bound) (read lowerbound)) (read bound))

showMultiplyBrackets :: [String] -> IO() -- Takes a Taylor Expansion and (1 + (1-b)x) and multiplies them together (ignoring lower order terms) to get coeff of bound.
showMultiplyBrackets [a, b, bound] = do
    print(pullChi(multiplyBrackets  (taylorExpand (read a) (read b) (read bound) ((read bound)-2)) [1, 1-(read b)]) (read bound))

run2D :: [String] -> IO()
run2D [file,r] = do
    thefile <- readFile file
    let [v,e,f] = lines thefile
    print(pullChi(eg2D (verticesMake (read v)) (read e) (read f) (read r)) (read r))

oneR :: [String] -> IO()
oneR [file] = do
    thefile <- readFile file
    let [v,e,f] = lines thefile
    print(oneRobot (read v) (read e) (read f))
