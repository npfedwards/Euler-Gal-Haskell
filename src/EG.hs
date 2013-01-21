import EGPowerSeries
import Checks
import Combinations
import Polynomials
import System.Environment
import Complexes
import Data.List

dispatch :: [(String, [String] -> IO())]
dispatch =  [ ("star", showStar)
            , ("graph", showGraph)
            , ("taylor", showTaylor)
            , ("multbrackets", showMultiplyBrackets)
            , ("2D", run2D)
            , ("onerobot", oneR)
            , ("kpage", kP)
            , ("gen", gen)
            , ("link", link)
            , ("eg", eg)
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

run2D :: [String] -> IO() -- Takes an v an Integer, and e,f [[Integer]] lists of simplices defined by vertices. Gives us Chi of our 2D Config Space.
run2D [file,r] = do
    thefile <- readFile file
    let [f,ae] = lines thefile
    let faces = checkEorF (read f)
    let edges = checkEorF (genSimplices faces 2 ++ (read ae))
    let v = genVertices edges faces
    print(pullChi(eg2D v edges faces (read r)) (read r))

oneR :: [String] -> IO()
oneR [file] = do
    thefile <- readFile file
    let [v,e,f] = lines thefile
    print(oneRobot (read v) (read e) (read f))

kP :: [String] -> IO()
kP [k,r] = do
    print(kPage (read k) (read r))

gen :: [String] -> IO()
gen [thelist] = do
    let list = read thelist
    let n = genericLength (head list)
    print(cleanList (genAll list 1 n [[[]]]))

link :: [String] -> IO()
link [thelist, simplex] = do
    let list = read thelist
    let n = genericLength (head list)
    let newlist = cleanList (genAll list 1 n [[[]]])
    print(linkofnsimplex (read simplex) newlist)

eg :: [String] -> IO()
eg [thelist, robots] = do
    let list = read thelist
    let r = read robots
    let n = genericLength(head list)
    let newlist = cleanList (genAll list 1 n [[[]]])
    print(pullChi(egGeneral newlist r) r)
