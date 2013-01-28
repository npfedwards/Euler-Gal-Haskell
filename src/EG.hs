-----------------------------------------------------------------------------
--
-- Program     :  EG
-- Copyright   :  Nathan Edwards
-- License     :  GPLv3
--
-- Computations of the Euler Gal Power Series for a configuration space
--
-----------------------------------------------------------------------------

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
            , ("2D", run2D)
            , ("onerobot", oneR)
            , ("kpage", kP)
            , ("eg", eg)
            , ("readme", readme)
            , ("help", readme)
            , ("manifold", mf)
            , ("onerobotgen", oneRG)
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

run2D :: [String] -> IO() -- Takes an v an Integer, and e,f [[Integer]] lists of simplices defined by vertices. Gives us Chi of our 2D Config Space.
run2D [file,r] = do
    thefile <- readFile file
    let [f,ae] = lines thefile
    let faces = checkEorF (read f)
    let edges = checkEorF (genSimplices faces 2 ++ (read ae))
    let v = genVertices edges faces
    print(pullChi(eg2D v edges faces (read r)) (read r))

oneR :: [String] -> IO() -- Gives the result for one robot for a 2D complex
oneR [file] = do
    thefile <- readFile file
    let [v,e,f] = lines thefile
    print(oneRobot (read v) (read e) (read f))

oneRG :: [String] -> IO()
oneRG [list] = do
    print(oneRobotGeneral (read list))

kP :: [String] -> IO() -- Gives Chi of a k-paged book
kP [k,r] = do
    print(kPage (read k) (read r))

eg :: [String] -> IO() -- Gives Chi of an arbitrary complex.
eg [thelist, robots] = do
    let list = read thelist
    let r = read robots
    let n = genericLength(head list)
    let newlist = cleanList (genAll list 1 n [[[]]])
    print(pullChi(egGeneral newlist r) r)

mf :: [String] -> IO() --Gives Chi of a manifold
mf [chi, dim, r] = do
    print(manifold (read chi) (read dim) (read r))

readme :: [String] -> IO() -- Prints the readme
readme [anything] = do
    print("readme")
