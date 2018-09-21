import           Control.Monad                      (void)

import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny.SVG  as SVG

-- >>>>> GENERIC METHODS <<<<<

-- \\Private
-- #root2_p
-- @param Int, Int
-- @return Int
-- Returns the floor-square-root of the number.
froot2_p :: Int -> Int -> Int
froot2_p n i = if (i*i) > n then (i-1) else (froot2_p n (succ i))
-- END: froot2_p

-- #root2
-- @param Int
-- @return Int
-- Returns the floor-square-root of the number.
froot2 :: Int -> Int
froot2 n = froot2_p n 0
-- END: froot2

-- #subtractArr
-- @param [Int], [Int]
-- @return [Int]
-- Returns all contained into de first arrar but not into the second.
subtractArr :: [Int] -> [Int] -> [Int]
subtractArr array1 array2 = [x | x <- array1, notElem x array2]
-- END: subtractArr

-- #modifyArr
-- @param [Int], Int, Int
-- @return [Int]
-- Returns a new array with the value at the position j.
modifyArr :: [Int] -> Int -> Int -> [Int]
modifyArr array value j = [if x == j then value else array !!x  | x<- [0..((length array)-1)]]
-- END: modifyArr

-- #modifyArr2
-- @param [[Int]], [Int] Int
-- @return [[Int]]
-- Returns a new matrix with the array at the position i.
modifyArr2 :: [[Int]] -> [Int] -> Int -> [[Int]]
modifyArr2 matrix array i = [if x == i then array else matrix !!x  | x<- [0..((length matrix)-1)]]
-- END: modifyArr2

-- >>>>> SPECIAL METHODS <<<<<

-- #rmUsed
-- @param [[Int]], Int Int
-- @return [[Int]]
-- Returns a new matrix3 wit an empty array at the position (i, j).
rmUsed :: [[[Int]]] -> Int -> Int -> [[[Int]]]
rmUsed matrix3 i j = [[ if(y == j && x == i) then [] else (matrix3 !!x !!y) | y <- [0..((length (matrix3 !!x))-1)]]  | x <- [0..((length matrix3)-1)]]
-- END: rmUsed

-- #addUsed
-- @param [[Int]], Int Int
-- @return [[Int]]
-- Returns a new matrix3 without the array at the position (i, j).
addUsed :: [[[Int]]] -> Int -> Int -> Int -> [[[Int]]]
addUsed matrix3 value i j = [[ if(y == j && x == i) then ((matrix3 !!x !!y) ++ [value]) else (matrix3 !!x !!y) | y <- [0..((length (matrix3 !!x))-1)]]  | x <- [0..((length matrix3)-1)]]
-- END: addUsed

-- #square
-- @param Int, Int
-- @return Int
-- Returns the square of the position,
-- It depends of the size.
square :: Int -> Int -> Int
square size position = (position `div` (froot2 size))
-- END: square

-- #setposition
-- @param [[Int]], Int, Int, Int
-- @return [[Int]]
-- Returns board with the value at the position (i, j).
setposition :: [[Int]] -> Int -> Int -> Int -> [[Int]]
setposition board value i j = modifyArr2 board (modifyArr (board !!i) value j) i
-- END: setposition

-- #isFull
-- @param [[Int]]
-- @return Bool
-- Checks if there is 0 Into de board,
-- that means there's an empty slot.
isFull :: [[Int]] -> Bool
isFull board = sum [1 | x <- board, 0 `elem` x] == 0
-- END: isFull

-- #indexOf
-- @param [[Int]], Int, Int
-- @return (Int, Int)
-- Returns the index of the next cero.
indexOf :: [[Int]] -> Int -> Int -> (Int, Int)
indexOf board i j =
    if(board !!i !!j) == 0 then
        head (zip [i] [j])
    else if j < ((length (board !!i))-1) then indexOf board i (succ j) else indexOf board (succ i) 0
-- END: indexOf

-- #mapUnavailable
-- @param [[Int]], Int, Int
-- @return [Int]
-- Returns a list of de unavailable numbers.
mapUnavailable :: [[Int]] -> Int -> Int -> [Int]
mapUnavailable board i j =
    (board !!i) ++ -- Unavailable numbers (Horizontal)
    [y !!j | y <- board, (y !!j) /= 0] ++ -- Unavailable numbers (Vertical)
    [board !!row !!col | row <- [((square (length board) i) * (froot2 (length board))) .. (((square (length board) (i + (froot2 (length board)))) * (froot2 (length board)))-1)], col <- [((square (length board) j) * (froot2 (length board))) .. (((square (length board) (j + (froot2 (length board)))) * (froot2 (length board)))-1)]] -- Unavailable numbers (Square)
-- END: mapUnavailable

-- #possibilities
-- @param [[Int]], Int, Int
-- @return [Int]
-- Returns all the posible numbers.
possibilities :: [[Int]] -> Int -> Int -> [Int]
possibilities board i j = subtractArr [1..(length board)] (mapUnavailable board i j)
-- END: possibilities

-- #backtrack
-- @param [[[Int]]], [[Int]], [(Int, Int)], [[[Int]]], Int, Int
-- @return [[[Int]]]
-- Evaluate each position with its possible values,
-- Decide whether to return or continue.
-- Returns the map of every correct step.
backtrack :: [[[Int]]] -> [[Int]] -> [(Int, Int)] -> [[[Int]]] -> Int -> Int -> [[[Int]]]
backtrack track board step used i j =
    if (isFull board) then (track ++ [board]) -- The board is completed... finish.
    else
        if null (subtractArr (possibilities board i j) (used !!i !!j)) then -- There aren't more posibilities... return.
            backtrack
            (init track)
            (setposition board 0 (fst (last step)) (snd (last step)))
            (init step)
            (rmUsed used i j)
            (fst (last step))
            (snd (last step))
        else -- There are more posibilities... continue. 
            backtrack
            (track ++ [board])
            (setposition board (head (subtractArr (possibilities board i j) (used !!i !!j))) i j)
            (step ++ (zip [i] [j]))
            (addUsed used (head(subtractArr (possibilities board i j) (used !!i !!j))) i j)
            (fst(indexOf (setposition board (head (subtractArr (possibilities board i j) (used !!i !!j))) i j) i j))
            (snd(indexOf (setposition board (head (subtractArr (possibilities board i j) (used !!i !!j))) i j) i j))
-- END: backtrack

-- #solve
-- @param [[Int]]
-- @return [[[Int]]]
-- Calls the backtracking function and returns the complete right track of the solution.
solve :: [[Int]] -> [[[Int]]]
solve board = tail (backtrack [[[]]] board [(0,0)] (replicate (length board) (replicate (length board) [])) (fst(indexOf board 0 0)) (snd(indexOf board 0 0)))
-- END: solve

board_4x4 :: [[Int]]
board_4x4 = [[0, 3, 4, 0],
         [4, 0, 0, 2],
         [1, 0, 0, 3],
         [0, 2, 1, 0]]

board_9x9 :: [[Int]]
board_9x9 = [[0, 6, 0, 1, 0, 4, 0, 5, 0],
           [0, 0, 8, 3, 0, 5, 6, 0, 0],
           [2, 0, 0, 0, 0, 0, 0, 0, 1],
           [8, 0, 0, 4, 0, 7, 0, 0, 6],
           [0, 0, 6, 0, 0, 0, 3, 0, 0],
           [7, 0, 0, 9, 0, 1, 0, 0, 4],
           [5, 0, 0, 0, 0, 0, 0, 0, 2],
           [0, 0, 7, 2, 0, 6, 9, 0, 0],
           [0, 4, 0, 5, 0, 8, 0, 7, 0]]

toStr4 :: String
toStr4 = show (board_4x4 !!0)
         ++ "<br>"
         ++ show (board_4x4 !!1)
         ++ "<br>"
         ++ show (board_4x4 !!2)
         ++ "<br>"
         ++ show (board_4x4 !!3)

toStr9 :: String
toStr9 = show (board_9x9 !!0)
         ++ "<br>"
         ++ show (board_9x9 !!1)
         ++ "<br>"
         ++ show (board_9x9 !!2)
         ++ "<br>"
         ++ show (board_9x9 !!3)
         ++ "<br>"
         ++ show (board_9x9 !!4)
         ++ "<br>"
         ++ show (board_9x9 !!5)
         ++ "<br>"
         ++ show (board_9x9 !!6)
         ++ "<br>"
         ++ show (board_9x9 !!7)
         ++ "<br>"
         ++ show (board_9x9 !!8)
         ++ "<br>"

formatLine:: [[Int]] -> Int -> [Char]
formatLine board i =
    if (i ==  ((length board)-1))
        then (show(board !!i))
    else
        ((show (board !!i)) ++ "<br>" ++ (formatLine board (succ i)) ++ "<br>" )

showSteps :: [[[Int]]] -> Int -> [Char]
showSteps steps i =
    if (i ==  ((length steps)-1))
        then ("Solucion:" ++ "<br>" ++ formatLine (steps !!i) 0)
    else
        (( "Paso #"
            ++ (show i))
            ++ "<br>"
            ++ (formatLine (steps !!i) 0)
            ++ "<br>"
            ++ (showSteps steps (succ i))
            ++ "<br> <br>")
{-----------------------------------------------------------------------------
    Sudoku
------------------------------------------------------------------------------}
main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup w = void $ do
    return w # set title "Resolución de Sudoku con Haskell"

    heading <- UI.h1 # set text "Resolviendo Sudokus con haskell"

    getBody w #+ [element heading
                 , UI.div # set html htmlInject
                 ]

htmlInject :: String
htmlInject = "<div>"
         ++ "<h2>Sudoku de 9x9 sin resolver</h2>"
         ++ "<div>"--Sudoku 9x9 sin resolver
         ++  (toStr9) ++ "<br>"
         ++ "</div>"
         ++ "<h2>Sudoku de 9x9 resuelto</h2>"
         ++ "<div>"
         ++ (showSteps (solve board_9x9) 0)--coloca el sudoku de 9x9 resuelto
         ++ "</div>"
         ++ "<br>"
         ++ "<h2>Sudoku de 4x4 sin resolver</h2>"
         ++ "<div>"
         ++ (toStr4) ++ "<br>"
         ++ "</div>"
         ++ "<h2>Sudoku de 4x4 resuelto</h2>"
         ++ "<div>"
         ++ (showSteps (solve board_4x4) 0) -- colocar el sudoku de 4x4 resuelto
         ++ "</div>"