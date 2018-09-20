module Main where

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

-- #set
-- @param [[Int]], Int, Int, Int
-- @return [[Int]]
-- Returns board with the value at the position (i, j).
set :: [[Int]] -> Int -> Int -> Int -> [[Int]]
set board value i j = modifyArr2 board (modifyArr (board !!i) value j) i
-- END: set

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

-- #solve
-- @param [[Int]]
-- @return [[Int]]
-- Calls the backtracking function and returns the completed board 
backtrack :: [[Int]] -> [(Int, Int)] -> [[[Int]]] -> Int -> Int -> [[Int]]
backtrack board prev used i j =
    if (isFull board) then board
    else
        if null (subtractArr (possibilities board i j) (used !!i !!j)) then
            backtrack
            (set board 0 (fst (last prev)) (snd (last prev)))
            (init prev)
            (rmUsed used i j)
            (fst (last prev))
            (snd (last prev))
        else                
            backtrack
            (set board (head (subtractArr (possibilities board i j) (used !!i !!j))) i j)
            (prev ++ (zip [i] [j]))
            (addUsed used (head(subtractArr (possibilities board i j) (used !!i !!j))) i j)
            (fst(indexOf (set board (head (subtractArr (possibilities board i j) (used !!i !!j))) i j) i j))
            (snd(indexOf (set board (head (subtractArr (possibilities board i j) (used !!i !!j))) i j) i j))
-- END: backtrack

-- #solve
-- @param [[Int]]
-- @return [[Int]]
-- Calls the backtracking function and returns the completed board 
solve :: [[Int]] -> [[Int]]
solve board =
    backtrack
    board
    [(0,0)]
    (replicate (length board) (replicate (length board) []))
    (fst(indexOf board 0 0))
    (snd(indexOf board 0 0))
-- END: solve

-- #main
main = do
    let board_4x4 =
          [[2, 0, 0, 4],
           [4, 0, 3, 0],
           [0, 2, 4, 1],
           [1, 4, 0, 3]]

    let board_9x9 = 
          [[0, 6, 0, 1, 0, 4, 0, 5, 0],
           [0, 0, 8, 3, 0, 5, 6, 0, 0],
           [2, 0, 0, 0, 0, 0, 0, 0, 1],
           [8, 0, 0, 4, 0, 7, 0, 0, 6],
           [0, 0, 6, 0, 0, 0, 3, 0, 0],
           [7, 0, 0, 9, 0, 1, 0, 0, 4],
           [5, 0, 0, 0, 0, 0, 0, 0, 2],
           [0, 0, 7, 2, 0, 6, 9, 0, 0],
           [0, 4, 0, 5, 0, 8, 0, 7, 0]]
    putStrLn "\nTablero original" 
    putStrLn (show board_9x9)
    putStrLn "\nTablero Resuelto" 
    putStrLn (show (solve board_9x9) )
    putStrLn ""
-- END: main