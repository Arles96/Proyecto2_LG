module Main where

-- #isFull
-- @param [[Int]] 
-- @return Bool 
-- Checks if there is 0 Into de board
-- that means there's an empty slot.
isFull :: [[Int]] -> Bool
isFull board = sum [1 | x <- board, 0 `elem` x] == 0
-- END: isFull

-- #findCeros
-- @param [[Int]] 
-- @return [[Int]]
-- Returns the index of each cero.
findCeros :: [[Int]] -> [[Int]]
findCeros board = [[y | y <- [0..((length board)-1)], (board !!x !!y) == 0] | x <- [0..((length board)-1)]]
-- END: findCeros

-- #square
-- @param Int, Int 
-- @return Int
-- Returns the square of the position,
-- It depends of the size.
square :: Int -> Int -> Int
-- 3 x 3
square 3 x =
    if(x `elem` [0..2])
    then 0        
    else if(x `elem` [3..5])
        then 3        
        else if(x `elem` [6..8])
            then 6
            -- #Error: Out of range.
            else -1        
-- 4 x 4
square 4 x =
    if(x `elem` [0..3])
    then 0        
    else if(x `elem` [4..7])
        then 4        
        else if(x `elem` [8..11])
            then 8
            else if(x `elem` [12..15])
                then 12
                -- #Error: Out of range.
                else -1
-- #Error: Out of range.
square n x = -1
-- END: square
     
-- #mapUnavailable
-- @param [[Int]], Int, Int, Int 
-- @return [Int]
-- Returns a list of de unavailable numbers.
mapUnavailable :: [[Int]] -> Int -> Int -> Int -> [Int]
mapUnavailable board n i j = 
    (board !!i) ++ -- Unavailable numbers (Horizontal)
    [y !!j | y <- board, (y !!j) /= 0] ++ -- Unavailable numbers (Vertical)
    [[s | s <- take n (drop (square n i) ([take n (drop (square n j) z) | z <- board]))] !!h !!v 
        | h <- [0..(n-1)], v <- [0..(n-1)]] -- Unavailable numbers (Square)
-- END: mapUnavailable

-- #possibilities
-- @param [[Int]], Int, Int, Int 
-- @return [Int]
-- Returns all the posible numbers. 
possibilities :: [[Int]] -> Int -> Int -> Int -> [Int]
possibilities board n i j = [x | x <- [1..(length board)], notElem x (mapUnavailable board n i j) ]
-- END: possibilities

solve :: [[Int]] -> Int -> [[Int]]
solve board  n =
    --board doesn't need to be further solved
    if(isFull board) then
      board
    else
      [[-1]]
-- END: solve

mult :: Int -> Int -> [Int] -> [Int]
mult _ 4 xs = xs
mult pos n xs =
  if(xs !!n == 0) then
    mult pos (n+1) (take n xs ++ [head (possibilities board 0 pos n)] ++ drop (n + 1) xs)
  else
    mult pos (n+1) xs

board :: [[Int]]
board = [[0, 3, 4, 0],
         [4, 0, 0, 2],
         [1, 0, 0, 3],
         [0, 2, 1, 0]]

-- #main
main = do
    putStrLn "\nTablero original" 
    putStrLn (show board)
    putStrLn "\n possibilities"
    putStrLn (show (possibilities board 0 0 3))
    putStrLn "\nTablero Resuelto" 
    putStrLn (show [mult 0 0 (board !!0), mult 1 0 (board !!1), mult 2 0 (board !!2), mult 3 0 (board !!3)])
-- END: main