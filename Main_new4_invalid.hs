module Main where


import Killer
zeros :: Int -> Int -> Board
zeros w h = [ [ Cell 0 0 | x <- [1 .. w] ] | y <- [1 .. h] ]

mapBoard :: Board -> [Cage] -> Int -> Board
mapBoard sudoku [] _ = sudoku
mapBoard sudoku (h:t) id = mapBoard (markCells sudoku (cells h) id) t (id+1)

markCells :: Board -> [Int] ->Int -> Board
markCells sudoku [] id = sudoku
markCells sudoku (h:t) id = markCells (setId sudoku (div h 10) (mod h 10) id) t id



setId :: Board -> Int -> Int -> Int -> Board
setId sudoku rowIdx colIdx id =
  take rowIdx sudoku
    ++ [replace (sudoku !! rowIdx) colIdx id]
    ++ drop (rowIdx + 1) sudoku
  where
  replace :: [Cell] -> Int -> Int -> [Cell]
  replace row _ _ = take colIdx row ++ [(row !! colIdx) {cage_id = id}] ++ drop (colIdx + 1) row

createCage :: Int -> Int -> [Int] -> Cage
createCage a b lst = Cage a b lst [1,2,3,4,5,6,7,8,9]



main = do
putStrLn "Test invalid"
let c1 = createCage 100 3 [0,10,11]
let c2 = createCage 11 2 [1,2]
let c3 = createCage 10 2 [3,4]
let c4 = createCage 8 2 [5,6]
let c41 = createCage 11 2 [7,8]
let c5 = createCage 13 2 [12,13]
let c6 = createCage 15 2 [14,15]
let c7 = createCage 6 2 [16,26]
let c8 = createCage 17 3 [17,18,27]
let c9 = createCage 14 2 [20,21]
let c10 = createCage 15 3 [22,23,33]
let c11 = createCage 6 2 [24,34]
let c12 = createCage 12 2 [25,35]
let c13 = createCage 9 2 [28,38]
let c14 = createCage 17 3 [30,40,50]
let c15 = createCage 3 2 [31,32]
let c16 = createCage 17 2 [36,46]
let c17 = createCage 13 2 [37,47]
let c18 = createCage 15 3 [41,51,61]
let c19 = createCage 21 3 [42,43,52]
let c20 = createCage 10 2 [44,45]
let c21 = createCage 10 2 [48,58]
let c22 = createCage 12 3 [53,54,55]
let c23 = createCage 7 2 [56,57]
let c24 = createCage 8 3 [60,70,71]
let c25 = createCage 17 3 [62,72,82]
let c26 = createCage 15 2 [63,64]
let c27 = createCage 9 3 [65,66,67]
let c28 = createCage 10 2 [73,83]
let c29 = createCage 11 3 [74,75,84]
let c30 = createCage 6 2 [76,77]
let c31 = createCage 17 2 [78,88]
let c32 = createCage 16 2 [80,81]
let c33 = createCage 15 3 [85,86,87]
let cages = c1:c2:c3:c4:c41:c5:c6:c7:c8:c9:c10:c11:c12:c13:c14:c15:c16:c17:c18:c19:c20:
          c21:c22:c23:c24:c25:c26:c27:c28:c29:c30:c31:c32:c33:[]
let zero_sudoku = zeros 9 9
let mapped_sudoku = mapBoard zero_sudoku cages 0
let processed_cages = processCages cages
putStrLn "The solution is:"
print (solve mapped_sudoku processed_cages)
