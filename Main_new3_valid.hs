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
putStrLn "Test 2 Puzzle: wikipedia example"
let cnew1 = createCage 3 2 [0,1] 
let cnew2 = createCage 15 3 [2,3,4] 
let cnew3 = createCage 22 4 [5,14,15,24] 
let cnew4 = createCage 4 2 [6,16] 
let cnew5 = createCage 16 2 [7,17] 
let cnew6 = createCage 15 4 [8,18,28,38]
let cnew7 = createCage 25 4 [10,11,20,21] 
let cnew8 = createCage 17 2 [12,13] 
let cnew9 = createCage 9 3 [22,23,33] 
let cnew10 = createCage 8 3 [25,35,45]
let cnew11 = createCage 20 3 [26,27,36] 
let cnew12 = createCage 6 2 [30,40]
let cnew13 = createCage 14 2 [31,32] 
let cnew14 = createCage 17 3 [34,44,54] 
let cnew15 = createCage 17 3 [37,46,47] 
let cnew16 = createCage 13 3 [41,42,51] 
let cnew17 = createCage 20 3 [43,53,63] 
let cnew18 = createCage 12 2 [48,58] 
let cnew19 = createCage 27 4 [50,60,70,80] 
let cnew20 = createCage 6 3 [52,61,62] 
let cnew21 = createCage 20 3 [55,65,66] 
let cnew22 = createCage 6 2 [56,57] 
let cnew23 = createCage 10 4 [64,73,74,83] 
let cnew24 = createCage 14 4 [67,68,77,78] 
let cnew25 = createCage 8 2 [71,81]  
let cnew26 = createCage 16 2 [72,82] 
let cnew27 = createCage 15 2 [75,76] 
let cnew28 = createCage 13 3 [84,85,86] 
let cnew29 = createCage 17 2 [87,88] 
     
let cages = cnew1:cnew2:cnew3:cnew4:cnew5:cnew6:cnew7:cnew8:cnew9:cnew10:
						cnew11:cnew12:cnew13:cnew14:cnew15:cnew16:cnew17:cnew18:cnew19:cnew20:
						cnew21:cnew22:cnew23:cnew24:cnew25:cnew26:cnew27:cnew28:cnew29:[]
let zero_sudoku = zeros 9 9
let mapped_sudoku = mapBoard zero_sudoku cages 0
let processed_cages = processCages cages
putStrLn "The solution is:"
print (solve mapped_sudoku processed_cages)
