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
  let c1 = createCage 14 2 [0,10] 
  let c2 = createCage 11 2 [1,11] 
  let c3 = createCage 16 3 [2,3,4] 
  let c4 = createCage 14 2 [5,6] 
  let c5 = createCage 9 3 [7,8,18] 
  let c6 = createCage 8 3 [12,13,22] 
  let c7 = createCage 9 2 [14,15]
  let c8 = createCage 12 2 [16,17] 
  let c9 = createCage 9 2 [20,21] 
  let c10 = createCage 11 2 [23,24]
  let c11 = createCage 15 2 [25,35] 
  let c12 = createCage 8 2 [26,36] 
  let c13 = createCage 15 3 [27,28,37] 
  let c14 = createCage 12 2 [30,31] 
  let c15 = createCage 5 2 [32,33] 
  let c16 = createCage 18 3 [34,44,45] 
  let c17 = createCage 8 2 [38,48] 
  let c18 = createCage 4 2 [40,41]
  let c19 = createCage 15 2 [42,43] 
  let c20 = createCage 19 3 [46,47,56] 
  let c21 = createCage 16 3 [50,51,52] 
  let c22 = createCage 14 3 [53,63,64] 
  let c23 = createCage 9 2 [54,55] 
  let c24 = createCage 12 3 [57,58,67] 
  let c25 = createCage 14 3 [60,70,80] 
  let c26 = createCage 14 3 [61,62,71] 
  let c27 = createCage 4 2 [65,66] 
  let c28 = createCage 15 2 [68,78] 
  let c29 = createCage 15 3 [72,73,74] 
  let c30 = createCage 16 3 [75,76,77] 
  let c31 = createCage 15 2 [81,82] 
  let c32 = createCage 14 3 [83,84,85] 
  let c33 = createCage 15 3 [86,87,88] 
     
  let cages = c1:c2:c3:c4:c5:c6:c7:c8:c9:c10:c11:c12:c13:c14:c15:c16:c17:c18:c19:c20:
            c21:c22:c23:c24:c25:c26:c27:c28:c29:c30:c31:c32:c33:[]
  let zero_sudoku = zeros 9 9
  let mapped_sudoku = mapBoard zero_sudoku cages 0
  let processed_cages = processCages cages
  putStrLn "The solution is:"
  print (solve mapped_sudoku processed_cages)
