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
  putStrLn "Test 2 Puzzle: 17259"
  let c1 = createCage 11 2 [0,1]
  let c2 = createCage 39 8 [2,3,4,5,6,7,12,13]
  let c3 = createCage 12 2 [8,18]
  let c4 = createCage 9 2 [10,11]
  let c41 = createCage 9 2 [14,15]
  let c5 = createCage 18 3 [16,17,27]
  let c6 = createCage 24 4 [20,21,30,31]
  let c7 = createCage 6 2 [22,32]
  let c8 = createCage 12 3 [23,24,33]
  let c9 = createCage 10 2 [25,26]
  let c10 = createCage 8 3 [28,38,48]
  let c11 = createCage 8 3 [34,44,54]
  let c12 = createCage 22 3 [35,36,37]
  let c13 = createCage 13 3 [40,50,60]
  let c14 = createCage 11 3 [41,42,43]
  let c15 = createCage 21 3 [45,46,47]
  let c16 = createCage 18 3 [51,52,53]
  let c17 = createCage 12 3 [55,64,65]
  let c18 = createCage 13 2 [56,66]
  let c19 = createCage 19 4 [57,58,67,68]
  let c20 = createCage 16 3 [61,71,72]
  let c21 = createCage 15 2 [62,63]
  let c22 = createCage 3 2 [70,80]
  let c23 = createCage 11 2 [73,74]
  let c24 = createCage 41 8 [75,76,81,82,83,84,85,86]
  let c25 = createCage 15 2 [77,78]
  let c26 = createCage 9 2 [87,88]
  let cages = c1:c2:c3:c4:c41:c5:c6:c7:c8:c9:c10:c11:c12:c13:c14:c15:c16:c17:c18:c19:c20:
            c21:c22:c23:c24:c25:c26:[]
  let zero_sudoku = zeros 9 9
  let mapped_sudoku = mapBoard zero_sudoku cages 0
  let processed_cages = processCages cages
  putStrLn "The solution is:"
  print (solve mapped_sudoku processed_cages)


