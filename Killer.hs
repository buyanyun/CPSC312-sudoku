module Killer where


data Cell = Cell {value:: Int, cage_id :: Int} deriving Show
type Row = [Cell]
type Board = [Row]

data Cage = Cage{ nosum::Int,
                  capacity::Int,
                  cells::[Int],
                  possible_num::[Int] }  deriving Show

data EntryPosition = Position Int Int | NONE
isNONE NONE           = True
isNONE (Position a b) = False
getX (Position a b) = a
--getX NONE = 8
getY (Position a b) = b
--getY NONE = 8

containsZero :: Board -> Bool
containsZero [] = False
containsZero (r:rs) | hasZero r = True
                    | otherwise  = containsZero rs

hasZero :: Row -> Bool
hasZero [] = False
hasZero (h:t) | (value h) == 0 = True
              | otherwise = hasZero t

getSMNum :: Int -> Int -> Int
getSMNum rowIdx colIdx
  | rowIdx < 3 && colIdx < 3               = 1
  | rowIdx < 3 && colIdx > 2 && colIdx < 6 = 2
  | rowIdx < 3 && colIdx > 5               = 3
  | rowIdx > 2 && rowIdx < 6 && colIdx < 3 = 4
  | rowIdx > 2 && rowIdx < 6 && colIdx > 2 && colIdx < 6 = 5
  | rowIdx > 2 && rowIdx < 6 && colIdx > 5 = 6
  | rowIdx > 5 && colIdx < 3               = 7
  | rowIdx > 5 && colIdx > 2 && colIdx < 6 = 8
  | otherwise                              = 9



getSubMatrix :: Board -> Int -> Board
getSubMatrix sudoku smNum = case smNum of
  1 -> getSubMatrixHelper sudoku 0 0
  2 -> getSubMatrixHelper sudoku 0 3
  3 -> getSubMatrixHelper sudoku 0 6
  4 -> getSubMatrixHelper sudoku 3 0
  5 -> getSubMatrixHelper sudoku 3 3
  6 -> getSubMatrixHelper sudoku 3 6
  7 -> getSubMatrixHelper sudoku 6 0
  8 -> getSubMatrixHelper sudoku 6 3
  9 -> getSubMatrixHelper sudoku 6 6
  where
  getSubMatrixHelper :: Board -> Int -> Int -> Board
  getSubMatrixHelper _ rowIdx colIdx =
    [ drop colIdx (take (colIdx + 3) (getRow sudoku (rowIdx + offset)))
    | offset <- [0 .. 2]
    ]

-- list of cell to list of Int
cell2Int :: [Cell] -> [Int]
cell2Int cells = [value x | x <- cells]

-- Return the row of sudoku at rowIdx
getRow :: Board -> Int -> [Cell]
getRow sudoku rowIdx = sudoku !! rowIdx

-- Return the column of sudoku at colIdx
getCol :: Board -> Int -> [Cell]
getCol sudoku colIdx = [ x !! colIdx | x <- sudoku ]

-- Return list of cells in cage
getCageCells :: Board -> Cage -> [Cell]
getCageCells sudoku cage = [ (sudoku !! (div x 10)) !! (mod x 10) | x <- (cells cage) ]

-- Return the cage of current slot
getCage :: Board -> [Cage] ->Int -> Int -> Cage
getCage sudoku cages row col= cages !! (cage_id (sudoku !! row !! col) )

-- Return the height of the sudoku board
getHeight :: Board -> Int
getHeight = length

-- Return the width of the sudoku board
getWidth :: Board -> Int
getWidth sudoku = length $ getRow sudoku 0

-- Return number in the slot
getNum :: Board -> Int -> Int -> Int
getNum sudoku rowIdx colIdx = value ((sudoku !! rowIdx) !! colIdx)

-- Fill the number in slot at (rowIdx, colIdx)
setNum :: Board -> Int -> Int -> Int -> Board
setNum sudoku rowIdx colIdx num =
  take rowIdx sudoku
    ++ [replace (sudoku !! rowIdx) colIdx num]
    ++ drop (rowIdx + 1) sudoku
  where
  replace :: [Cell] -> Int -> Int -> [Cell]
  replace row _ _ = take colIdx row ++ [(row !! colIdx) {value = num}] ++ drop (colIdx + 1) row



-- Removes zeros from a list
stripZero :: [Int] -> [Int]
stripZero x = filter (>0) x

-- Check if a list has unique elements
isUnique :: [Int] -> Bool
isUnique []     = True
isUnique (x:xs) = x `notElem` xs && isUnique xs

-- Check if a slot at (rowIdx, colIdx) is empty
isEmpty :: Board -> Int -> Int -> Bool
isEmpty sudoku rowIdx colIdx = 0 == getNum sudoku rowIdx colIdx

-- Check if a sudoku is valid
isValid :: Board -> [Cage] -> Bool
isValid sudoku cages= not (containsZero sudoku) && satisfiesConstraints sudoku cages

-- Check if a sudoku satisfies sudoku constraints (but can have zeroes)
satisfiesConstraints :: Board -> [Cage] -> Bool
satisfiesConstraints sudoku cages =
  and [ checkRow sudoku rowIdx | rowIdx <- [0 .. 8] ]
    && and [ checkCol sudoku colIdx | colIdx <- [0 .. 8] ]
    && and [ checkSubMatrix sudoku smNum | smNum <- [1 .. 9] ]
    && and [ checkCage sudoku cage | cage <- cages ]
  where
    checkRow :: Board -> Int -> Bool
    checkRow b r = isUnique (stripZero (cell2Int (getRow b r)))
    checkCol :: Board -> Int -> Bool
    checkCol b c = isUnique (stripZero (cell2Int(getCol b c)))
    checkSubMatrix :: Board -> Int -> Bool
    checkSubMatrix b n = isUnique (stripZero (cell2Int (concat (getSubMatrix b n))))
    checkCage :: Board -> Cage -> Bool
    checkCage b cage = isUnique (stripZero (cell2Int (getCageCells b cage))) && satisfyCage b cage

becauseImpossible :: Board -> [Cage] -> Bool
becauseImpossible sudoku cages = and [ hasImpossibleNumber sudoku (getCageCells sudoku cage) cage | cage <- cages ]

-- Return number of filled cells in cage
numberOfFilled :: Board -> [Cell] -> Int
numberOfFilled _ [] = 0
numberOfFilled sudoku (h:t)
  | value h /= 0 = 1+ numberOfFilled sudoku t
  | otherwise = numberOfFilled sudoku t

-- return sum of cells in the cage
getCageSum :: Board -> [Cell] -> Int
getCageSum _ [] = 0
getCageSum sudoku (h:t) = value h + getCageSum sudoku t

-- check if has impossible number
hasImpossibleNumber :: Board -> [Cell] -> Cage -> Bool
hasImpossibleNumber _ [] _ = False
hasImpossibleNumber sudoku (h:t) cage
  | ((value h) /= 0) && (value h) `notElem` (possible_num cage) = True
  | otherwise = hasImpossibleNumber sudoku t cage
-- check if a number satisfy cage
satisfyCage :: Board -> Cage -> Bool
satisfyCage sudoku cage
  | hasImpossibleNumber sudoku (getCageCells sudoku cage) cage = False
  | numberOfFilled sudoku (getCageCells sudoku cage) < (capacity cage ) = True
  | otherwise =  getCageSum sudoku (getCageCells sudoku cage) == nosum cage


{--- Check if a number can be filled in a slot (rowIdx,colIdx)
canFill :: Board -> Int -> Int -> Int -> [Cage]-> Bool
canFill sudoku rowIdx colIdx num cages=
  isEmpty sudoku rowIdx colIdx
    && checkRow       sudoku rowIdx colIdx num
    && checkCol       sudoku rowIdx colIdx num
    && checkSubMatrix sudoku rowIdx colIdx num
    && checkCage      sudoku rowIdx colIdx num cages
  where
  checkRow :: Board -> Int -> Int -> Int -> Bool
  checkRow _ _ _ _ = num `notElem`  cell2Int (getRow sudoku rowIdx)
  checkCol :: Board -> Int -> Int -> Int -> Bool
  checkCol _ _ _ _ = num `notElem` cell2Int (getCol sudoku colIdx)
  checkSubMatrix :: Board -> Int -> Int -> Int -> Bool
  checkSubMatrix _ _ _ _ =
    num `notElem` cell2Int (concat (getSubMatrix sudoku (getSMNum rowIdx colIdx)))
  checkCage :: Board -> Int -> Int -> Int -> [Cage]-> Bool
  checkCage sudoku row col num cages = num `elem` possible_num (getCage sudoku cages row col)  && satisfyCage sudoku num (getCage sudoku cages row col)
-}

instance Show EntryPosition where
  show (Position a b) = concat ["Position ", show a, " ", show b]
  show _ = "NONE"
instance Eq EntryPosition where
  (==) (Position a b) (Position c d) = (a == c) && (b == d)
  (==) _ _ = False



-- firstZero: finds (x,y) location of first zero, or NONE
firstZero :: Board -> EntryPosition
firstZero sudoku = firstZeroHelper sudoku 0 0

firstZeroHelper :: Board -> Int -> Int -> EntryPosition
firstZeroHelper sudoku y x
  | getNum sudoku y x == 0 = Position x y
  | (y == ((getHeight sudoku)-1)) && (x == ((getWidth sudoku)-1)) = NONE
  | otherwise = firstZeroHelper sudoku nexty nextx
  where
    nextx | x < (getWidth sudoku) - 1 = x + 1
          | otherwise                 = 0
    nexty | nextx == 0 = y + 1
          | otherwise  = y


-- last number which can be incremented
lastIncrementableEntry :: Board -> Board -> EntryPosition
lastIncrementableEntry sudoku scratch =
  lastIncrementableEntryHelper sudoku scratch (getHeight sudoku - 1) (getWidth sudoku - 1)
  where fz = firstZero scratch

lastIncrementableEntryHelper :: Board -> Board -> Int -> Int -> EntryPosition
lastIncrementableEntryHelper sudoku scratch y x
  | (x < 0) || (y < 0) = NONE
  | getNum scratch y x == 0 = next
  | getNum scratch y x == 9 = next
  | getNum sudoku y x /= 0 = next
  | otherwise = Position x y
  where nextx | x > 0 = (x - 1)
              | otherwise = getWidth sudoku - 1
        nexty | x > 0 = y
              | otherwise = y - 1
        next = lastIncrementableEntryHelper sudoku scratch nexty nextx
-- get the different position of cell in sudoku and scratch
getDiffCell :: Board -> Board ->  Int -> Int ->EntryPosition
getDiffCell sudoku scratch y x
  | getNum scratch y x == 0 = next
  | getNum sudoku y x /= 0 = Position x y
  where nextx | x > 0 = (x - 1)
              | otherwise = getWidth sudoku - 1
        nexty | x > 0 = y
              | otherwise = y - 1
        next = lastIncrementableEntryHelper sudoku scratch nexty nextx

-- Fills in all positions after the given in matrix 2 with matrix 1
replaceAfter :: Board -> Board -> Int -> Int -> Board
replaceAfter sudoku scratch y x
  | (x == getWidth scratch - 1) && (y == getHeight scratch - 1) = scratch
  | otherwise = replaceAfter sudoku (setNum scratch nexty nextx (getNum sudoku nexty nextx)) nexty nextx
  where nextx | x < getWidth scratch - 1 = x + 1
              | otherwise = 0
        nexty | nextx == 0 = y + 1
              | otherwise = y

-- Brute Force solver: fills in zeros, or returns impossible
data SudokuSolution = FoundSolution Board | UNSAT
instance Show SudokuSolution where
  show (FoundSolution sudoku) =  sudoku2Str sudoku
  show UNSAT = "Solution not found :("
instance Eq Cell where
  (==) a b = (value a == value b)
instance Eq SudokuSolution where
  (==) (FoundSolution x) (FoundSolution y) = ( x == y)
  (==) _ _ = False

sudoku2Str :: Board -> String
sudoku2Str []     = ""
sudoku2Str (r:rs) = row2Str r ++ "\n" ++ sudoku2Str rs
  where
  row2Str :: [Cell] -> String
  row2Str []     = ""
  row2Str (x:xs) = show (value x) ++ " " ++ row2Str xs

-- check if sudoku cage is valid
validCage :: [Cage] -> Bool
validCage cages = sum [nosum x | x <- cages] == 405

solve :: Board -> [Cage]-> SudokuSolution
solve sudoku cages| not (validCage cages) = UNSAT
                  | otherwise = solveHelper sudoku sudoku cages

solveHelper :: Board -> Board -> [Cage]-> SudokuSolution
solveHelper sudoku scratch cages
  | isValid scratch cages= FoundSolution scratch
  | (satisfiesConstraints scratch cages) && (matches scratch sudoku) =
      solveHelper sudoku (getMatrix nextScratch) cages
  | becauseImpossible scratch cages && ((getNum scratch (getY dc) (getX dc)) /= 9) = solveHelper sudoku (getMatrix nextScratch1) cages
  | isUNSAT backtrackScratch = UNSAT
  | otherwise = solveHelper sudoku (getMatrix backtrackScratch) cages
  where nextScratch = increment sudoku scratch False
        backtrackScratch = increment sudoku scratch True
        nextScratch1 = FoundSolution $ setNum scratch y x (getNum scratch y x + 1)
        dc = getDiffCell sudoku scratch 8 8
        y= (getY dc)
        x =(getX dc)
-- increment: checks if scratch board can be incremented given a constraining sudoku puzzle
increment :: Board -> Board -> Bool -> SudokuSolution
increment sudoku scratch backtrack
  | backtrack = FoundSolution $ incrementAt sudoku scratch (getY ln) (getX ln)
  | isNONE fz = UNSAT
  | otherwise = FoundSolution $ incrementAt sudoku scratch (getY fz) (getX fz)
  where fz = firstZero scratch
        ln = lastIncrementableEntry sudoku scratch

-- adds one to the given entry of the second matrix and
-- replaces numbers right thereof with entries from first matrix
incrementAt :: Board -> Board -> Int -> Int -> Board
incrementAt sudoku scratch y x =
  replaceAfter sudoku (setNum scratch y x (getNum scratch y x + 1)) y x




-- matches: checks that the non-zero terms agree
matches :: Board -> Board -> Bool
matches a b = not (any (\(a, b) -> ((value a) /= 0 && (value b) /= 0 && a /= b)) (zip fa fb))
  where
  fa = concat a
  fb = concat b

isUNSAT :: SudokuSolution -> Bool
isUNSAT UNSAT                  = True
isUNSAT (FoundSolution matrix) = False

getMatrix (FoundSolution matrix) = matrix




-- save time before solve call
processCages:: [Cage] -> [Cage]
processCages lst_cages = [x {possible_num = generatePossibleNum (nosum x) (capacity x)} |x<-lst_cages]
    where
    generatePossibleNum :: Int -> Int -> [Int]
    generatePossibleNum nosum capacity
        | capacity == 2 = case  nosum of 3 -> [1,2]
                                         4 -> [1,3]
                                         5 -> [1,2,3,4]
                                         6 -> [1,2,4,5]
                                         7 -> [1,2,3,4,5,6]
                                         8 -> [1,2,3,5,6,7]
                                         9 -> [1,2,3,4,5,6,7,8]
                                         10 -> [1,2,3,4,6,7,8,9]
                                         11 -> [2,3,4,5,6,7,8,9]
                                         12 -> [3,4,5,7,8,9]
                                         13 -> [4,5,6,7,8,9]
                                         14 -> [5,6,8,9]
                                         15 -> [6,7,8,9]
                                         16 -> [7,9]
                                         17 -> [8,9]
       | capacity == 3 = case nosum of 6 -> [1,2,3]
                                       7 -> [1,2,4]
                                       8 -> [1,2,3,4,5]
                                       9 -> [1,2,3,4,5,6]
                                       10 -> [1,2,3,4,5,6,7]
                                       11 -> [1,2,3,4,5,6,7,8]
                                       19 -> [2,3,4,5,6,7,8,9]
                                       20 -> [3,4,5,6,7,8,9]
                                       21 -> [4,5,6,7,8,9]
                                       22 -> [5,6,7,8,9]
                                       23 -> [6,8,9]
                                       24 -> [7,8,9]
                                       _  -> [1,2,3,4,5,6,7,8,9]
       | capacity == 4 = case nosum of 10 -> [1,2,3,4]
                                       11 -> [1,2,3,5]
                                       12 -> [1,2,3,4,5,6]
                                       13 -> [1,2,3,4,5,6,7]
                                       14 -> [1,2,3,4,5,6,7,8]
                                       26 -> [2,3,4,5,6,7,8,9]
                                       27 -> [3,4,5,6,7,8,9]
                                       28 -> [4,5,6,7,8,9]
                                       29 -> [5,7,8,9]
                                       30 -> [6,7,8,9]
                                       _  -> [1,2,3,4,5,6,7,8,9]
       | capacity == 5 = case nosum of 15 -> [1,2,3,4,5]
                                       16 -> [1,2,3,4,6]
                                       17 -> [1,2,3,4,5,6,7]
                                       18 -> [1,2,3,4,5,6,7,8]
                                       32 -> [2,3,4,5,6,7,8,9]
                                       33 -> [3,4,5,6,7,8,9]
                                       34 -> [4,6,7,8,9]
                                       35 -> [5,6,7,8,9]
                                       _  -> [1,2,3,4,5,6,7,8,9]
       | capacity == 6 = case nosum of 21 -> [1,2,3,4,5,6]
                                       22 -> [1,2,3,4,5,7]
                                       23 -> [1,2,3,4,5,6,7,8]
                                       37 -> [2,3,4,5,6,7,8,9]
                                       38 -> [3,5,6,7,8,9]
                                       39 -> [4,5,6,7,8,9]
                                       _  -> [1,2,3,4,5,6,7,8,9]
       | capacity == 7 = case nosum of 28 -> [1,2,3,4,5,6,7]
                                       29 -> [1,2,3,4,5,6,8]
                                       41 -> [2,4,5,6,7,8,9]
                                       42 -> [3,4,5,6,7,8,9]
                                       _  -> [1,2,3,4,5,6,7,8,9]
       | capacity == 8 = case nosum of 36 -> [1,2,3,4,5,6,7,8]
                                       37 -> [1,2,3,4,5,6,7,9]
                                       38 -> [1,2,3,4,5,6,8,9]
                                       39 -> [1,2,3,4,5,7,8,9]
                                       40 -> [1,2,3,4,6,7,8,9]
                                       41 -> [1,2,3,5,6,7,8,9]
                                       42 -> [1,2,4,5,6,7,8,9]
                                       43 -> [1,3,4,5,6,7,8,9]
                                       44 -> [2,3,4,5,6,7,8,9]
                                       _  -> [1,2,3,4,5,6,7,8,9]
