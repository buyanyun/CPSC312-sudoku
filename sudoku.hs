-- sudoku.hs take care the state of sudoku.hs 
-- It will focus whether the enter value restrict the rule of sudoku

data State = State InternalState
      deriving (Ord, Eq, Show)

data Result = EndOfGame String State  -- message_of_end_game end_game_state
            | ContinueGame String State  __ message_of_continue_game continue_game_state
      deriving (Eq, show)

type Game = State -> Result

type InternalState = [[Int]]

sudoku :: Game
sudoku state move
    | checknoempty state move          = ContinueGame "illegal Enter" state
    | checkboxillegal state move  = ContinueGame "illegal Enter" state
    | checkcolumnillegal state move    = ContinueGame "illegal Enter" state
    | checkrowillegal state move       = ContinueGame "illegal Enter" state
    | checkdiagonalillegal state move  = ContinueGame "illegal Enter" state
    | otherwise                        = changestatus state move

checknoempty (State internal) (r:c:v:t) = internal!!r!!c != 0
checkrowillegal (State internal) (r:c:v:t) = elem v internal!!r
checkcolumnillegal (State []) = False
checkcolumnillegal (State (sh:st)) (r:c:v:t)
    | sh!!c == v = True
    | otherwise = checkcolumnillegal (State st) (r:c:v:t)
checkdiagonalillegal (State internal) (r:c:v:t)
    | r != c = False
    | otherwise = elem v checkdiagonalillegalhelper internal 0
checkdiagonalillegalhelper lst n =
    | n == 9 = []
    | otherwise lst!!n!!n : checkdiagonalillegalhelper lst n+1
checkboxillegal (State internal) (r:c:v:t) =
    | if r<=2 && c<=2                 = elem v checkboxillegalhelper 0 2 0 2 internal
    | if r<=2 && c>=3 && c<=5         = elem v checkboxillegalhelper 0 2 3 5 internal
    | if r<=2 && c>=6                 = elem v checkboxillegalhelper 0 2 6 8 internal
    | if r>=3 && r<=5 && c<=2         = elem v checkboxillegalhelper 3 5 0 2 internal
    | if r>=3 && r<=5 && c>=3 && c<=5 = elem v checkboxillegalhelper 3 5 3 5 internal
    | if r>=3 && r<=5 && c>=6         = elem v checkboxillegalhelper 3 5 6 8 internal
    | if r>=6 && c<=2                 = elem v checkboxillegalhelper 6 8 0 2 internal
    | if r>=6 && c>=3 && c<=5         = elem v checkboxillegalhelper 6 8 3 5 internal
    | otherwise                       = elem v checkboxillegalhelper 6 8 6 8 internal
checkboxillegalhelper r1 r2 c1 c2 lst = (checkboxillegalhelperCol c1 c2 lst!!r1)++(checkboxillegalhelperCol c1 c2 lst!!(r1+1))++(checkboxillegalhelperCol c1 c2 lst!!r2)

checkboxillegalhelperCol c1 c2 lst
    | c1 > c2 = []
    | lst!!c1 != 0 = lst!!c1 : checkboxillegalhelperCol c1+1 c2 lst
    | otherwise = checkboxillegalhelperCol c1+1 c2 lst

changestatus :: Game
changestatus (State internal) (r:c:v:t) = checkWin ContinueGame "update" (State (replace internal r c v t))

replace :: [[Int]] -> Int -> Int -> Int -> [[Int]]
replace (h:t) r c v
    | r == 0 = (replaceCol h c v):t
    | otherwise = h:(replace t r-1 c v)

replaceCol :: [Int] -> Int -> Int -> [[Int]]
replaceCol (h:t) c v
    | c == v = v:t
    | otherwise h:(replaceCol t c-1 v)

sudoku_start = State take 9 repeat((take 9 (repeat 0)))
