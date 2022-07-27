
  import Data.List
  import Data.Char

  data Move = X | O deriving (Show, Eq)
  data Cell = Taken Move | Empty deriving (Eq)
  data UpdatedBoard = Success [Cell] | Fail String  [Cell]
  data GameStatus = Win Move | Tie | NotOver deriving (Show ,Eq)

--   data Row = R1 | R2 | R3
--   data Col  = C1 | C2 | C3

--   type Board = Map (Row, Col) Cell 

--   instance Eq GameStatus where
--     Win X == Win X = True
--     Win O == Win O = True
--     Tie == Tie = True
--     NotOver == NotOver = True 
--     _ == _ = False



  instance Show Cell where
   show (Taken X)     = "X"
   show (Taken O)     = "O"
   show Empty         = " "

--   instance Eq Cell where
--     Taken X == Taken X = True
--     Taken O == Taken O = True
--     Empty == Empty           = True
--     _ == _                   = False

  nextMove :: Move -> Move
  nextMove X = O
  nextMove O = X

  printRow :: [Cell] -> String
  printRow row = intercalate " | "  (map show row)

  spaceBetweenRows :: String
  spaceBetweenRows = "----------"

  printBoard :: [Cell] -> IO ()
  printBoard board = do
    putStrLn  (printRow (take 3 board)) --firstRow
    putStrLn spaceBetweenRows
    putStrLn  (printRow ((drop 3 . take 6)  board)) --secondRow
    putStrLn spaceBetweenRows
    putStrLn  (printRow (drop 6 board))  --ThirdRow
    

  takeCellPosition :: String -> Maybe Int
  takeCellPosition "0" = Just 0
  takeCellPosition "1" = Just 1
  takeCellPosition "2" = Just 2
  takeCellPosition "3" = Just 3
  takeCellPosition "4" = Just 4
  takeCellPosition "5" = Just 5
  takeCellPosition "6" = Just 6
  takeCellPosition "7" = Just 7
  takeCellPosition "8" = Just 8
  takeCellPosition _ = Nothing

  checkIfCellAvailable ::  [Cell] -> Int -> Maybe Int
  checkIfCellAvailable board cellPosition = if board !! cellPosition == Empty then Just cellPosition else Nothing

  

--   assignCellPosition :: String -> Move -> [Cell] -> UpdatedBoard
--   assignCellPosition location move board =
--     case  checkIfCellAvailable board (takeCellPosition location) of
--       Nothing -> Fail  board
--       Just i -> Success ((take i board) ++ [Taken move] ++ (drop (i+1) board))


  assignCellPosition :: String -> Move -> [Cell] -> UpdatedBoard
  assignCellPosition location move board =
    case takeCellPosition location >>= checkIfCellAvailable board of
      Nothing -> Fail "This is not valid move dear player!!"  board
      Just i -> Success ((take i board) ++ [Taken move] ++ (drop (i+1) board))
 
  --monad for Maybe is
  
--   mx >>= f == case mx of 
--                Nothing -> Nothing
--                Just n - > f n 


--   winning :: Move -> [Cell] -> Bool
--   winning move board =   -- or will take list of Bool and return OR operation on them .
--     or [
--       -- check top row or [0,1,2]
--       board !! 0 == (Taken move) && board !! 1 == (Taken move) && board !! 2 == (Taken move),
--       -- check middle row or [3,4,5]
--       board !! 3 == (Taken move) && board !! 4 == (Taken move) && board !! 5 == (Taken move),
--       -- check bottom row or [6,7,8]
--       board !! 6 == (Taken move) && board !! 7 == (Taken move) && board !! 8 == (Taken move),
--       -- check left column or [0,3,6]
--       board !! 0 == (Taken move) && board !! 3 == (Taken move) && board !! 6 == (Taken move),
--       -- check middle column or [1,4,7]
--       board !! 1 == (Taken move) && board !! 4 == (Taken move) && board !! 7 == (Taken move),
--       -- check right column or [2,5,8]
--       board !! 2 == (Taken move) && board !! 5 == (Taken move) && board !! 8 == (Taken move),
--       -- check top left to bottom right  or [0,4,8]
--       board !! 0 == (Taken move) && board !! 4 == (Taken move) && board !! 8 == (Taken move),
--       -- check bottom left to top right  or [6,4,2]
--       board !! 6 == (Taken move) && board !! 4 == (Taken move) && board !! 2 == (Taken move)
--     ]
   


--   winning :: [Cell] -> String
--   winning board 
--           | board !! 0 == board !! 1 && board !! 1 == board !! 2 && board !! 0 /= Empty = "winner is " ++ show (board !! 0)
--           | board !! 3 == board !! 4 && board !! 4 == board !! 5 && board !! 3 /= Empty = "winner is " ++ show (board !! 3)
--           | board !! 6 == board !! 7 && board !! 7 == board !! 8 && board !! 6 /= Empty = "winner is " ++ show (board !! 6)
--           | board !! 0 == board !! 3 && board !! 3 == board !! 6 && board !! 0 /= Empty = "winner is " ++ show (board !! 0)
--           | board !! 1 == board !! 4 && board !! 4 == board !! 7 && board !! 1 /= Empty = "winner is " ++ show (board !! 1)
--           | board !! 2 == board !! 5 && board !! 5 == board !! 8 && board !! 2 /= Empty = "winner is " ++ show (board !! 2)
--           | board !! 0 == board !! 4 && board !! 4 == board !! 8 && board !! 0 /= Empty = "winner is " ++ show (board !! 0)
--           | board !! 6 == board !! 4 && board !! 4 == board !! 2 && board !!  6/= Empty = "winner is " ++ show (board !! 6)
--           | Empty `elem` board = "Game not over"
--           | otherwise = "Tie" 


  winning :: Move -> [Cell] -> GameStatus
  winning move board 
               | board !! 0 == board !! 1 && board !! 1 == board !! 2  && board !! 0 /= Empty  = Win move
               | board !! 3 == board !! 4 && board !! 4 == board !! 5  && board !! 3 /= Empty  = Win move
               | board !! 6 == board !! 7 && board !! 7 == board !! 8  && board !! 6 /= Empty  = Win move
               | board !! 0 == board !! 3 && board !! 3 == board !! 6  && board !! 0 /= Empty  = Win move
               | board !! 1 == board !! 4 && board !! 4 == board !! 7  && board !! 1 /= Empty  = Win move
               | board !! 2 == board !! 5 && board !! 5 == board !! 8  && board !! 2 /= Empty  = Win move
               | board !! 0 == board !! 4 && board !! 4 == board !! 8  && board !! 0 /= Empty  = Win move
               | board !! 6 == board !! 4 && board !! 4 == board !! 2  && board !! 6 /= Empty  = Win move
               | Empty `elem` board  = NotOver
               | otherwise = Tie
   
-- !! is unsafe function because it does not return Maybe.
-- Total function example are , Avoid Partial function . 
  letUsPlayGame :: Move  -> [Cell] -> IO ()
  letUsPlayGame move board = do
    putStrLn  ((show move) ++ " 's turn.")
    putStrLn  ("Pick a cell from 0 to 8 .")
    printBoard board
    cellPosition <- getLine
    case assignCellPosition cellPosition move board of
      Fail errMsg board -> do
        putStrLn errMsg
        letUsPlayGame move board
      Success board -> do
        if (winning move board == (Win move)) then do
          putStrLn  ("Winner! is  " ++ (show move))
          printBoard board
        else if (winning move board == NotOver) then do
          putStrLn ("Game is not over")
          letUsPlayGame (nextMove move) board
        else if (winning move board == Tie) then do
          putStrLn ("Game is Tie")
          printBoard board
        else letUsPlayGame (nextMove move) board        

        
       
          
          
        
        
        
    
      
          
        

  main :: IO ()
  main = do
    putStrLn  ("Starting Game.")
    letUsPlayGame X [Empty , Empty , Empty , Empty , Empty, Empty , Empty , Empty , Empty ]
    
    