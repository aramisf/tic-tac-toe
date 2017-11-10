{-
 - Tic Tac Toe
 -}

-- Using exception and error control
import Control.Exception
import System.IO.Error
import System.IO
import System.Process
import Data.List
import Data.Function

-- Definition of my data types
type Players = [Player] -- all registered users
type Name = String
type Score = Int
type Turn = Int
type Table = [Char]

data Player = Player Name Score
                deriving (Show, Read)

initialBoard = ['1', '2', '3', '4', '5', '6', '7', '8', '9']

getString :: String -> IO String
getString str = do
                  putStr str
                  answer <- getLine
                  return answer


-- Start the program
start :: IO()
start = do
          {catch (read_file) handle_error}
          where
            read_file = do
            {
              f <- openFile "data.txt" ReadMode;
              myData <- hGetLine f;
              hClose f;
              menu (read myData);
              return ()
            }
            handle_error err = if isDoesNotExistError err then do {
              -- file does not exist, create one
              myData <- openFile "data.txt" WriteMode;
              hPutStrLn myData "[]"; -- adds an empty list to the file
              hClose myData;
              menu [];
              return ()
            }
            else ioError err

-- displays the menu
menu :: Players -> IO Players
menu myData = do
              system "clear" -- unix like systems
              putStrLn "------------------------- Tic Tac Toe -------------------------"
              putStrLn "\nType 1 to register player"
              putStrLn "Type 2 to play"
              putStrLn "Type 3 to print ranking"
              putStrLn "Type 0 to exit"
              putStr "Your choice: "
              opt <- getChar
              getChar -- flush keyboard buffer
              executeOpt myData opt
              return myData


executeOpt :: Players -> Char -> IO Players
executeOpt myData '1' = registerPlayer myData
executeOpt myData '2' = prepareGame myData
executeOpt myData '3' = do
                          putStrLn "Ranking:"
                          if (null myData) then do
                            putStrLn "No records to show!"
                          else do
                            showRanking (reverse (sortData myData))
                          putStrLn "Press <enter> to return to the menu..."
                          getChar
                          menu myData

executeOpt myData '0' = do
                    putStrLn "Exiting..."
                    return myData

executeOpt myData _ = do
                        putStr "Invalid option. Press <enter> to continue..."
                        getChar
                        menu myData


registerPlayer :: Players -> IO Players
registerPlayer myData = do
                        name <- getString "\nType in a user name: "
                        if (playerExists myData name) then do
                          putStrLn "Name already taken, choose another one!"
                          putStr "Press <enter> to continue... "
                          getChar
                          menu myData
                        else do
                          arq <- openFile "data.txt" WriteMode
                          hPutStrLn arq (show ((Player name 0):myData))
                          hClose arq
                          putStrLn ("\nUser " ++ name ++ " successfully registered.")
                          putStr "Press <enter> to continue... "
                          getChar
                          menu ((Player name 0):myData)

playerExists :: Players -> String -> Bool
playerExists [] _ = False
playerExists ((Player n s):xs) name | (n == name) = True
                                    | otherwise = playerExists xs name


-- prepare the inital data for the game
prepareGame :: Players -> IO Players
prepareGame myData = do
                      p1 <- getString "\nType the name of the first player: "
                      -- test if given name corresponds to a registered user
                      if not (playerExists myData p1) then do
                        putStrLn "\nThis player doesn't exist, bitch!"
                        putStr "Press <enter> key to continue... "
                        getChar
                        menu myData
                      else do
                        p2 <- getString "\nType the name of the second player: "
                        if not (playerExists myData p2) then do
                          putStrLn "\nThis player doesn't exist, bitch!"
                          putStr "Press <enter> key to continue... "
                          getChar
                          menu myData
                        else do
                          -- alright, both players correctly given
                          newGame myData p1 p2

-- Starts a new game
newGame :: Players -> Name -> Name -> IO Players
newGame myData p1 p2 = do
                        putStrLn ("\nStarting game: \"" ++ p1 ++ " vs " ++ p2 ++ "\" ...")
                        putStrLn "Squares with no numbers are not marked"
                        putStrLn ("Player " ++ p1 ++ " will be the \'X\', player " ++ p2 ++
                                  " will the the \'O\'. Go!")
                        {-
                          This:
                           ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

                          Iis the same as:
                            1 | 2 | 3
                            ---------
                            4 | 5 | 6
                            ---------
                            7 | 8 | 9
                         -}
                        -- turn 0 -> player's 1 turn
                        -- turn 1 -> player's 2 turn
                        runGame myData initialBoard p1 p2 0

runGame :: Players -> Table -> Name -> Name -> Turn -> IO Players
runGame myData board p1 p2 turn = do
                      putStrLn ("\n" ++ "                              " ++
                        (show (board !! 0)) ++ " | " ++ (show (board !! 1)) ++ " | " ++ (show (board !! 2)) ++
                        "\n                              ---------------\n                              " ++
                        (show (board !! 3)) ++ " | " ++ (show (board !! 4)) ++ " | " ++ (show (board !! 5)) ++
                        "\n                              ---------------\n                              " ++
                        (show (board !! 6)) ++ " | " ++ (show (board !! 7)) ++ " | " ++ (show (board !! 8)) ++
                        "\n")

                      -- check if p1 is the winner
                      if (winnerIsP1 board) then do
                        putStrLn (p1 ++ " -> congrats bitch! You won!")

                        -- save
                        file2write <- openFile "data.txt" WriteMode
                        hPutStrLn file2write (show (updateScore myData p1))
                        hClose file2write

                        -- update memory data
                        file2read <- openFile "data.txt" ReadMode
                        updatedData <- hGetLine file2read
                        hClose file2read

                        putStrLn "Press <enter> to continue..."
                        getChar
                        menu (read updatedData)

                      else do
                        -- check if p2 is the winner
                        if (winnerIsP2 board) then do
                          putStrLn (p2 ++ " -> congrats bitch! You won!")

                          -- save
                          file2write <- openFile "data.txt" WriteMode
                          hPutStrLn file2write (show (updateScore myData p2))
                          hClose file2write

                          -- update memory data
                          file2read <- openFile "data.txt" ReadMode
                          updatedData <- hGetLine file2read
                          hClose file2read

                          putStrLn "Press <enter> to continue..."
                          getChar
                          menu (read updatedData)

                        else do
                          -- check if there was a tie
                          -- tie happens when the intersection between the board
                          -- and the string "123456789"
                          if (length (intersect "123456789" board) == 0) then do
                            -- tie
                            putStrLn "Yo bitches, you got a tie! xD"
                            putStrLn "Press <enter> to continue..."
                            getChar
                            menu myData
                          else do
                            -- keep playing bitch. Check who's turn is it
                            if (turn == 0) then do
                              putStr (p1 ++ " it is your turn. Which cell would you like to choose? ")
                              op <- getChar
                              getChar

                              if not (elem op "123456789") then do
                                putStrLn "Invalid choice. Try again ...(press <enter>, bitch!)"
                                runGame myData board p1 p2 0 -- still p1's turn

                              else do
                                if not (elem op board) then do
                                  putStrLn "Option already taken, please choose another one"
                                  runGame myData board p1 p2 0 -- still p1's turn
                                else do
                                  -- choice is ok, update board and go to p2's
                                  -- turn
                                  runGame myData (updateBoard board turn op) p1 p2 1 -- now go for p2's turn

                            else do -- turn != 0
                              putStr (p2 ++ " it is your turn. Which cell would you like to choose? ")
                              op <- getChar
                              getChar

                              if not (elem op "123456789") then do
                                putStrLn "Invalid choice. Try again ...(press <enter>, bitch!)"
                                runGame myData board p1 p2 1 -- still p2's turn

                              else do
                                if not (elem op board) then do
                                  putStrLn "Option already taken, please choose another one"
                                  runGame myData board p1 p2 1 -- still p2's turn
                                else do
                                  -- choice is ok, update board and go to p1's
                                  -- turn
                                  runGame myData (updateBoard board turn op) p1 p2 0 -- now go for p1's turn


updateBoard :: Table -> Turn -> Char -> Table
updateBoard (x:xs) turn e | ((x == e) && (turn == 0)) = (['X'] ++ xs)
                          | ((x == e) && (turn == 1)) = (['O'] ++ xs)
                          | otherwise = x:(updateBoard xs turn e)


winnerIsP1 :: Table -> Bool
winnerIsP1 board
  -- rows
  | (((board !! 0) == 'X') && ((board !! 1) == 'X') && ((board !! 2) == 'X')) = True
  | (((board !! 3) == 'X') && ((board !! 4) == 'X') && ((board !! 5) == 'X')) = True
  | (((board !! 6) == 'X') && ((board !! 7) == 'X') && ((board !! 8) == 'X')) = True
  -- columns
  | (((board !! 0) == 'X') && ((board !! 3) == 'X') && ((board !! 6) == 'X')) = True
  | (((board !! 1) == 'X') && ((board !! 4) == 'X') && ((board !! 7) == 'X')) = True
  | (((board !! 2) == 'X') && ((board !! 5) == 'X') && ((board !! 8) == 'X')) = True
  -- diagonals
  | (((board !! 0) == 'X') && ((board !! 4) == 'X') && ((board !! 8) == 'X')) = True
  | (((board !! 2) == 'X') && ((board !! 4) == 'X') && ((board !! 6) == 'X')) = True
  -- other
  | otherwise = False

winnerIsP2 :: Table -> Bool
winnerIsP2 board
  -- rows
  | (((board !! 0) == 'O') && ((board !! 1) == 'O') && ((board !! 2) == 'O')) = True
  | (((board !! 3) == 'O') && ((board !! 4) == 'O') && ((board !! 5) == 'O')) = True
  | (((board !! 6) == 'O') && ((board !! 7) == 'O') && ((board !! 8) == 'O')) = True
  -- columns
  | (((board !! 0) == 'O') && ((board !! 3) == 'O') && ((board !! 6) == 'O')) = True
  | (((board !! 1) == 'O') && ((board !! 4) == 'O') && ((board !! 7) == 'O')) = True
  | (((board !! 2) == 'O') && ((board !! 5) == 'O') && ((board !! 8) == 'O')) = True
  -- diagonals
  | (((board !! 0) == 'O') && ((board !! 4) == 'O') && ((board !! 8) == 'O')) = True
  | (((board !! 2) == 'O') && ((board !! 4) == 'O') && ((board !! 6) == 'O')) = True
  -- otherwise
  | otherwise = False

-- updateScore myData p1
updateScore :: Players -> Name -> Players
updateScore ((Player name score): xs) winner
  | (name == winner) = [(Player name (score + 1))] ++ xs
  | otherwise = (Player name score):(updateScore xs winner)


showRanking :: Players -> IO ()
showRanking [] = return ()
showRanking (x:xs) = do
                      putStrLn ((getName x) ++ "'s score is: " ++ (show (getScore x)))
                      showRanking xs


getName :: Player -> Name
getName (Player name _) = name

getScore :: Player -> Score
getScore (Player _ score) = score

sortData :: Players -> Players
sortData players = sortBy (compare `on` getScore) players
