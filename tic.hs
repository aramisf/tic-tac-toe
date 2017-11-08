{-
 - Tic Tac Toe
 -}

-- Using exception and error control
import Control.Exception
import System.IO.Error
import System.IO
import System.Process

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
                        putStrLn ("\nStarting game: \"" ++ p1 ++ " vs " ++ p2 ++ " ...")
                        putStrLn "Squares with no numbers are not marked"
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
                        putStrLn ("Player " ++ p1 ++ " will be the \'X\', player " ++ p2 ++
                                  " will the the \'O\'. Go!")
                        -- turn 0 -> player's 1 turn
                        -- turn 1 -> player's 2 turn
                        putStr "Press <enter> key to continue... "
                        getChar
                        runGame myData initialBoard p1 p2 0

runGame :: Players -> Table -> Name -> Name -> Int -> IO Players
runGame myData board p1 p2 turn = menu myData
