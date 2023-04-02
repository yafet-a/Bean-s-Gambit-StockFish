module Main where

import Bean.Types
import Bean.Game (startingPos, makeMove, gameIsWon, gameIsDrawn)
import System.Environment (getArgs)
import Data.Maybe (mapMaybe)
import Data.List.Split (splitOn, chunksOf)


{-
I decided to use a mapping to visually see for myself when testing
 that the coordinates were being parsed correctly. -}

-- Parses a move string into a pair of coordinates.
parseMove :: String -> Maybe (Coord, Coord)
parseMove "#" = Nothing  -- special case for end of game
parseMove "D" = Nothing  -- special case for draw
parseMove m = let
    --I defined a mapping of the coordinates to the board.
    --This also made it easy to check my ouputs were correct without
    --having to open the coursework spec.continually
    mapping = [("a1",(0,3)),("a2",(0,2)),("a3",(0,1)),("a4",(0,0))
              ,("b1",(1,3)),("b2",(1,2)),("b3",(1,1)),("b4",(1,0))
              ,("c1",(2,3)),("c2",(2,2)),("c3",(2,1)),("c4",(2,0))
              ,("d1",(3,3)),("d2",(3,2)),("d3",(3,1)),("d4",(3,0))]
    --I used the lookup function to get the coordinates from the mapping
    getCoord str = lookup str mapping
    --Initially my code only worked for moves of the form "a1b2" or "b2a1"
    --This led me to using the splitOn function from Data.List.Split to split the move on the x
    splitMoves = splitOn "x" m
    in case length splitMoves of
    --I then used mapMaybe to get the coordinates from the mapping.
        2 -> case mapMaybe getCoord splitMoves of
                [from, to] -> Just (from, to)
                _ -> Nothing
    --Below is the conidition for when the move is of the form a1b2 etc (without the x)

        _ -> let chunks = chunksOf 2 m
             in case (mapMaybe getCoord chunks, mapMaybe getCoord chunks) of
                    ([from, to], _) -> Just (from, to)
                    _ -> Nothing



{-
[In order to apply the parsed moves onto the boards I created this function.
I began by pattern matching on the moves argument. If it were empty, return an empty list.
For anything else, applied foldl to the tail. I chose foldl because it was easier to implement
with the accumulator and was more readable. I also made this more readable by using
a lambda function to take in the arguments of an accumulator and a move (an element from
the parsedMoves list). Initially the playMoves function kept returning empty boards because
I had used validity checkers in here to check whether something was valid. However, by using the
makeMove function over setCoord, I didnt have to worry about that since makeMove has the helper function
isLegal to do that for me. This made the code quite short here. If the move is parsed from
the parseMove function, it feeds thise coordinates into the makeMove function and that to
the last element of acc (which we want because it means the moves keep getting applied to the recent
boards). If the move can't be parsed, it returns the accumulator without changing it.]
-}

-- Plays a sequence of moves on a list of boards, returning the sequence of resulting boards.
playMoves :: Board -> [String] -> [Board]
playMoves b moves = case moves of
    [] -> []
    _  -> foldl (\acc move -> case parseMove move of
        Just (from, to) -> case makeMove (last acc) from to of
            Just board -> acc ++ [board]
            Nothing -> acc
        Nothing -> acc) [b] (tail moves)


{- 
[allows the contents of the file thats put as an argument to become the string of words
that ends up being parsed. I used the words function to do this, since in bgn notation, every new move is
separated by a " ". It also applies the playMoves function to the startingPos.
This is needed because startingPos is going to be in every board so it is always whats applied to playMoves.
The line putStrLn $ "\n Move history: " ++ show moves was actually a line I used to debug my parseMove function.
However, I decided to keep it in because I thought it was useful to see the moves that were played anyways.]
-}
--Allows me to take the filepath as an argument and then read the file.
playFromFile :: FilePath -> IO [Board]
playFromFile file = do
    contents <- readFile file
    let moves = words contents
    let boards = playMoves startingPos moves
    putStrLn $ "\n Move history: " ++ show moves

    if null boards
        then do
            putStrLn "No valid moves found"
            return []
        else do
            putStrLn "\nBegin Battle!\n"
            putStrLn "\nMove 0: Starting Board \n"
            return boards


-- Returns the player who is not the given player. (This is useful in printBoards.)
opponent :: Player -> Player
opponent p = case p of
  RedPlayer -> BluePlayer
  BluePlayer -> RedPlayer


{-
[This function is used to print the boards and the state of the game and its outcome
The end of turn I had to make to calculate the move number. I had to do this because I kept getting
issues with the move number being off by one. I also had to make a special case for when the game was won.
The gameIsWon functionfrom Game.hs made this simple. Initially I was thinking of looking through the
parsedMoves list and seeing if the last move was a win for the player. However, this would not work and was
unecessarily complex especially since a gameIsWon function already existed. The same applies for the gameIsDrawn
I also initially made it not use the board format that is already here but made my own where
BlueCow was the string BC etc. I found that the default was better though because it was actually in colour
so I removed my implementation.-}
printBoards :: [Board] -> Player -> Int -> IO ()
printBoards [] _ _ = return ()
printBoards (b:bs) p n = do
    putStrLn $ show b
    let endOfTurn = (n `div` 2) + 1
    let moveNum = if p == BluePlayer then endOfTurn else endOfTurn - 1
    if gameIsWon b p
        then do
            putStrLn $ "\nThe " ++ show p ++ "'s Cow has been trapped: " 
            putStrLn $ "\nThe game has been won by " ++ show (opponent p) ++ "!\n"
        else if gameIsDrawn (b:bs)
            then putStrLn "Game has ended in a draw."
            else do
                putStrLn $ "\nMove " ++ show moveNum ++ ": " ++ show p ++ "'s move.\n"
                let nextPlayer = opponent p
                printBoards bs nextPlayer (n+1)

{-Main function. It gets the argument from the command line using getArgs,
which I got from the System.Environment import. It also pattern matches to make sure
a file has actually been entered in the command line and prompts them how to do it if they don't-}
main :: IO ()
main = do
    args <- getArgs
    case args of
        [file] -> do
            boards <- playFromFile file
            let startingPlayer = BluePlayer
            printBoards boards startingPlayer 1
        _ -> putStrLn "To Use Correctly do: stack run <filename.bgn>"
