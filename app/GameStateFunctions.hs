module GameStateFunctions where

import System.Random
import System.IO.Unsafe
import qualified Data.Map as Map
import Data.Maybe
import Data.List

import DataDeclarations

allPlayers = Map.fromList [
    (Red, Player "red" 4 0 [] "1" False), 
    (Blue, Player "blue" 4 0 [] "11" False), 
    (Green, Player "green" 4 0 [] "21" False), 
    (Yellow, Player "yellow" 4 0 [] "31" False)
    ]

changeRollCount :: GameState -> Int -> GameState
changeRollCount state n = GameState (players state) (turn state) n

putPlayerOnBoard :: GameState -> Player -> GameState
putPlayerOnBoard state playerToModify = if arePiecesInHouse then modifiedState else state
	where
        arePiecesInHouse = (inHouse playerToModify) > 0
        updatedPlayer = Player {
        	colour = colour playerToModify,
            inHouse = (inHouse playerToModify) - 1,
            inGoal = inGoal playerToModify,
            occupiedFields = (startField playerToModify) : (occupiedFields playerToModify),
            startField = startField playerToModify,
            mustLeaveStart = True 
        }
        modifiedState = GameState {
            players = Map.adjust (\_ -> updatedPlayer) (turn state) (players state),
            turn = turn state,
            rollsAllowed = 1
        }

getPlayerByColour :: GameState -> Colour -> Player
getPlayerByColour state colour = fromJust $ Map.lookup colour $ players state

handleRoll :: GameState -> Player -> DiceRoll -> GameState
handleRoll state player 6
	| nothingOnBoard = putPlayerOnBoard state player
    | otherwise = checkStartField
	where
        nothingOnBoard = null $ occupiedFields player
        checkStartField = 
            if (startField player) `elem` (occupiedFields player) 
                then movePlayer state player 6 (startField player) -- move away immediately
                else putPlayerOnBoard state player
handleRoll state player n = changeRollCount state ((rollsAllowed state) - 1) 

movePlayer :: GameState -> Player -> DiceRoll -> String -> GameState
movePlayer state playerToModify roll fromField = GameState {
        players = Map.adjust (\_ -> updatedPlayer) (turn state) (players state),
        turn = turn state,
        rollsAllowed = 1
    }
    where
        updatedPlayer = Player {
        	colour = colour playerToModify,
            inHouse = inHouse playerToModify,
            inGoal = inGoal playerToModify,
            occupiedFields = (newField fromField roll) : (delete fromField (occupiedFields playerToModify)),
            startField = startField playerToModify,
            mustLeaveStart = False 
        }

newField :: String -> Int -> String
newField fieldId steps = show $ (read fieldId) + steps

roll :: Int 
roll = unsafePerformIO $ randomRIO (1, 6)

getRandomPlayer :: Int -> Colour
getRandomPlayer 1 = Red
getRandomPlayer 2 = Blue
getRandomPlayer 3 = Green
getRandomPlayer 4 = Yellow