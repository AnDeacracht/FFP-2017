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

initialState =  GameState 
        { players = allPlayers
        , turn = getRandomPlayer $ unsafePerformIO $ randomRIO (1, 4)
        , rollsAllowed = 3
        , roll = 0
        }

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
            rollsAllowed = 1,
            roll = 6
        }

getPlayerByColour :: GameState -> Colour -> Player
getPlayerByColour state colour = fromJust $ Map.lookup colour $ players state

getActivePlayer :: GameState -> Player
getActivePlayer state = getPlayerByColour state (turn state)

 -- handleMoveRequest :: GameState -> DiceRoll -> GameState
 -- handleMoveRequest state 6 = TODO

nothingOnBoard :: Player -> Bool
nothingOnBoard player = null $ occupiedFields player

handleRoll :: GameState -> DiceRoll -> GameState
handleRoll state 6 -- take action immediately if a 6 is rolled
	| nothingOnBoard (getActivePlayer state) = putPlayerOnBoard state activePlayer
    | otherwise = checkStartField
	where
        activePlayer = getActivePlayer state
        checkStartField = 
            if (startField activePlayer) `elem` (occupiedFields activePlayer) 
                then movePlayer state activePlayer 6 (startField activePlayer) -- move away immediately
                else putPlayerOnBoard state activePlayer
handleRoll state rollResult
    | nothingOnBoard (getActivePlayer state) = checkRollCount
    | otherwise =  state --TODO
    where
        checkRollCount =
            if (rollsAllowed state) > 1
                then GameState {
                    players = players state,
                    turn = turn state,
                    rollsAllowed = (rollsAllowed state) - 1,
                    roll = rollResult
                }
                else GameState {
                    players = players state,
                    turn = succ $ turn state,
                    rollsAllowed = 3,
                    roll = rollResult
                }

movePlayer :: GameState -> Player -> DiceRoll -> String -> GameState
movePlayer state playerToModify rollResult fromField = GameState {
        players = Map.adjust (\_ -> updatedPlayer) (turn state) (players state),
        turn = succ $ turn state,
        rollsAllowed = 1,
        roll = rollResult
    }
    where
        updatedPlayer = Player {
        	colour = colour playerToModify,
            inHouse = inHouse playerToModify,
            inGoal = inGoal playerToModify,
            occupiedFields = move fromField (newField fromField rollResult) (occupiedFields playerToModify),
            startField = startField playerToModify,
            mustLeaveStart = False 
        }

newField :: String -> Int -> String
newField fieldId steps = show $ (read fieldId) + steps -- TODO fails with goal fields for now

move :: String -> String -> [String] -> [String]
move fromField toField fieldList = toField : delete fromField fieldList

rollDie :: Int 
rollDie = unsafePerformIO $ randomRIO (1, 6)

getRandomPlayer :: Int -> Colour
getRandomPlayer 1 = Red
getRandomPlayer 2 = Blue
getRandomPlayer 3 = Green
getRandomPlayer 4 = Yellow

