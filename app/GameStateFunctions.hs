module GameStateFunctions where

import System.Random
import System.IO.Unsafe
import qualified Data.Map as Map
import Data.Maybe
import Data.List

import DataDeclarations

{-- INITIAL STATE --}

allPlayers = 
    Map.fromList 
    [ (Red, Player "red" 4 0 [] "1" False)
    , (Blue, Player "blue" 4 0 [] "11" False)
    , (Green, Player "green" 4 0 [] "21" False)
    , (Yellow, Player "yellow" 4 0 [] "31" False)
    ]

initialState =  
    GameState 
    { players = allPlayers
    , turn = getRandomPlayer $ unsafePerformIO $ randomRIO (1, 4)
    , rollsAllowed = 3
    , roll = 0
    , waitingForMove = False
    }

{-- THE ROLL HANDLER --}

handleRoll :: GameState -> DiceRoll -> GameState
-- if the player rolls a 6, take action immediately.
handleRoll state 6
    | nothingOnBoard activePlayer = putPlayerOnBoard state activePlayer -- nothing on board, put piece on it
    | nothingInHouse activePlayer = -- nothing left in house, reroll
        GameState
        { players = players state
        , turn = turn state
        , rollsAllowed = 1 -- you must reroll if you have a 6
        , roll = roll state + 6
        , waitingForMove = False
        }
    | otherwise = checkStartField
    where
        activePlayer = getActivePlayer state
        checkStartField = -- check start field - if something is there, is needs to move on
            if (startField activePlayer) `elem` (occupiedFields activePlayer) 
                then movePlayer state activePlayer 6 (startField activePlayer) -- move away immediately
                else putPlayerOnBoard state activePlayer -- put new piece on board
-- if the player rolls anything else
handleRoll state rollResult
    | nothingOnBoard (getActivePlayer state) = checkRollCount
    | otherwise = 
        GameState 
        { players = players state
        , turn = turn state
        , rollsAllowed = 0 -- no reroll
        , roll = roll state + rollResult
        , waitingForMove = True -- wait for move command
        }
    where
        checkRollCount =
            -- still more than one roll allowed
            if (rollsAllowed state) > 1
                then GameState 
                { players = players state
                , turn = turn state
                , rollsAllowed = (rollsAllowed state) - 1
                , roll = rollResult
                , waitingForMove = False
                }
                -- all rolls used up, move on
                else GameState 
                { players = players state
                , turn = succ $ turn state
                , rollsAllowed = 3
                , roll = 0 -- reset roll
                , waitingForMove = False
                }

{-- MOVE FUNCTIONS --}

movePlayer :: GameState -> Player -> DiceRoll -> String -> GameState
movePlayer state playerToModify rollResult fromField = 
    GameState 
    { players = Map.adjust (\_ -> updatedPlayer) (turn state) (players state)
    , turn = succ $ turn state
    , rollsAllowed = 1
    , roll = 0 -- reset roll
    , waitingForMove = False
    }
    where
        updatedPlayer = 
            Player 
            { colour = colour playerToModify
            , inHouse = inHouse playerToModify
            , inGoal = inGoal playerToModify
            , occupiedFields = move fromField (newField fromField rollResult) (occupiedFields playerToModify)
            , startField = startField playerToModify
            , mustLeaveStart = False 
            }

-- handleMoveRequest :: GameState -> DiceRoll -> GameState
-- handleMoveRequest state 6 = TODO

putPlayerOnBoard :: GameState -> Player -> GameState
putPlayerOnBoard state playerToModify = if arePiecesInHouse then modifiedState else state
	where
        arePiecesInHouse = (inHouse playerToModify) > 0
        updatedPlayer = Player 
            { colour = colour playerToModify
            , inHouse = (inHouse playerToModify) - 1
            , inGoal = inGoal playerToModify
            , occupiedFields = (startField playerToModify) : (occupiedFields playerToModify)
            , startField = startField playerToModify
            , mustLeaveStart = True 
            }
        modifiedState = GameState 
            { players = Map.adjust (\_ -> updatedPlayer) (turn state) (players state)
            , turn = turn state
            , rollsAllowed = 1
            , roll = 6
            , waitingForMove = True
            }

{-- UTILITY FUNCTIONS --}

getPlayerByColour :: GameState -> Colour -> Player
getPlayerByColour state colour = fromJust $ Map.lookup colour $ players state

getActivePlayer :: GameState -> Player
getActivePlayer state = getPlayerByColour state (turn state)


nothingOnBoard :: Player -> Bool
nothingOnBoard player = null $ occupiedFields player

nothingInHouse :: Player -> Bool
nothingInHouse player = inHouse player <= 0


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

