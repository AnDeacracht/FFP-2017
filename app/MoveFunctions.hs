{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}


module MoveFunctions where

import qualified Data.Text as T

import DataDeclarations
import Utils 

determineMoveType :: Player -> FieldId -> Int -> MoveType
determineMoveType player fromField roll
    | isGoalField fromField && invalidGoalMove = InvalidMove $ "Invalid goal move: " ++ message
    | isGoalField fromField = GoalMove roll
    | (isPastFinalField player fromField roll) && (isInvalidEnterGoalMove player fromField roll) = InvalidMove "Rolled to high"
    | isPastFinalField player fromField roll = EnterGoalMove roll
    | fieldOccupiedBySelf player $ show (toInt fromField + roll) = InvalidMove "Got one of your guys there"
    | otherwise = FieldMove roll
    where
    	(invalidGoalMove, message) = isInvalidGoalMove player fromField roll

calculateTargetFieldNr :: FieldId -> Int -> Int
calculateTargetFieldNr start roll = (toInt start + roll) `mod` 40 -- modulo for wraparound

-- you're past your final field if you roll high enough to end up with a number higher than your final field
-- can't use calculateTargetFieldNr since we're not interested in a concrete value and modulo would screw up the check

isPastFinalField :: Player -> FieldId -> Int -> Bool
isPastFinalField player fromField roll = (toInt fromField) + roll > finalFieldNr
    where
        finalFieldNr = toInt $ finalField player


-- a move is invalid if you roll too high to even fit into the goal
-- can't use calculateTargetFieldNr since we're not interested in a concrete value and modulo would screw up the check

isInvalidEnterGoalMove :: Player -> FieldId -> Int -> Bool
isInvalidEnterGoalMove player fromField roll = (toInt fromField) + roll  > finalFieldNr + 4 - (goalOccupation player)
    where
        finalFieldNr = toInt $ finalField player

isInvalidGoalMove :: Player -> FieldId -> Int -> (Bool, String)
isInvalidGoalMove player fromField roll = ((violatesOccupation || violatesSpace || violatesNoSkipping), message)
    where
        from = extractGoalCellNumber fromField
        violatesOccupation = fieldOccupiedBySelf player $ makeGoalCellNumber player (from - roll) 
        violatesSpace = (from - roll) < 1
        violatesNoSkipping = isSkipNeeded player fromField roll
        message
	        | violatesOccupation = "Goal field already taken by one of your guys"
	        | violatesSpace = "Goal ends at field 1"
	        | violatesNoSkipping = "Can't skip pieces inside goal"

-- returns the occupied goal cell that is closest to the entrance 

goalOccupation :: Player -> Int
goalOccupation player = foldl1 max goalCells
    where
        goalFields = map (T.pack) $ filter isGoalField (occupiedFields player)
        goalCells = map extractGoalCellNumber $ map T.unpack goalFields

isGoalField :: FieldId -> Bool
isGoalField fieldId = T.isInfixOf "goal" $ T.pack fieldId

-- this operates on fields like "goal-4-red"

isSkipNeeded :: Player -> FieldId -> Int -> Bool
isSkipNeeded player startField roll = any (\goalField -> fieldOccupiedBySelf player goalField) fieldsBetween
    where
    	startFieldNr = extractGoalCellNumber startField 
       	numbersBetween = coreList [(startFieldNr - roll) .. startFieldNr] -- doesn't matter if the higher index goes last, all we want are the fields between
        fieldsBetween = map (makeGoalCellNumber player) numbersBetween

fieldOccupiedBySelf :: Player -> FieldId -> Bool
fieldOccupiedBySelf player fieldId = fieldId `elem` (occupiedFields player)

