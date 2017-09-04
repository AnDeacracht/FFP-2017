{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}


module MoveFunctions where

import qualified Data.Text as T

import DataDeclarations
import Player
import Utils 

determineMoveType :: Player -> FieldId -> DiceRoll -> MoveType
determineMoveType player fromField roll
    | isGoalField fromField && invalidGoalMove = InvalidMove $ "Invalid goal move: " ++ msg
    | isGoalField fromField = GoalMove roll
    | (isPastFinalField player fromField roll) && (isInvalidEnterGoalMove player fromField roll) = InvalidMove "Rolled too high"
    | isPastFinalField player fromField roll = EnterGoalMove roll
    | fieldOccupiedBySelf player $ show (toInt fromField + roll) = InvalidMove "Got one of your guys there"
    | otherwise = FieldMove roll
    where
        (invalidGoalMove, msg) = isInvalidGoalMove player fromField roll


-- you're past your final field if you roll high enough to end up with a number higher than your final field

isPastFinalField :: Player -> FieldId -> DiceRoll -> Bool
isPastFinalField player fromField roll = toInt fromField + roll > standardisedFinal
    where standardisedFinal = 40 + (toInt $ startField player) - 1


-- a move is invalid if you roll too high to even fit into the goal

isInvalidEnterGoalMove :: Player -> FieldId -> DiceRoll -> Bool
isInvalidEnterGoalMove player fromField roll = toInt fromField + roll  > finalFieldNr + 4 - goalOccupation player
    where
        finalFieldNr = toInt $ finalField player

isInvalidGoalMove :: Player -> FieldId -> DiceRoll -> (Bool, String)
isInvalidGoalMove player fromField roll = (violatesOccupation || violatesSpace || violatesNoSkipping, msg)
    where
        from = extractGoalCellNumber fromField
        violatesOccupation = fieldOccupiedBySelf player $ makeGoalCellNumber player (from - roll) 
        violatesSpace = (from - roll) < 1
        violatesNoSkipping = isSkipNeeded player fromField roll
        msg
            | violatesOccupation = "Goal field already taken by one of your guys"
            | violatesSpace = "Goal ends at field 1"
            | violatesNoSkipping = "Can't skip pieces inside goal"
            | otherwise = "This makes the linter happy"

-- returns the occupied goal cell that is closest to the entrance 

goalOccupation :: Player -> Int
goalOccupation player = if (null goalCells) then 0 else maximum goalCells
    where
        goalFields = map T.pack $ filter isGoalField (occupiedFields player)
        goalCells = map (extractGoalCellNumber . T.unpack) goalFields

isGoalField :: FieldId -> Bool
isGoalField fieldId = T.isInfixOf "goal" $ T.pack fieldId

-- this operates on fields like "goal-4-red"

isSkipNeeded :: Player -> FieldId -> DiceRoll -> Bool
isSkipNeeded player fromField roll = any (fieldOccupiedBySelf player) fieldsBetween
    where
        fromFieldNr = extractGoalCellNumber fromField 
        numbersBetween = coreList [(fromFieldNr - roll) .. fromFieldNr] -- doesn't matter if the higher index goes last, all we want are the fields between
        fieldsBetween = map (makeGoalCellNumber player) numbersBetween

fieldOccupiedBySelf :: Player -> FieldId -> Bool
fieldOccupiedBySelf player fieldId = fieldId `elem` occupiedFields player

-- Goal fields are numbered 4 to 1, starting with the field closest to the entrance.
-- To determine how far into the goal a player gets, subtract the steps they have left at the entrance from 5.
-- Thus, if you have 4 steps left, you'll get to the top field, while with only 1 step you'll make the first field. 

makeEnterGoalMove :: Player -> FieldId -> DiceRoll -> FieldId
makeEnterGoalMove player fromField roll = "goal-" ++ targetField ++ "-" ++ colour player
    where
        finalFieldNr = toInt $ finalField player
        fromFieldNr = toInt fromField
        -- final field is 40, you're at 38 and rolled a 4 ---> 4 - 2 = 2 steps left inside the goal
        stepsLeftAtGoal = roll - (finalFieldNr - fromFieldNr)
        targetField = show $ 5 - stepsLeftAtGoal


makeFieldMove :: FieldId -> DiceRoll -> FieldId
makeFieldMove start roll = show $ (toInt start + roll) `mod` 41 -- modulo for wraparound

makeGoalMove :: Player -> FieldId -> DiceRoll -> FieldId
makeGoalMove player fromField roll = "goal-" ++ targetField ++ "-" ++ (colour player)
    where
        fromFieldNr = extractGoalCellNumber fromField
        targetField = show $ fromFieldNr - roll


cannotMove :: Player -> DiceRoll -> Bool
cannotMove player roll = all isInvalid possibleMovers
    where
        fields = occupiedFields player
        possibleMovers = map (\field -> determineMoveType player field roll) fields