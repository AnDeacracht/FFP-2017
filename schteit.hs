import Control.Monad.State
import qualified Data.Map as Map

type FieldNumber = Int
type PlayingField = Map.Map FieldNumber Field

data Player = Player { 
    colour :: Colour, 
    startingPoint :: FieldNumber,
    house :: Int,
    safeHouse :: Int

} deriving (Show)

data Colour = Red | Blue | Green | Yellow deriving (Show)

data Field = Field {
    occupiedBy :: Maybe Player
} deriving (Show)


data Game = Game {
    playingField :: PlayingField,
    players :: [Player]
}

{-- for testing --}

blue = Player { 
    colour = Blue, 
    startingPoint = 1, 
    house = 4, 
    safeHouse = 0 
}

green = Player { 
    colour = Green, 
    startingPoint = 11, 
    house = 4, 
    safeHouse = 0 
}

yellow = Player { 
    colour = Yellow, 
    startingPoint = 21, 
    house = 4, 
    safeHouse = 0 
}

red = Player { 
    colour = Red, 
    startingPoint = 31, 
    house = 4, 
    safeHouse = 0 
}

emptyPlayingField = Map.fromList $ map (\x -> (x, Field Nothing)) [1..40]

game = Game { 
    playingField = emptyPlayingField,
    players = [blue, green, yellow, red]
}

clearField :: FieldNumber -> PlayingField -> PlayingField
clearField fieldnr playingField =
    Map.adjust (\_ -> Field Nothing) fieldnr playingField

putPlayer :: Player -> FieldNumber -> PlayingField -> PlayingField
putPlayer player fieldnr playingField =
    Map.adjust (\_ -> Field(Just player)) fieldnr playingField

enterPlayingField :: Player -> PlayingField -> PlayingField
enterPlayingField player playingField = 
    putPlayer player (startingPoint player) playingField

movePlayer :: Player -> FieldNumber -> Int -> PlayingField -> PlayingField
movePlayer player fieldnr steps playingField =
    let clearedField = clearField fieldnr playingField
    in putPlayer player (fieldnr + steps) clearedField