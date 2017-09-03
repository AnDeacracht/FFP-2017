{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DeriveGeneric         #-}

module Colour where

import GHC.Generics
import Data.Aeson

data Colour = Red | Blue | Green | Yellow deriving (Ord, Eq, Generic)

instance Show Colour where
    show c = case c of
        Red -> "red"
        Blue -> "blue"
        Green -> "green"
        Yellow -> "yellow"

instance Enum Colour where
    
    succ Red = Blue
    succ Blue = Green
    succ Green = Yellow
    succ Yellow = Red

    toEnum 1 = Red
    toEnum 2 = Blue
    toEnum 3 = Green
    toEnum _ = Yellow

    fromEnum Red = 1
    fromEnum Blue = 2
    fromEnum Green = 3
    fromEnum _ = 4

instance ToJSON Colour
--instance FromJSON Colour
instance ToJSONKey Colour
--instance FromJSONKey Colour
