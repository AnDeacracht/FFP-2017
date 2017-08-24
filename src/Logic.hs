{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Logic where

import Data.Aeson
import Data.Text
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B
import GHC.Generics


data Player = Player { 
	colour :: String,
	house :: Int, 
	goal :: Int, 
	fields :: [String]
} deriving (Show, Generic)

data GameState = GameState {
	players :: Player,
	turn :: String,
	roll :: Int
} deriving (Show, Generic)