{-
This Haskell file aims to define the data type and data construct of Proj1 - type declarations
author: Xing Junwei
Student ID: 745568
-}

module TypeofProj1 where

import Data.List

-- Note 
data Note = A | B | C | D | E | F | G
    deriving (Show, Read, Eq, Ord, Enum, Bounded)

--Octaves with a prefix "O" for defining a data type
data Octave = O1 | O2 | O3
    deriving (Read, Eq, Ord, Enum, Bounded)

--Show a octave without the "O"
instance Show Octave where
    show O1 = "1"
    show O2 = "2"
    show O3 = "3"

--Each Pitch consists of a Note and a Octave
data Pitch = Pitch {myNote::Note,
                    myOctave::Octave}
    deriving (Show, Eq, Ord, Bounded)

--Enum instance includes forEnum, toNnum, enumFrom, enumFromThen
instance Enum Pitch where
    fromEnum (Pitch n o) =(fromEnum n) + (fromEnum o) * m
        where
            m = 1 + (fromEnum (maxBound::Note))
    toEnum x = (Pitch n o)
        where
            m = 1 + (fromEnum (maxBound::Note))
            o = toEnum(x `div` m)
            n = toEnum(x `mod` m)
    enumFrom x  = enumFromTo x maxBound
    enumFromThen x y = enumFromThenTo x y bound
        where
            bound 
                | fromEnum y >= fromEnum x = maxBound
                | otherwise = minBound

-- Chord, which consists of 3 Pitches
data Chord = Chord Pitch Pitch Pitch
    deriving (Show, Eq, Ord)

-- Possible_guess consists of 1, a Chord as a Guess. 2, a Float saves the expectation within current GameState
data Possible_guess = Possible_guess Chord Float
    deriving (Show, Eq, Ord)

-- GameState represents the pool of guesses for a game state
data GameState = GameState [Possible_guess]
    deriving (Show)

-- Feedback 1, number of pitch matches 2, number of note matches without pitch matches 3, number of octave matches without pitch matches
type Feedback = (Int, Int, Int)

-- Feedback_score consistes of 1, Feedback 2, a Int stores the number of this feedback occurs in a GameState
data Feedback_score = Feedback_score Feedback Int
    deriving (Show, Eq, Ord)