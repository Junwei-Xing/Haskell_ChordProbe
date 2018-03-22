{-
This is assignment 1 of COMP90048
Author: Junwei Xing
Student ID: 745568
Date: 8/25/2017

This module realize the implementation of Project1 by using the types from TypeofProj1
This module would be invoked by Proj1Test
-}

module Proj1 (initialGuess, nextGuess, GameState) where

import TypeofProj1
import Data.List

-- transforming a Chord to a Pitch to using the feedback function
beforeFeedback :: Chord -> [Pitch]
beforeFeedback (Chord pitch_a pitch_b pitch_c)
    = [pitch_a, pitch_b, pitch_c ]

-- inputting two Pitches to compare to generate a feedback. f1 is Pitch matches, f2 is note matches without f1, f3 is octave matches without f1
feedback :: [Pitch] -> [Pitch] -> Feedback
feedback answer guess = (f1, f2, f3)
  where 
    f1 = length (correctPitches guess answer)
    f2 = correctNote (getNote(guess\\(correctPitches guess answer))) (getNote(answer\\(correctPitches guess answer)))
    f3 = correctOctave (getOctave(guess\\(correctPitches guess answer))) (getOctave(answer\\(correctPitches guess answer)))

-- getting the intersection of two Pitch list
correctPitches :: [Pitch] -> [Pitch] -> [Pitch]
correctPitches [] _ = []
correctPitches (x:xs) answer
    | (elem x answer) = x : correctPitches xs (removeItem x answer)
    | otherwise = correctPitches xs answer

-- removing a Pitch from a Pitch list
removeItem :: Pitch -> [Pitch] -> [Pitch]
removeItem _ [] = []
removeItem t (x:xs)
    | t == x = xs
    | otherwise = x: removeItem t xs

-- extracting a note list from a pitch list
getNote :: [Pitch] -> [Note]
getNote [] = []
getNote (x:xs) = (myNote x) : getNote xs

-- extracting a octave list from a pitch list
getOctave :: [Pitch] -> [Octave]
getOctave [] = []
getOctave (x:xs) = (myOctave x) : getOctave xs

-- comparing two note lists to record the number of same notes
correctNote :: [Note] -> [Note] -> Int
correctNote [] _ = 0
correctNote (x:xs) another
    | (elem x another) = 1 + correctNote xs (removeNote x another)
    | otherwise = correctNote xs another

-- comparing two octave lists to record the number of same octaves
correctOctave :: [Octave] -> [Octave] -> Int
correctOctave [] _ = 0 
correctOctave (x:xs) another
    | (elem x another) = 1 + correctOctave xs (removeOctave x another)
    | otherwise = correctOctave xs another

-- removing a Note from a Note list
removeNote :: Note -> [Note] -> [Note]
removeNote _ [] = []
removeNote t (x:xs)
    | t == x = xs
    | otherwise = x: removeNote t xs

-- removing a Octave from a Octave list
removeOctave :: Octave -> [Octave] -> [Octave]
removeOctave _ [] = []
removeOctave t (x:xs)
    | t == x = xs
    | otherwise = x: removeOctave t xs

-- initialGuess would take ["A1", "B1", "A2"] as the guess, and the remaining of all possible enumerations as the game state
initialGuess :: ([String], GameState)
initialGuess = (initGuess, initState)
    where
        initGuess = ["A1", "B1", "A2"]
        initState = initialState

-- listing all possible combinations with expectation 0 without ["A1", "B1", "A2"] as initState  
initialState :: GameState
initialState
    = (GameState [(Possible_guess (Chord a b c) 0) 
        | a <- [minBound::Pitch ..], 
        b <- [a ..], 
        c <- [b ..], 
        a /= b, b /= c,
        (a, b, c) /= ((Pitch A O1), (Pitch B O1), (Pitch A O2))
        ])

{- 
nextGuess would use previous guess, feedback for the guess, and current game state to :
(1) filter the game state; delect the possible guesses if they have different feedbacks against the previous guess
(2) calculating the excepetation value for each remaining possible guess which is in the filtered game state
(3) finding the Chort with minimum excepetation value in the filtered game, as next guess
(4) excluding the guess to get the next game state
-}
nextGuess :: ([String],GameState) -> Feedback -> ([String],GameState)
nextGuess (formerGuess, (GameState formerState)) myFeedback = (toString newGuess, GameState newState)
    where
        afterFilter = myFilter (toChord formerGuess) formerState myFeedback
        afterscorer = scorer afterFilter afterFilter
        (newGuess, newState) = minFinder afterscorer

-- removing the inconsistent guesses from previous game state 
myFilter :: Chord -> [Possible_guess] -> Feedback -> [Possible_guess]
myFilter  _ [] _  = []
myFilter myPitch ((Possible_guess x value):xs) myFeedback
    |  ((feedback (beforeFeedback myPitch) (beforeFeedback x)) == myFeedback) = (Possible_guess x value): myFilter myPitch xs myFeedback
    | otherwise = myFilter myPitch xs myFeedback

-- updating the excepetation value of each guess in a game state
scorer :: [Possible_guess] -> [Possible_guess] -> [Possible_guess]
scorer [] _ = []
scorer ((Possible_guess x value):xs) duplicate = (Possible_guess x newValue) : scorer xs duplicate
    where
        listResult = feedbackList x duplicate
        countResult = countScore listResult listResult
        newValue = calculateEx countResult (length listResult)

-- using a target chord against the whole game state to generate a corresponding feedback list
feedbackList :: Chord -> [Possible_guess] -> [Feedback]
feedbackList _ [] = []
feedbackList myPitch ((Possible_guess x value):xs) = currentFeedback : feedbackList myPitch xs
    where 
        currentFeedback = feedback (beforeFeedback myPitch) (beforeFeedback x)

-- resulting a feedback list by counting the number of same feedbacks, and deleting duplications
countScore :: [Feedback] -> [Feedback] -> [Feedback_score]
countScore [] _ = []
countScore (x:xs) duplicate = (Feedback_score x result) : countScore (deleter x xs) duplicate
    where result = counter x duplicate

-- counting
counter :: Feedback -> [Feedback] -> Int
counter _ [] = 0
counter myFeedback (x:xs)
    | (myFeedback == x) = 1 + counter myFeedback xs
    | otherwise = counter myFeedback xs

-- deleting
deleter ::  Feedback -> [Feedback] -> [Feedback]
deleter _ [] = []
deleter myFeedback (x:xs)
    | (myFeedback /= x) = x : deleter myFeedback xs
    | otherwise = deleter myFeedback xs

-- using the processed feedback_score list to compute the excepetation value
calculateEx :: [Feedback_score] -> Int -> Float
calculateEx [] _ = 0
calculateEx ((Feedback_score x num):xs) len = ( (fromIntegral num) / (fromIntegral len) * (fromIntegral num)) + calculateEx xs len

{-
last step! finding the minimum expectation value guess 
<I select the first one element in a list as nextGuessg
because it is possible these are multiple minimum guesses in this game state>
and excluding it to generate next game state
-}
minFinder :: [Possible_guess] -> (Chord, [Possible_guess])
minFinder myGuess = (newGuess , newState)
    where
        Possible_guess newGuess myScore = head (beforeComp myGuess myGuess)
        newState = myGuess \\ [Possible_guess newGuess myScore]

-- a recursion for the game state, returning the minimum excepetation value list
beforeComp :: [Possible_guess] -> [Possible_guess] -> [Possible_guess]
beforeComp [] _ = []
beforeComp ((Possible_guess x value):xs) myGuess
    | comparer x myGuess = (Possible_guess x value) : beforeComp xs myGuess
    | otherwise = beforeComp xs myGuess

-- whether this Chord (i.e. guess) is minimum
comparer :: Chord -> [Possible_guess] -> Bool
comparer _ [] = True
comparer myChord ((Possible_guess x value1):xs)
    | myChord <= x = True && comparer myChord xs
    | otherwise = False && comparer myChord xs

-- transforming a chord to a string for user outputting
toString :: Chord -> [String]
toString (Chord pitch_a pitch_b pitch_c) 
    = [(show(myNote pitch_a))++(show(myOctave pitch_a)),(show(myNote pitch_b))++(show(myOctave pitch_b)),(show(myNote pitch_c))++(show(myOctave pitch_c))]

-- transforming a string to a chord for user inputting
toChord:: [String] -> Chord
toChord [a,b,c] 
    = Chord(Pitch (read [(head a)]) (read ("O"++[(last a)]))) (Pitch (read [(head b)]) (read ("O"++[(last b)]))) (Pitch (read [(head c)]) (read ("O"++[(last c)])))