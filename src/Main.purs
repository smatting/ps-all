module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Maybe (Maybe(..), maybe, fromMaybe)
import Data.Array (range, index)
import Data.Show
import Data.Foldable (elem)
import Data.Tuple
import Data.String.Common (toLower)

data NoteLetter = A | B | C | D | E | F | G

instance showNoteLetter :: Show NoteLetter
  where
    show A = "A"
    show B = "B"
    show C = "C"
    show D = "D"
    show E = "E"
    show F = "F"
    show G = "G"

notesOrdered :: Array NoteLetter
notesOrdered = [A, B, C, D, E, F, G]

noteLetter :: Int -> NoteLetter
noteLetter i = fromMaybe A (index notesOrdered (i `mod` 7))

data NoteMod = Sharp | Flat

data Note = Note NoteLetter (Maybe NoteMod)

instance showNote :: Show Note
  where
    show (Note nl mMod) =
      show nl <> maybe "" showMod mMod
      where
        showMod Sharp = "#"
        showMod Flat = "b"

fifth :: Int -> Int
fifth i = (2 + i * 4) `mod` 7

--| sharps i is the sharped notes for fifth i
sharps :: Int -> Array Int
sharps 0 = []
sharps i = (\i -> fifth ((6 + i) `mod` 7)) <$> range 0 (i-1)

--| flats i are the flatted notes for fifth (-i)
flats :: Int -> Array Int
flats 0 = []
flats i = (\i -> fifth ((5 - i) `mod` 7)) <$> range 0 (i-1)

scale :: Int -> Array Note
scale n =
  let Tuple modIdxs modSymb =
        if (n < 0) then
          Tuple (flats (-n)) Flat
        else
          Tuple (sharps n) Sharp

    in flip map (range 0 6) $ (\i ->
      let noteIdx = (fifth n + i) `mod` 7
        in Note (noteLetter noteIdx) (if noteIdx `elem` modIdxs then (Just modSymb) else Nothing)
          )

data ChordGenus = MajorChord | MinorChord | DiminishedChord | AugmentedChor

data Chord = Chord {
    chordBase :: Note,
    chordGenus :: ChordGenus
  }

data Roman = Roman Int ChordGenus

instance showRoman :: Show Roman
  where
    show (Roman k genus) =
      let caseMod = case genus of
            MajorChord -> identity
            MinorChord -> toLower
            DiminishedChord -> toLower
            AugmentedChor -> identity
      in
        caseMod (roman k) <> showChordGenus genus

showChordGenus :: ChordGenus -> String
showChordGenus MajorChord = ""
showChordGenus MinorChord = ""
showChordGenus DiminishedChord = "o"
showChordGenus AugmentedChor = "+"

roman :: Int -> String
roman 1 = "I"
roman 2 = "II"
roman 3 = "III"
roman 4 = "IV"
roman 5 = "V"
roman 6 = "VI"
roman 7 = "VII"
roman _ = "?"


{-- analyzeScale :: Int -> Array (Tuple Chord Roman) --}
{-- analyzeScale n = --}


main :: Effect Unit
main = do
  log "🍝 oh"
