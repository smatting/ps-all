module Main where

import Prelude (Unit)
import Effect (Effect)
import Effect.Console (log)
import Data.Maybe
{-- import Data.EuclidianRing (mod) --}

data NoteLetter = A | B | C | D | E | F | G

data NoteMod = Sharp | Flat

data Note = Note NoteLetter (Maybe NoteMod)

a :: Note
a = Note A (Just Sharp)

{-- bottom = let x = x in x --}

{-- shift :: Array a -> Array a --}
{-- shift = bottom --}

{-- fifthAsc :: Int -> Int --}
{-- fifthAsc i = (6 + i * 5) `mod` 7 --}

{-- G D A E B F C --}
{-- F C G D A E B --}

{-- F B E A D G C --}
{-- B E A D G C F --}

data ChordGenus = Minor | Major | Dim | Aug

data Foo = Bar | Sadg { meh :: String }

main :: Effect Unit
main = do
  log "🍝 oh"
