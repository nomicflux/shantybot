module Song.Song where

import Data.Argonaut
import Prelude

import Data.Array (filter)
import Data.Array as A
import Data.Either (hush)
import Data.Maybe (Maybe(..), maybe)
import Data.String (Pattern(..), split, trim)
import Data.String as S

data Line = Line String

instance encodeLine :: EncodeJson Line where
  encodeJson (Line line) = jsonSingletonObject "line" (encodeJson line)

instance decodeLine :: DecodeJson Line where
  decodeJson l = do
    obj <- decodeJson l
    line <- getField obj "line"
    pure (Line line)

data Verse = Verse (Array Line)

instance encodeVerse :: EncodeJson Verse where
  encodeJson (Verse lines) = jsonSingletonObject "verse" (encodeJson lines)

instance decodeVerse :: DecodeJson Verse where
  decodeJson v = do
    obj <- decodeJson v
    verse <- getField obj "verse"
    pure (Verse verse)

data Song = Song { songTitle :: String
                 , songChorus :: Maybe (Array Line)
                 , songVerses :: Array Verse
                 }

instance encodeSong :: EncodeJson Song where
  encodeJson (Song song) =
    let base = ("title" := song.songTitle
                ~> "verses" := song.songVerses
                ~> jsonEmptyObject
               )
    in maybe base (\c -> "chorus" := c ~> base) song.songChorus

instance decodeSong :: DecodeJson Song where
  decodeJson s = do
    obj <- decodeJson s
    title <- getField obj "title"
    let chorus = getField obj "chorus"
    verses <- getField obj "verses"
    pure (Song { songTitle: title
               , songChorus: hush chorus
               , songVerses: verses
               })

mkLines :: String -> Array Line
mkLines = map Line <<< filter (not S.null) <<< map trim <<< split (Pattern "\n")

lineToString :: Line -> String
lineToString (Line line) = line

getTitleText :: Song -> String
getTitleText (Song song) = song.songTitle

getChorusText :: Song -> Maybe (Array String)
getChorusText (Song song) = map (map lineToString) song.songChorus

getVersesText :: Song -> Array (Array String)
getVersesText (Song song) = verseToBlock <$> song.songVerses
  where
    verseToBlock (Verse verse) = lineToString <$> verse

mkSong :: String -> String -> Array String -> Song
mkSong title chorus verses =
  let chorus' = mkLines chorus
      verses' = Verse <$> mkLines <$> verses
  in Song { songTitle: title
          , songChorus: if S.null chorus then Nothing else Just chorus'
          , songVerses: verses'
          }
