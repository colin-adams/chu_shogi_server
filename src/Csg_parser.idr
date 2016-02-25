-- Copyright 2016 Colin Adams
--
-- This file is part of chu-shogi.
--
--  Chu-shogi is free software: you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation, either version 3 of the License, or
--  (at your option) any later version.

--  Chu-shogi is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.

--  You should have received a copy of the GNU General Public License
--  along with chu-shogi.  If not, see <http://www.gnu.org/licenses/>.

||| Parser for format-3 .csg files (as produced by Chu_shogi 2.10 (?) by Colin Adams) 
module Forsythe_parser

import Lightyear
import Lightyear.Strings
import Lightyear.Char
import Board
import Piece
import Game_state
import Effect.File
import Effects

%default total
%access private

||| Parse a String (the contents of a .csg file) yielding a Game_state or an error string
from_csg : Parser Game_state
from_csg = fail "TODO"

||| Re-construct the state of the game at the end of a Format-3 .csg file
|||
||| @file_name - absolute or relative file-system path to .csg file e.g. /home/colin/Downlads/hist.csg 
partial abstract parse_csg : (file_name : String) -> Eff (Either String Game_state) [FILE_IO ()]
parse_csg file_name = do
  ei <- Effect.File.Default.readFile file_name
  case ei of
    Left e  => pure $ Left e
    Right c => pure $ parse from_csg c


{- Forsythe_parse code follows for example usage
col : Char -> Piece_colour
col c = case isUpper c of
  True  => White
  False => Black

||| Parse + prefix
parse_promoted : Parser (Maybe Char)
parse_promoted = opt (char '+')

||| Parse = prefix
parse_deferred : Parser (Maybe Char)
parse_deferred = opt (char '=')

||| Combine one or two characters into a list
abbreviation : Char -> Maybe Char -> List Char
abbreviation abbrev1 abbrev2 = case abbrev2 of
    Nothing => [abbrev1]
    Just l  => abbrev1 :: [l]

status : Maybe Char -> Maybe Char -> Promotion_status
status pr df = case pr of
    Nothing => case df of
      Nothing => Not_yet_promoted
      Just _ => Declined_to_promote
    Just _  => No_promotion

||| Produce a piece from it's Forsythe abbreviation
parse_abbreviation : Parser (List Square)
parse_abbreviation = do
  pr <- parse_promoted
  df <-parse_deferred
  abbrev1 <- letter
  abbrev2 <- opt letter
  let abbrev = (abbreviation abbrev1 abbrev2)
  let abbrev' = case pr of
    Nothing => pack abbrev
    Just p  => pack $ p :: abbrev
  case piece_from_abbreviation abbrev' of
    Nothing => fail "Not a piece abbreviation"
    Just p  => case (pr, df) of
      (Just _, Just _) => fail "cannot both promote and defer promotion"
      _                => pure $ [Just (Make_piece p (col abbrev1), status pr df)]


count : Parser Nat
count = assert_total integer -- Hmm. TODO?

||| Produce a list of empty Squares
spaces : Parser (List Square)
spaces = do
  cnt <- count
  pure $ replicate cnt Nothing
  
piece_or_squares : Parser (List Square)
piece_or_squares = do
  spaces <|> parse_abbreviation
  
parse_rank : Parser (List Square)
parse_rank = do
  lists <- assert_total (sepBy piece_or_squares (char ',')) -- Hmmm. TODO
  pure $ flatten lists

parse_ranks : Parser (List (List Square))
parse_ranks = do
  assert_total (sepBy parse_rank (char '/')) -- Hmmm. TODO
 

to_vector_with_length : (n : Nat) -> List a -> Maybe (Vect n a)
to_vector_with_length Z xs =
    Just []
to_vector_with_length (S k) [] =
    Nothing
to_vector_with_length (S k) (x :: xs) =
    [| pure x :: to_vector_with_length k xs |]

to_vectors : List (List Square) -> Maybe Board
to_vectors lists =
  do inner_vecs <- sequence (map (to_vector_with_length 12) lists)
     outer_vecs <- to_vector_with_length 12 inner_vecs
     pure outer_vecs

||| Construct a Board corresponding to @forsythe (if valid)
|||
||| @for - textual representation of a position, as produced by Board.forsythe
abstract from_forsythe : (for : String) -> Maybe Board
from_forsythe for = case parse (parse_ranks) for of
  Left _ => Nothing
  Right lists => case lists of
        []       => Nothing
        [] :: xs => case init' xs of
          Nothing => Nothing
          Just ys => to_vectors ys
        _        => Nothing
    
-}
    
 
