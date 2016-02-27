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

||| Inverse of Board.forsythe - this parses George Hodges' Forsythe notation for Chu Shogi, with the addition of a prefix = to indicate deferred promotion
module Forsythe_parser

import Lightyear
import Lightyear.Strings
import Lightyear.Char
import Board
import Piece

%default total

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
  pure $ join lists

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
export from_forsythe : (for : String) -> Either String Board
from_forsythe for = case parse (parse_ranks) for of
  Left e      => Left e
  Right lists => case lists of
        []       => Left "No ranks"
        [] :: xs => case init' xs of
          Nothing => Left "No ranks"
          Just ys => case to_vectors ys of
            Nothing => Left "Strange board geometry"
            Just b  => Right b
        _        => Left "Missing leading / (?)"
    

    
 
