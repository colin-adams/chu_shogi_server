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

||| Generate candidate moves - lion capture rules, promotion rules and repetition are not considered for validity
module Move_generator

import Data.Vect
import Board
import Move
import Piece
import Coordinate

%default total

||| Can @piece jump from @source to capture on @target?
can_jump_to_without_reason : Piece -> Coordinate -> Occupied_location -> Bool
can_jump_to_without_reason p c occ = case occ of
  ((c2, b), _) => fst $ can_jump_to p c b c2

||| Can @piece range from @source to capture on @target?
can_range_to_without_reason : Piece -> Coordinate -> Occupied_location -> Bool
can_range_to_without_reason p c occ = case occ of
  ((c2, b), _) => fst $ can_range_to p c b "" c2

||| Capture (single) move from @source to @target
|||
||| @source - starting square and piece for move
||| @promote - is the piece promoting?
||| @decline - is the piece declining the chance to promote?
||| @target - destination of move where opposing piece is captured
capture_from : (source : Occupied_location) -> (promote : Bool) -> (decline : Bool) -> (target : Occupied_location) -> Move
capture_from source promote decline target = case source of
  ((c1, b), p1) => case target of
    ((c2, _), p2) => Capture p1 c1 p2 c2 promote decline

||| List of capture moves all starting from @source and ending on each of @targets
|||
||| TODO type as output list of three times length of input list (all possibilities for promotion are considered)
||| @source - starting square and piece for all moves
||| @targets - destinations for moves where opposing pieces are captured
captures : (source : Occupied_location) -> (targets : List Occupied_location) -> List Move
captures source targets = (map (capture_from source True False) targets) ++ (map (capture_from source False True) targets) ++
  (map (capture_from source False False) targets)

||| Can non-Lion @piece at @source make a lion-power capture to @target?
can_use_lion_power_to : Piece -> Coordinate -> Occupied_location -> Bool
can_use_lion_power_to p c occ = case occ of
 ((c2, b), _) => case has_lion_a_to p c c2 of
   (True, _) => True 
   _         => fst $ has_lion_b_to p c c2
  
||| Double-capture move by piece at @source capturing @piece at @intermediate then capturing on @target
double_capture_from : (source : Occupied_location) -> (prisoner : Piece) -> (intermediate : Coordinate ) -> (target : Occupied_location) -> Move
double_capture_from ((c, b), p) p2 c2 ((c3, b2), p3) = Double_move p c p2 c2 (Just p3) c3

||| Capture-and-move by piece at @source capturing @piece at @intermediate then moving to @destination
capture_and_move_from : (source : Occupied_location) -> (prisoner : Piece) -> (intermediate : Coordinate ) -> (destination : Coordinate) -> Move
capture_and_move_from ((c, b), p) p2 c2 c3 = Double_move p c p2 c2 Nothing c3

||| Capture by piece at @source of @prisoner on @intermediate then return to @source
igui_from : (source : Occupied_location) -> (prisoner : Piece) -> (intermediate : Coordinate ) -> Move
igui_from ((c, _), p) p2 c2 = Igui p c p2 c2

||| Is @target one square away (any principal direction) from @source?
distance_of_one : (target : Coordinate) ->  (source : Occupied_location) -> Bool
distance_of_one c2 ((c1, _), _) = if (distance_between c1 c2) == 1 then True else False

||| Double captures and capture-and-move-on from @source to @target and then to an adjacent square
double_captures_from : (source : Occupied_location) -> (target : Occupied_location) -> List Move
double_captures_from source target = case source of
  ((c1, b), p1) => case target of
    ((c2, _), p2) => case distance_between c1 c2 of
      S Z => let occupied_squares = filter (distance_of_one c2) (pieces_of_colour (piece_colour p2) b)
                 free_squares = empty_squares_adjacent_to_except c2 c1 b
             in (map (double_capture_from source p2 c2) occupied_squares) ++ (map (capture_and_move_from source p2 c2) free_squares) ++
               [igui_from source p2 c2]
      _   => []

||| Double captures and capture-and-move-on plus igui captures from @source to @target and then to an adjacent square or back
lion_double_captures_from : (source : Occupied_location) -> (target : Occupied_location) -> List Move
lion_double_captures_from ((c1, b), p) ((c2, b2), p2) =  case distance_between c1 c2 of
  S Z => let occupied_squares = filter (distance_of_one c2) (pieces_of_colour (piece_colour p2) b)
             free_squares = empty_squares_adjacent_to_except c2 c1 b 
         in (map (double_capture_from ((c1, b), p) p2 c2) occupied_squares) ++ (map (capture_and_move_from ((c1, b), p) p2 c2) free_squares) ++
           [igui_from ((c1, b), p) p2 c2]    
  _   => []
  
||| All possible direct and double captures from piece at @source to all of @targets
lion_power_captures :  (source : Occupied_location) -> (targets : List Occupied_location) -> List Move
lion_power_captures source targets = (map (capture_from source False False) targets) ++ (concatMap (lion_double_captures_from source) targets)
 
||| Is @piece on @source within two squares of @target?
is_lion_target : Piece -> Coordinate -> Occupied_location -> Bool
is_lion_target p c ((c2, b), p2) = let d = distance_between c c2
                                   in if d == 1 then True else if d == 2 then True else False

||| TODO - how does this differ from lion_power_captures?? 
lion_captures : (source : Occupied_location) -> (targets : List Occupied_location) -> List Move
lion_captures source targets = (map (capture_from source False False) targets) ++ (concatMap (lion_double_captures_from source) targets)

||| Generate all captures from @source to @targets on @bd (moves may not necessarily be valid for lion capture rules and repetition)
|||
||| @targets - squares occupied by opposing pieces
||| @source  - piece to move
generate_captures_to : (targets : List Occupied_location) -> (source : Occupied_location) -> List Move
generate_captures_to targets source = case source of
  ((c, b), p) => let jump_targets         = filter (can_jump_to_without_reason p c) targets
                     range_targets        = filter (can_range_to_without_reason p c) targets 
                     eagle_falcon_targets = filter (can_use_lion_power_to p c) targets
                     lion_targets         = filter (is_lion_target p c) targets
    in (captures source jump_targets) ++ (captures source range_targets) ++ (lion_power_captures source eagle_falcon_targets) ++
      (lion_captures source lion_targets)

||| Generate all captures on @bd by pieces of @colour (moves may not necessarily be valid for lion capture rules and repetition)
|||
||| @bd     - position under consideration
||| @colour - Colour of pieces to move
export generate_captures : (bd : Board) -> (colour : Piece_colour) -> List Move
generate_captures b col = let sources = pieces_of_colour col b
                              targets = pieces_of_colour (opposite_colour col) b
  in concatMap (generate_captures_to targets) sources

