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

||| Structures that contain the entire state of a game of Chu Shogi
module Game_state
  -- This does not include any state used by a specific client, such as interactive state

import Data.Fin
import Board
import Move_state
import Stack
import Move
import Piece
import Coordinate               

%default total
%access private

||| Data for calculating winning condition
public record King_count where
  constructor Make_king_count
  ||| white_kings = FZ --> White has lost. Ditto for Black
  white_kings, black_kings : Fin 3
  ||| At least two pieces must be on the board. At the start of the game there are 94 additional pieces (for standard handicaps)
  non_kings : Fin 95

||| Number of pieces on the board
total_pieces : King_count -> Nat
total_pieces kc = finToNat (non_kings kc) + 2

-- Has the game finished with a win for either side or a draw (may want to change the Bool to a finish-type)?
finished : King_count -> Bool
finished kc = case white_kings kc of
  FZ => True -- Black wins
  _ => case black_kings kc of
       FZ => True -- White wins
       _ => case non_kings kc of
         FZ => True -- draw - an agree_draw procedure can simply reduce non_kings to zero
         _ => False

||| History of moves made and the associated state. Needed for repetition rule.
public data Move_stack =
  Make_stack Board Move_state King_count (Maybe Move) (Stack Move)

||| Why is the game not running?
public data Termination_reason = Not_yet_started | Black_won | White_won | Draw

||| The top-level state of the game. 
public data Game_state = Running Board Move_state King_count Move_stack | Not_running Termination_reason

||| Concrete state for a game
data Chu_game : Game_state -> Type

||| Is piece @source on @board same as @piece?
is_same_piece_at : (piece: Piece) -> (source: Coordinate) -> (board : Board) -> Bool
is_same_piece_at p c b = case piece_at c b of
  Nothing => False
  Just (p', _) => p' == p


||| Is it valid for a Lion (or a promoted Kylin) @c1 to capture @p2 on @c2 then move to @c3 (capturing if occupied by @p2)?
|||
||| Note that repetition cannot occur. The lion double-capture rule is not checked, as it is dependent upon the board (protection).
||| It is assumed that the moving lion is of the opposite colour to @p2.
||| We give a reason why the move is not valid
is_valid_lion_double_move_stage_1 : Coordinate -> Piece -> Coordinate -> Maybe Piece -> Coordinate -> (Bool, String)
is_valid_lion_double_move_stage_1 c1 p2 c2 p3 c3 = let col = opposite_colour (piece_colour p2) in
  case (direction_and_range col c1 c2) of
    Just (d, r) => case r of
        SZ => case (direction_and_range col c2 c3) of
          Just (d', r') => case r' of
            S Z => if opposite_direction d == d' then
                (False, "Igui distinct from other double moves")
              else
                case p3 of
                  Nothing => (True, "")
                  Just p3' => if piece_colour p3' == col then
                                 (False, "Second captured piece is of same colour as moving piece")
                              else
                                (True, "")
            _   => (False, "Destination square of double move must be only 1 square away from intermediate square")
          Nothing       => (False, "Destination square is not on an orthogonal or diagonal line from the intermediate square")
        _  => (False, "Intermediate square imust be one square from starting square")
    _           => (False, "Intermediate square imust be one square from starting square")
 
||| Is it valid for a Soaring Eagle @c1 to capture @p2 on @c2 then move to @c3 (capturing if occupied by @p2)?
|||
||| Note that repetition cannot occur. The lion capture rule is not checked, as it is dependent upon the board (protection).
||| It is assumed that the moving soaring eagle is of the opposite colour to @p2.
||| We also return a validity message
is_valid_soaring_eagle_double_move_stage_1 : Coordinate -> Piece -> Coordinate -> Maybe Piece -> Coordinate -> (Bool, String)
is_valid_soaring_eagle_double_move_stage_1 c1 p2 c2 p3 c3 = let col = opposite_colour (piece_colour p2) in
                                                                case (direction_and_range col c1 c2) of
                                                                  Just (North_east, r) => 
                                                                    case (direction_and_range col c2 c3) of
                                                                      Just (North_east, r2) => case (r, r2) of
                                                                          (S Z, SZ) => case p3 of
                                                                            Nothing  => (True, "")
                                                                            Just p3' => if piece_colour p3' == col then
                                                                                           (False, "Can't capture a piece on the final square of the same colour")
                                                                                        else
                                                                                          (True, "")
                                                                          _   => (False, "Final square must be one away from intermediate square")
                                                                      _                => (False, "Final square must be in the same direction as the intermediate square")
                                                                  Just (North_west, r) => 
                                                                    case (direction_and_range col c2 c3) of
                                                                      Just (North_west, r2) => case (r, r2) of
                                                                          (S Z, SZ) => case p3 of
                                                                            Nothing  => (True, "")
                                                                            Just p3' => if piece_colour p3' == col then
                                                                                           (False, "Can't capture a piece on the final square of the same colour")
                                                                                        else
                                                                                          (True, "")
                                                                          _   => (False, "Final square must be one away from intermediate square")
                                                                      _                => (False, "Final square must be in the same direction as the intermediate square")                 
                                                                  _               => (False, "Bad direction for Soaring Eagle lion move")


||| Is it valid for a Horned Falcon @c1 to capture @p2 on @c2 then move to @c3 (capturing if occupied by @p2)?
|||
||| Note that repetition cannot occur. The lion capture rule is not checked, as it is dependent upon the board (protection).
||| It is assumed that the moving horned falcon is of the opposite colour to @p2.
||| We also return a validity message
is_valid_horned_falcon_double_move_stage_1 : Coordinate -> Piece -> Coordinate -> Maybe Piece -> Coordinate -> (Bool, String)
is_valid_horned_falcon_double_move_stage_1 c1 p2 c2 p3 c3 = let col = opposite_colour (piece_colour p2) in
                                                                case (direction_and_range col c1 c2) of
                                                                  Just (North, r) => 
                                                                    case (direction_and_range col c2 c3) of
                                                                      Just (North, r2) => case (r, r2) of
                                                                          (S Z, SZ) => case p3 of
                                                                            Nothing  => (True, "")
                                                                            Just p3' => if piece_colour p3' == col then
                                                                                           (False, "Can't capture a piece on the final square of the same colour")
                                                                                        else
                                                                                          (True, "")
                                                                          _   => (False, "Final square must be one away from intermediate square")
                                                                      _                => (False, "Final square must be in the same direction as the intermediate square")                  
                                                                  _               => (False, "Bad direction for Horned Falcon lion move")

||| Is @m valid for game state, ignoring lion capture rules and repetition_rule?
|||
||| A reason is given why validity fails
is_valid_move_stage_1 : (m : Move) -> (Game_state) -> (Bool, String)
is_valid_move_stage_1 m (Not_running _) = (False, "Game not in progress")
is_valid_move_stage_1 m (Running b _ _ _) = case m of
  Pass p c1 c2                  => case is_same_piece_at p c1 b of
    False => (False, "Selected piece is not piece on starting square")
    True  => case piece_at c2 b of
      Just _  => (False, "Can't pass when target square is occupied")
      Nothing => case piece_at c1 b of
        Nothing => (False, "Starting square lacks a piece")
        Just (p2, _) => case is_lion (piece_type p2) of
          True  => (True, "")
          False => has_lion_a_to p2 c1 c2 
  Igui p c1 p2 c2              => case is_same_piece_at p c1 b of
    False => (False, "Selected piece is not piece on starting square")
    True  => case piece_at c2 b of
      Nothing => (False, "No piece on target square to capture")
      Just (p1, _) => case (piece_colour p1) == (piece_colour p2) of
          True  => (False, "Can't capture piece of same colour")
          False => case is_lion (piece_type p) of
            True  => (True, "")
            False => has_lion_a_to p c1 c2 
  Double_move p1 c1 p2 c2 p3 c3 => case is_same_piece_at p1 c1 b of
   False => (False, "Selected piece is not piece on starting square")
   True => case (piece_colour p1) == (piece_colour p2) of
     True  => (False, "Can't capture piece of same colour")
     False => case is_lion (piece_type p1) of                           
       True  => is_valid_lion_double_move_stage_1 c1 p2 c2 p3 c3 
       False => case piece_type p1 of
         Soaring_eagle => is_valid_soaring_eagle_double_move_stage_1 c1 p2 c2 p3 c3
         Horned_falcon => is_valid_horned_falcon_double_move_stage_1 c1 p2 c2 p3 c3
         _             => (False, "That type of piece may not make double moves")
  Simple_move p c1 c2 pr dec    =>  case is_same_piece_at p c1 b of
   False => (False, "Selected piece is not piece on starting square")
   True => case piece_at c2 b of
     Just _  => (False, "Can't make a non-capturing move to an occupied square")
     Nothing => case can_jump_to p c1 c2 b of
       (True, _)  => check_promotion c1 c2 b pr dec False 
       (False, v) => can_range_to p c1 c2 b v
  Capture p c1 p2 c2 pr dec    =>  case is_same_piece_at p c1 b of
   False => (False, "Selected piece is not piece on starting square")
   True => case piece_at c2 b of
     Nothing  => (False, "No piece on target square to capture")
     Just (p', _) => case (piece_colour p) == (piece_colour p') of
       True  => (False, "Can't capture piece of same colour")
       False => case can_jump_to p c1 c2 b of
         (True, _)  => check_promotion c1 c2 b pr dec False 
         (False, v) => can_range_to p c1 c2 b v

||| Is @m valid for @state, ignoring repetition_rule?
|||
||| True if the move is basically valid and lion capture rules are honoured
||| A reason is given why validity fails
is_valid_move_stage_2 : (m : Move) -> (state : Game_state) -> (Bool, String)
is_valid_move_stage_2 m st = case is_valid_move_stage_1 m st of
  (False, v) => (False, v)
  _          => case st of
    Not_running _         => (False, "No game")
    Running b mv_st _ stk =>  case m of
      Double_move p1 c1 p2 c2 p3 c3 => case is_lion (piece_type p1) of
        False => case (last_move_lion_by_non_lion mv_st) of -- non-lion capturing
          False => (True, "")
          True => case is_lion (piece_type p2) of 
            True  => case is_protected c2 (piece_colour p2) b of
              True  => (False, "Non-lion capture of protected lion after non-lion capture of lion")
              False => case p3 of
                Nothing  => (True, "Okazaki") -- single capture
                Just p3' => case is_lion (piece_type p3') of
                  False => (True, "")
                  True  => case is_protected c3 (piece_colour p3') b of
                    True  => (False, "Non-lion capture of protected lion after non-lion capture of lion")
                    False => (True, "Okazaki")
            False => case p3 of
              Nothing  => (True, "") -- no lions involved, single capture
              Just p3' => case is_lion (piece_type p3') of
                False => (True, "") -- no lions are involved
                True => case is_protected c2 (piece_colour p3') b of
                  True  => (False, "Non-lion capture of protected lion after non-lion capture of lion")
                  False => (True, "Okazaki")
        True  => case p3 of -- Lion capturing
          Nothing  => (True, "") -- single capture on first square
          Just p3' => case is_lion (piece_type p3') of
            False => (True, "")
            True  => case is_pawn_or_go_between p2 of
              False => (True, "")
              True  => case is_protected c3 (piece_colour p3') b of
                False => (True, "")
                True  => (False, "Cannot capture protected lion at distance 2 when first piece is pawn or go-between")
      Capture p c1 p2 c2 _ _        => case is_lion (piece_type p) of
        False => case is_lion (piece_type p2) of
          False => (True, "")
          True  => case (last_move_lion_by_non_lion mv_st) of
            False => (True, "")
            True  => case is_protected c2 (piece_colour p2) b of
              True  => (False, "Non-lion capture of protected lion after non-lion capture of lion")
              False => (True, "Okazaki")
        True  => case is_lion (piece_type p2) of -- moving lion
          False => (True, "")
          True  => case direction_and_range (piece_colour p) c1 c2 of
            Nothing => (False, "Impossible. Valid stage1 but no move to destination")
            Just (_, S Z) => (True, "") -- Ln x Ln at a distance of 1
            Just (_, S (S Z)) => case is_protected c2 (piece_colour p2) b of
              True  => (False, "Protected lion at distance 2")
              False => (True, "")
            _             =>  (False, "Impossible. Valid stage1 of lion capture but distance to destination is not 1 or 2")
      _                             => (True, "")


||| Does @position violate the repetition rule on @board considering @state?
|||
||| @board - the position we are considering AFTER moving
||| @state - the game state AFTER moving
||| @position - representation of @board in Forsythe notation
is_repetition : (board : Board) ->  (state : Game_state) -> (position : String) -> Bool
is_repetition b st pos = False -- TODO

||| New board and game state resulting from applying @move to @board in @state
|||
||| @move - the move to apply.
||| @board - the position to be updated
||| @state - the existing state of the game
updated_by_move : (move : Move) -> (board : Board) -> (state : Game_state) -> (Board, Game_state)
updated_by_move m b st = (b, st) -- TODO

||| Is @m valid for @state?
|||
||| A reason is given why validity fails
is_valid_move : (m : Move) -> (state : Game_state) -> (Bool, String)
is_valid_move m st = case is_valid_move_stage_2 m st of
  (False, v) => (False, v)
  _          => case st of
    Not_running _         => (False, "No game") -- can't happen here
    Running b _ _ _ => let (b', st') = updated_by_move m b st
                           f = forsythe b'
                       in case is_repetition b' st' f of
                         True  => (False, "repetition")
                         False => (True, "")
