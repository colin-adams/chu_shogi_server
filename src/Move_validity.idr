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

||| Validating a Move
module Move_validity

import Data.Vect
import Game_state
import Move_state
import Board
import Move
import Piece
import Coordinate               
import Data.AVL.Dict
import Move_generator
import Direction
import Stack
import Forsythe_parser

%default total

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
is_valid_lion_double_move_stage_1 c1 p2 c2 p3 c3 = let col = opposite_colour (piece_colour p2) 
  in
    case (direction_and_range c1 c2) of
      Just (d, r) => if r == 1 then
        case (direction_and_range c2 c3) of
          Just (d', r') => if r' == 1 then
               if opposite_direction d == d' then
               	  (False, "Igui distinct from other double moves")
               else
		case p3 of
                  Nothing => (True, "")
                  Just p3' => if piece_colour p3' == col then
                                 (False, "Second captured piece is of same colour as moving piece")
                              else
                                (True, "")
            else (False, "Destination square of double move must be only 1 square away from intermediate square")
          Nothing       => (False, "Destination square is not on an orthogonal or diagonal line from the intermediate square")
        else (False, "Intermediate square imust be one square from starting square")
      _           => (False, "Intermediate square imust be one square from starting square")
 
||| Is it valid for a Soaring Eagle @c1 to capture @p2 on @c2 then move to @c3 (capturing if occupied by @p2)?
|||
||| Note that repetition cannot occur. The lion capture rule is not checked, as it is dependent upon the board (protection).
||| It is assumed that the moving soaring eagle is of the opposite colour to @p2.
||| We also return a validity message
is_valid_soaring_eagle_double_move_stage_1 : Coordinate -> Piece -> Coordinate -> Maybe Piece -> Coordinate -> (Bool, String)
is_valid_soaring_eagle_double_move_stage_1 c1 p2 c2 p3 c3 = 
  let col = opposite_colour (piece_colour p2) 
  in case (direction_and_range c1 c2) of
    Just (dir1, r1) => case (direction_and_range c2 c3) of
      Just (dir2, r2) =>
        if r1 == 1 && r2 == 1 && dir1 == dir2 then
          let dir' = if col == Black then opposite_direction dir1 else dir1
          in if dir' == North_east || dir' == North_west then
            case p3 of
              Nothing  => (True, "")
              Just p3' => 
                if piece_colour p3' == col then
                  (False, "Can't capture a piece on the final square of the same colour")
                else
                  (True, "")
          else
            (False, "Invalid direction")
        else
          (False, "Each step must be one square and both must be in the same direction")
      _ => (False, "Second step invalid")
    _ => (False, "First step invalid")

||| Is it valid for a Horned Falcon @c1 to capture @p2 on @c2 then move to @c3 (capturing if occupied by @p2)?
|||
||| Note that repetition cannot occur. The lion capture rule is not checked, as it is dependent upon the board (protection).
||| It is assumed that the moving horned falcon is of the opposite colour to @p2.
||| We also return a validity message
is_valid_horned_falcon_double_move_stage_1 : Coordinate -> Piece -> Coordinate -> Maybe Piece -> Coordinate -> (Bool, String)
is_valid_horned_falcon_double_move_stage_1 c1 p2 c2 p3 c3 = 
  let col = opposite_colour (piece_colour p2) 
  in case (direction_and_range c1 c2) of
    Just (dir1, r1) => case (direction_and_range c2 c3) of
      Just (dir2, r2) =>
        if r1 == 1 && r2 == 1 && dir1 == dir2 then
          let dir' = if col == Black then opposite_direction dir1 else dir1
          in if dir' == North then
            case p3 of
              Nothing  => (True, "")
              Just p3' => 
                if piece_colour p3' == col then
                  (False, "Can't capture a piece on the final square of the same colour")
                else
                  (True, "")
          else
            (False, "Invalid direction")
        else
          (False, "Each step must be one square and both must be in the same direction")
      _ => (False, "Second step invalid")
    _ => (False, "First step invalid")
      
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
     Nothing => case can_jump_to p c1 b c2 of
       (True, _)  => check_promotion c1 c2 b pr dec False 
       (False, v) => case can_range_to p c1 b v c2 of
         (True, _)  => check_promotion c1 c2 b pr dec False
         (False, v') => (False, v')
  
  Capture p c1 p2 c2 pr dec    =>  case is_same_piece_at p c1 b of
   False => (False, "Selected piece is not piece on starting square")
   True => case piece_at c2 b of
     Nothing  => (False, "No piece on target square to capture")
     Just (p', _) => case (piece_colour p) == (piece_colour p') of
       True  => (False, "Can't capture piece of same colour")
       False => case can_jump_to p c1 b c2 of
         (True, _)  => check_promotion c1 c2 b pr dec True
         (False, v) => case can_range_to p c1 b v c2 of
           (True, _)  => check_promotion c1 c2 b pr dec True
           (False, v') => (False, v')
   
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
          True  => case direction_and_range c1 c2 of
            Nothing => (False, "Impossible. Valid stage1 but no move to destination")
            Just (_, d) => 
              if d == 1 then
                (True, "") -- Ln x Ln at a distance of 1
              else
                if d == 2 then
                  case is_protected c2 (piece_colour p2) b of
                    True  => (False, "Protected lion at distance 2")
                    False => (True, "")
                else
                  (False, "Impossible. Valid stage1 of lion capture but distance to destination is not 1 or 2")
      _                             => (True, "")


||| Is @m a valid move (ignoring repetition rules)?
|||
||| @state - state of the game, for lion rules
||| @m     - move under consideration
is_candidate_move : (state : Game_state) -> (m : Move) -> Bool
is_candidate_move st m = case is_valid_move_stage_2 m st of
  (res, _) => res
  
||| Number of pieces of @colour that threaten to capture (ignoring repetition) - we need validity rules - problem of cyclic imports
|||
||| @bd     - the position under consideration
||| @st     - game state (for lion capture rules)
||| @colour - colour of the attacker
export attack_count : (bd : Board) -> (colour : Piece_colour) -> (st: Game_state) -> Nat
attack_count b col st = let moves = generate_captures b col
  in length $ filter (is_candidate_move st) moves

||| Does @position violate the repetition rule on @board considering @state?
|||
||| @board - the position we are considering AFTER moving
||| @state - the game state AFTER moving
||| @position - representation of @board in Forsythe notation
is_repetition : (board : Board) ->  (state : Game_state) -> (position : String) -> Bool
is_repetition b st pos = case st of
  Not_running _ => False
  Running bd mv_st _ _  => case mv_st of
      Make_move_state blk_to_play _ _ white_forsythes black_forsythes => let positions = if blk_to_play then black_forsythes else white_forsythes
        in case lookup pos positions of
          Nothing => False
          Just n  => case n < 3 of
            True  => False -- If you haven't seen this position 3 times before, then it is legal to repeat
            False => case n >= 4 of
              True  => True -- can never make a fifth repetition
              False => let black_attacks = attack_count b Black st
                           white_attacks = attack_count b White st
                       in case (black_attacks, white_attacks) of
                         (Z, Z)     => True -- player on move must vary
                         (Z, S m)   => not blk_to_play
                         (S m, Z)   => blk_to_play
                         (S m, S l) => case compare m l of
                           EQ => True -- player on move must vary
                           GT => blk_to_play
                           LT => not blk_to_play
                           
||| Is @m valid for @state?
|||
||| A reason is given why validity fails
export is_valid_move : (m : Move) -> (state : Game_state) -> (Bool, String)
is_valid_move m st = case is_valid_move_stage_2 m st of
  (False, v) => (False, v)
  _          => case st of
    Not_running _         => (False, "No game") -- can't happen here
    Running b _ _ _ => let (b', st') = updated_by_move m b st
                           f = forsythe b'
                       in case is_repetition b' st' f of
                         True  => (False, "repetition")
                         False => (True, "")

public export data Move_validity : (Move, Game_state) -> Type where
  Make_move_validity : {x : (Move, Game_state)} -> (is_valid_move (fst x) (snd x)) = (True, _) -> Move_validity x
  
||| Type of moves that are valid in a particular situation (with the proof erased)
public export Valid_move : Type
Valid_move = Subset (Move, Game_state) Move_validity


||| New board and game state resulting from applying @move in @state
|||
||| @move - the validated move to apply.
export update_with_valid_move : (move : Valid_move) -> (Game_state)
update_with_valid_move move = let (mv, gs) = getWitness move
  in case gs of
    Not_running _   => gs -- impossible
    Running b _ _ _ => snd $ updated_by_move mv b gs


