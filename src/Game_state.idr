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
import Data.Vect
import Data.AVL.Dict
import Board
import Move_state
import Stack
import Move
import Piece
import Coordinate
import Forsythe_parser
import Effect.File
import Effects

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
|||
||| board to which move was applied
||| state to which previous move was applied
||| previous victory conditions
||| previous move (Nothing is the oldest entry - the starting position)
||| future moves (for take-back handling)
Move_stack : Type
Move_stack = Stack (Board, Move_state, King_count, (Maybe Move), (Stack Move))


||| Why is the game not running?
public data Termination_reason = Not_yet_started | Black_won | White_won | Draw

||| The top-level state of the game. 
public data Game_state = 
  Running Board Move_state King_count Move_stack | 
  Not_running Termination_reason

||| New game prior to determination of handicap
abstract new_game_state : Game_state
new_game_state = Not_running Not_yet_started

||| Winning conditions criteria from board
king_count_from_board : Board -> King_count
king_count_from_board b = Make_king_count (white_king_count b) (black_king_count b) (non_king_count b)

||| Initial game state after handicap decided
|||
||| @file_name - name of Forsythe file for handicap set-up
partial abstract initial_game_state : (file_name : String) -> Eff (Either String Game_state) [FILE_IO ()]
initial_game_state file_name = do
  ei <- Effect.File.Default.readFile file_name
  case ei of
    Left e  => pure $ Left $ "Could not read " ++ file_name
    Right c => case from_forsythe c of
      Nothing => pure $ Left $ "failed to parse contents of " ++ file_name ++ " as Forsythe notation"
      Just b  => do
        let blk_to_move  = isInfixOf "EVEN" file_name
        let moves_played = if blk_to_move then Z else 1
        let posn         = insert (forsythe b) 1 empty
        let wht_posns    = if blk_to_move then empty else posn
        let blk_posns    = if blk_to_move then posn else empty
        let kc           = king_count_from_board b -- TODO really we ought to have two different boards. The even start and the particular handicap start
        let mv_state     = Make_move_state True 0 False empty empty
        let mv_state'    = Make_move_state blk_to_move moves_played False wht_posns blk_posns
        let stk          = pushS (b, mv_state, kc, Nothing, mkStack) mkStack
        pure $ Right $ Running b mv_state' kc stk
     
||| Concrete state for a game
abstract data Chu_game : Game_state -> Type

||| New state from passing by @p on @c1 using empty square @c2 on @bd in @mv_st with @kc and @stk
|||
||| @p - the piece that passes
||| @c1 - location of @p
||| @c2 - neighbouring empty square
||| @bd - the old position
||| @mv_st - the old move state
||| @kc - the old victory conditions
||| @stk the old move stack
updated_state_from_pass : (p : Piece) -> (c1 : Coordinate) -> (c2 : Coordinate) -> (bd : Board) ->  (mv_st : Move_state) -> (kc : King_count) -> (stk : Move_stack) -> (Board, Move_state, King_count, Move_stack)
updated_state_from_pass p c1 c2 bd mv_st kc stk = let mv_st' = next_move mv_st False (forsythe bd)
                                                      mv     = Pass p c1 c2
                                                      stk'   = pushS (bd, mv_st, kc, (Just mv), empty) stk
                                                  in (bd, mv_st', kc, stk')
 
||| New state from capturing @p2 on @c2 without moving by @p on @c1 on @bd in @mv_st with @kc and @stk
|||
||| @p1 - the piece that captures
||| @c1 - location of @p1
||| @p2 - the piece that is captured
||| @c2 - location of @p2
||| @bd - the old position
||| @mv_st - the old move state
||| @kc - the old victory conditions
||| @stk the old move stack                                                                       
updated_state_from_igui : (p1 : Piece) -> (c1 : Coordinate) -> (p2 : Piece) -> (c2 : Coordinate) -> (bd : Board) ->  (mv_st : Move_state) -> (kc : King_count) -> (stk : Move_stack) -> (Board, Move_state, King_count, Move_stack)                                                   
updated_state_from_igui p1 c1 p2 c2 bd mv_st kc stk = let bd'    = without_piece_at c2 bd
                                                          kc'    = king_count_from_board bd'
                                                          mv     = Igui p1 c1 p2 c2
                                                          stk'   = pushS (bd, mv_st, kc, (Just mv), empty) stk 
                                                          mv_st' = next_move mv_st False (forsythe bd') 
                                                      in (bd', mv_st', kc', stk')

||| Was @p1 a non-lion and was either @p2 or @p3 a lion?
|||
||| @p1 - Piece performing capture
||| @p2 - first piece captured
||| @p3 - possible second captured piece
was_lion_capture : (p1 : Piece) -> (p2 : Piece) -> (p3 : Maybe Piece) -> Bool
was_lion_capture p1 p2 p3 = case is_lion (piece_type p1) of
  True  => False
  False => case is_lion (piece_type p2) of
    True  => True
    False => case p3 of
      Nothing  => False
      Just p3' => is_lion (piece_type p3')

||| New state from capturing @p2 on @c2 by @p on @c1 and then moving onto @c3, possibly capturing @p3 there, on @bd in @mv_st with @kc and @stk
|||
||| @p1 - the piece that captures
||| @c1 - location of @p1
||| @p2 - the first piece that is captured
||| @c2 - location of @p2
||| @p3 - possible piece @c3
||| @c3 - destination
||| @bd - the old position
||| @mv_st - the old move state
||| @kc - the old victory conditions
||| @stk the old move stack                                                                       
updated_state_from_double_move : (p1 : Piece) -> (c1 : Coordinate) -> (p2 : Piece) -> (c2 : Coordinate) -> (p3 : Maybe Piece) -> (c3 : Coordinate) -> (bd : Board) ->  (mv_st : Move_state) -> (kc : King_count) -> (stk : Move_stack) -> (Board, Move_state, King_count, Move_stack)  
updated_state_from_double_move p1 c1 p2 c2 p3 c3 bd mv_st kc stk = let bd'    = with_piece_at (p1, No_promotion) c3 (without_piece_at c2 (without_piece_at c1 bd))
                                                                       kc'    = king_count_from_board bd'
                                                                       mv     = Double_move p1 c1 p2 c2 p3 c3
                                                                       stk'   = pushS (bd, mv_st, kc, (Just mv), empty) stk 
                                                                       mv_st' = next_move mv_st (was_lion_capture p1 p2 p3) (forsythe bd') 
                                                                   in (bd', mv_st', kc', stk')


||| New state from capturing @p2 on @c2 by @p on @c1 with promotion status given by @pr @dec  on @bd in @mv_st with @kc and @stk
|||
||| @p1 - the piece that captures
||| @c1 - location of @p1
||| @p2 - the piece that is captured
||| @c2 - location of @p2
||| @pr - does the piece promte?
||| @dec - does the piece decline to promote?
||| @bd - the old position
||| @mv_st - the old move state
||| @kc - the old victory conditions
||| @stk the old move stack                                                                       
updated_state_from_capture : (p1 : Piece) -> (c1 : Coordinate) -> (p2 : Piece) -> (c2 : Coordinate) -> (pr : Bool) -> (dec : Bool) -> (bd : Board) ->  (mv_st : Move_state) -> (kc : King_count) -> (stk : Move_stack) -> (Board, Move_state, King_count, Move_stack)  
updated_state_from_capture p1 c1 p2 c2 pr dec bd mv_st kc stk =  let pr_st = promotion_status_at c1 bd
                                                                     (p, pr_st') = promoted_piece p1 pr_st pr dec
                                                                     bd'    = with_piece_at (p, pr_st') c2 (without_piece_at c1 bd)
                                                                     kc'    = king_count_from_board bd'
                                                                     mv     = Capture p1 c1 p2 c2 pr dec
                                                                     stk'   = pushS (bd, mv_st, kc, (Just mv), empty) stk 
                                                                     mv_st' = next_move mv_st (was_lion_capture p1 p2 Nothing) (forsythe bd') 
                                                                 in (bd', mv_st', kc', stk')


||| New state from moving to @c2 by @p on @c1 with promotion status given by @pr @dec  on @bd in @mv_st with @kc and @stk
|||
||| @p1 - the piece that captures
||| @c1 - location of @p1
||| @c2 - destination
||| @pr - does the piece promte?
||| @dec - does the piece decline to promote?
||| @bd - the old position
||| @mv_st - the old move state
||| @kc - the old victory conditions
||| @stk the old move stack                                                                       
updated_state_from_simple_move : (p1 : Piece) -> (c1 : Coordinate) -> (c2 : Coordinate) -> (pr : Bool) -> (dec : Bool) -> (bd : Board) ->  (mv_st : Move_state) -> (kc : King_count) -> (stk : Move_stack) -> (Board, Move_state, King_count, Move_stack)  
updated_state_from_simple_move p1 c1 c2 pr dec bd mv_st kc stk = let pr_st = promotion_status_at c1 bd
                                                                     (p, pr_st') = promoted_piece p1 pr_st pr dec
                                                                     bd'    = with_piece_at (p, pr_st') c2 (without_piece_at c1 bd)
                                                                     mv     = Simple_move p1 c1 c2 pr dec
                                                                     stk'   = pushS (bd, mv_st, kc, (Just mv), empty) stk 
                                                                     mv_st' = next_move mv_st False (forsythe bd') 
                                                                 in (bd', mv_st', kc, stk')                                                          

||| New state from applying @mv to @bd
|||
||| @mv - the move made to update the position
||| @bd - the old position
||| @mv_st - the old move state
||| @kc - the old victory conditions
||| @stk the old move stack
updated_state_from_move : (mv : Move) -> (bd : Board) -> (mv_st : Move_state) -> (kc : King_count) -> (stk : Move_stack) -> (Board, Move_state, King_count, Move_stack)
updated_state_from_move mv bd mv_st kc stk = case mv of
  Pass p c1 c2                  => updated_state_from_pass p c1 c2 bd mv_st kc stk
  Igui p1 c1 p2 c2              => updated_state_from_igui p1 c1 p2 c2 bd mv_st kc stk
  Double_move p1 c1 p2 c2 p3 c3 => updated_state_from_double_move p1 c1 p2 c2 p3 c3 bd mv_st kc stk
  Capture p1 c1 p2 c2 pr dec    => updated_state_from_capture p1 c1 p2 c2 pr dec bd mv_st kc stk
  Simple_move p c1 c2 pr dec    => updated_state_from_simple_move p c1 c2 pr dec bd mv_st kc stk

||| New board and game state resulting from applying @move to @board over a running state
|||
||| @mv - the move made to update the position
||| @bd - the old position
||| @mv_st - the old move state
||| @kc - the old victory conditions
||| @stk the old move stack
new_move_state : (mv : Move) -> (bd : Board) -> (mv_st : Move_state) -> (kc : King_count) -> (stk : Move_stack) -> (Board, Game_state)
new_move_state mv bd mv_st kc stk = let (bd', mv_st', kc', stk') = updated_state_from_move mv bd mv_st kc stk
                                    in case finished kc' of
                                      True  => if black_kings kc' == FZ then
                                                  (bd', Not_running White_won)
                                               else
                                                 if white_kings kc' == FZ then
             	                                   (bd', Not_running Black_won)
                                                 else
                                                   (bd', Not_running Draw)
                                      False => (bd', Running bd' mv_st' kc' stk')

||| New board and game state resulting from applying @move to @board in @state
|||
||| @move - the move to apply.
||| @board - the position to be updated
||| @state - the existing state of the game
abstract updated_by_move : (move : Move) -> (board : Board) -> (state : Game_state) -> (Board, Game_state)
updated_by_move m b st = case st of
  Not_running r => case r of
    Not_yet_started => let mv_st = initial_move_state
                           kc    = (king_count_from_board b)
                       in new_move_state m b mv_st kc empty
    _               => (b, st)
  Running _ mv_st kc stk => new_move_state m b mv_st kc stk
  

