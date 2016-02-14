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
  ||| Create new move stack
  |||
  ||| board to which move was applied
  ||| state to which previous move was applied
  ||| previous victory conditions
  ||| previous move (Nothing is the oldest entry - the starting position)
  ||| future moves (for take-back handling)
  Make_stack Board Move_state King_count (Maybe Move) (Stack Move)

||| Why is the game not running?
public data Termination_reason = Not_yet_started | Black_won | White_won | Draw

||| The top-level state of the game. 
public data Game_state = 
  Running Board Move_state King_count Move_stack | 
  Not_running Termination_reason

||| Concrete state for a game
abstract data Chu_game : Game_state -> Type

