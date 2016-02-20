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

module Move_state

import Data.AVL.Dict

%default total
%access private

||| State that determines what moves will be legal
public record Move_state where
  constructor Make_move_state
  ||| Is black to play?
  black_to_play : Bool
  ||| How many moves have been played so far?
  moves_played : Nat
  ||| Was the last move a capture of a lion by a non-lion
  last_move_lion_by_non_lion : Bool
  ||| Forsythe of seen positions for white (keys) with count seen (value)
  white_positions : Dict String Nat
  ||| Forsythe of seen positions for black (keys) with count seen (value)
  black_positions : Dict String Nat

||| State of a newly started game prior to handicap set-up
abstract initial_move_state : Move_state
initial_move_state = Make_move_state True 0 False empty empty

||| Update positions with a new position
updated_positions : Dict String Nat -> String -> Dict String Nat
updated_positions d for = case lookup for d of
  Nothing => insert for 1 d
  Just v_  => update for (+ 1) d

||| new state from @old_state if @ln is lion capture by non-lion with new position @forsythe
|||
||| @old_state - previous state prior to move
||| @ln - Is a non-lion capturing a lion?
||| @forsythe - new position
abstract next_move : (old_state : Move_state) -> (ln : Bool) -> (forsythe : String) -> Move_state
next_move old_state ln for = let btp = not (black_to_play old_state)
                                 cnt = (moves_played old_state) + 1
                                 pns = if btp then
                                          ((updated_positions (black_positions old_state) for), white_positions (old_state))
                                       else
                                         (black_positions (old_state), (updated_positions (white_positions old_state) for))
                             in Make_move_state btp cnt ln (snd pns) (fst pns)
                                   
