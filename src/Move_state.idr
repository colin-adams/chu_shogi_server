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


