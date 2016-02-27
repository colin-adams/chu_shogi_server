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

||| Classification of types of move to a destination square
module Lighting

import Coordinate

%default total

||| Type of move possible onto a square. A GUI can assign a different colour to each type when showing plausible (i.e. legal before considering repetition) moves
public export data Lighting_colour =
       ||| Ordinary step or ranging move
       Step_lighting |
       ||| Jump move
       Jump_lighting |
       ||| Second square or direct jump for lion-type moves
       Lion_b_lighting |
       ||| Lion-type move to adjacent square
       Lion_a_lighting |
       ||| Ordinary step or ranging capture
       Step_capture |
       ||| Jump capture
       Jump_capture |
       ||| Lion-type capture on the second part of the move or direct jump
       Lion_b_capture |
       ||| Lion-type capture on an adjacetn square
       Lion_a_capture
 
