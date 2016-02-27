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

||| Directions on the board - North corresponds to White pieces moving up the board
module Direction

%default total
  
||| Directions of movement (North is the direction the Black player faces when seated at the board)
public export data Direction = North
               | East
               | South
               | West
               | North_east
               | South_east
               | South_west
               | North_west
         
public export Eq Direction where
  North == North = True
  South == South = True
  West == West = True
  East == East = True
  North_east == North_east = True
  South_east == South_east = True
  South_west == South_west = True
  North_west == North_west = True
  _ == _ = False

public export Show Direction where
  show North = "N"
  show South = "S"
  show East = "E"
  show West = "W"
  show North_east = "NE"
  show North_west = "NW"
  show South_east = "SE"
  show South_west = "SW"
    
||| Reverse direction 
export opposite_direction : Direction -> Direction
opposite_direction d = case d of
  North => South
  South => North
  East => West
  West => East
  North_west => South_east
  North_east => South_west
  South_east => North_west
  South_west => North_east
 
