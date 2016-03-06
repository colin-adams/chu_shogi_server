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

||| A single contest between two players
module Game

import Game_state
import Time

||| The complete record of a single game between two players
public export record Game where
  constructor Make_game
  ||| Names of the players (usually login-ids)
  black, white : String 
  ||| state of play (affects legal moves)
  state        : Game_state
  ||| name of handicap in use
  handicap     : String
  ||| method used to time the game
  time_control : Time_control
  ||| remaining time for each player
  black_time, white_time : Remaining_time
  
  
 export new_game : (black: String) -> (white : String) -> (handicap : String) -> (control : Time_control) -> Game
 new_game blk whte hdcp control = let time = (initial_time control)
                          in Make_game blk whte (Not_running Not_yet_started) hdcp control time time
