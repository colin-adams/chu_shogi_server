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

||| Time controls and time-management
module Time

||| Per-game Time-management rules
public export data Time_control =
  ||| Classic FIDE chess controls - n moves in m minutes, with time carry-over
  ||| Arguments are moves and seconds-per-period
  Classic_chess Nat Nat |
  ||| Amateur Japanese style - fixed time for a game, plus a fixed time for every move as overtime
  ||| arguments are seconds-remaining and number of seconds per move in byo-yomi
  Simple_byo_yomi Nat Nat |
  ||| Professional-style byo-yomi - only complete minutes are counted
  ||| arguments is minutes remaining
  Professional_byo_yomi Nat |
  No_time_limits

||| Per-player time control information
export record Remaining_time where
  constructor Make_time_controls
  ||| Only used for Classic_chess
  remaining_moves : Nat
  ||| Only used for Professional_byo_yomi
  remaining_minutes : Nat
  ||| Even used for Professional_byo_yomi, as we have to update each second so that we know when to deduct a minute
  remaining_second : Nat
  
||| Remaining time at start of game
export initial_time : Time_control -> Remaining_time
initial_time tc = case tc of
  Classic_chess mv sec         => Make_time_controls mv 1 sec
  Simple_byo_yomi rem per_move => Make_time_controls 1 1 rem
  Professional_byo_yomi min    => Make_time_controls 1 min 60
  No_time_limits               => Make_time_controls 1 1 1
  

  

  
       

