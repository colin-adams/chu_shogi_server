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

||| Logic for dealing with handicaps
module Handicaps

import Data.AVL.Dict
import Data.Vect
import Game_state
import Effects
import Effect.File
import Forsythe_parser
import Piece
import Board
import Move_state
import Move
import Data.Stack

%default total

||| Read map of handicap names to initial game states from @directory
|||
||| -- TODO add other handicaps
||| @directory - physical location for .fsy files describing handicap positions
partial export handicaps_map : (directory : String) -> Eff (Dict String Game_state ) [FILE_IO()]
handicaps_map dir = do
  ei <- Effect.File.Default.readFile (dir ++ "/EVEN.fsy")
  case ei of
      Left e  => pure empty
      Right c => case from_forsythe c of
        Left e  => pure empty
        Right b => let posn = insert (forsythe b) 1 empty
                       blk_posns = posn
                       wht_posns = empty
                       kc        = king_count_from_board b
                       mv_state  = Make_move_state True 0 False empty empty
                       mv_state' = Make_move_state True 0 False wht_posns blk_posns
                       stk       = pushS (b, mv_state, kc, Nothing, mkStack) mkStack
                   in pure $ insert "EVEN" (Running b mv_state' kc stk) empty
