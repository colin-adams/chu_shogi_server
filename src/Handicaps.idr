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
import Effect.State
import Forsythe_parser
import Piece
import Board
import Move_state
import Move
import Data.Stack

%default total

||| Handicap names paired with file-names of forsythe of starting position
handicap_names_and_positions : List (String, String)
handicap_names_and_positions = [("Free King", "/FK.fsy"), ("Free King (alternate)", "/FKa.fsy"), ("Free King and a Dragon King", "/FK+DK.fsy")]

||| Names of all handicaps known to the server
export available_handicap_names : List (String)
available_handicap_names = map fst handicap_names_and_positions


||| Transform @name_and_file_name to a name and corresponding position
|||
||| @directory          - path-name of directory containing .fsy files
||| @name_and_file_name - handicap name and file-name for corresponding position
partial read_handicap : (directory : String) -> (name_and_file_name : (String, String)) -> Eff (String, String) [FILE_IO ()]
read_handicap dir (nm, fnm) = do
  ei <- Effect.File.Default.readFile (dir ++ fnm)
  case ei of
    Left e  => pure (nm, "")
    Right c => pure (nm, c)
          
||| Transform @inputs from a list of handicap names and corresponding position-file-names to a list of handicap names and corresponding positions
|||
||| @directory - path-name of directory containing .fsy files
||| @names     - handicap names and corresponding position-file-names
partial read_handicaps : (directory : String) -> (names : List (String, String)) -> Eff (List (String, String)) [FILE_IO ()]
read_handicaps dir names = mapE (\x => read_handicap dir x) names
  
||| Add handicap named and given in @name_and_contents to @dictionary
||| 
||| @dictionary        - handicaps to be added to
||| @name_and_contents - handicap name paired with starting position
add_handicap  :  (name_and_contents : (String, String)) -> (dictionary : Dict String Game_state) -> Dict String Game_state
add_handicap (hdcp, cont) dict =
  case from_forsythe cont of
    Left e => dict -- TODO error reporting?
    Right b => let posn = insert (forsythe b) 1 empty
                   wht_posns = posn
                   blk_posns = empty
                   kc        = king_count_from_board b
                   mv_state  = Make_move_state False 1 False empty empty
                   mv_state' = Make_move_state False 1 False wht_posns blk_posns
                   stk       = pushS (b, mv_state, kc, Nothing, mkStack) mkStack
               in insert hdcp (Running b mv_state' kc stk) dict
                   
||| Read map of handicap names to initial game states from @directory
|||
||| @directory - physical location for .fsy files describing handicap positions
partial export handicaps_map : (directory : String) -> Eff () [FILE_IO(), STATE (Dict String Game_state)]
handicaps_map dir = do
  put empty
  ei <- Effect.File.Default.readFile (dir ++ "/EVEN.fsy")
  case ei of
      Left e  => pure ()
      Right c => case from_forsythe c of
        Left e  => pure ()
        Right b => let posn = insert (forsythe b) 1 empty
                       blk_posns = posn
                       wht_posns = empty
                       kc        = king_count_from_board b
                       mv_state  = Make_move_state True 0 False empty empty
                       mv_state' = Make_move_state True 0 False wht_posns blk_posns
                       stk       = pushS (b, mv_state, kc, Nothing, mkStack) mkStack
                       even_dict = insert "EVEN" (Running b mv_state' kc stk) empty
                   in do
                     hdcps <- read_handicaps dir handicap_names_and_positions
                     put even_dict
                     _ <- mapE (\x => Effect.State.update (add_handicap x)) hdcps
                     dict <- get
                     pure ()


