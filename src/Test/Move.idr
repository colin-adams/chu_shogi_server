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

||| Tests of making moves, valid and invalid
module Test.Move

import Move
import Move_validity
import Forsythe_parser
import Data.Vect
import Effect.File
import Effect.StdIO
import Effects
import Board
import Piece
import Coordinate
import Game_state
import Move_state

||| Test playing some moves from the initial start position
export test_moves_from_start : IO ()
test_moves_from_start = do
  ei_st <- run $ initial_game_state "../files/EVEN.fsy"
  case ei_st of
    Left msg                        => putStrLn $ "Failed: " ++ msg
    Right (Not_running _)           => putStrLn "Failed: game was not started"
    Right gs@(Running b st kc stk)  => do
      let bad_move_1 = Simple_move (Make_piece Pawn White) (Make_coordinate (fromInteger 4) (fromInteger 4)) (Make_coordinate (fromInteger 4) (fromInteger 4)) False False
      case is_valid_move bad_move_1 gs of
        (True, _)  => putStrLn $ "Invalid move " ++ (show bad_move_1) ++ " was flagged as valid"
        (False, _) => do
          let good_move_1 = Simple_move (Make_piece Pawn Black) (Make_coordinate (fromInteger 8) (fromInteger 4)) (Make_coordinate (fromInteger 7) (fromInteger 4)) False False
          case is_valid_move good_move_1 gs of
            (False, err)  => putStrLn $ "Good move " ++ (show good_move_1) ++ " was flagged as invalid for " ++ err ++ "\n" ++ (display_board b)
            (True, _) => putStrLn "Passed"
  


  
