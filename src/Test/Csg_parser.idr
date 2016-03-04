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

||| Tests of ../Csg_parser.idr
module Test.Csg_parser

import Csg_parser
import Effect.File
import Effect.StdIO
import Effects
import Game_state
import Handicaps
import Move_state
import Board
import Data.AVL.Dict

historical_game_test : String -> Eff () [FILE_IO (), STDIO]
historical_game_test file_name = 
  do
    map <- handicaps_map "../files"
    ei <- parse_csg_file file_name map
    case ei of
      Left e  => putStrLn $ "Failed: " ++ e
      Right g => do
        putStrLn "Passed"
        case g of
          (Not_running _) => putStrLn "\nOops\n"
          (Running b _ _ _ ) => putStrLn $ "\n" ++ (display_board b)
      
||| Test setting up a correct final position from a .csg file
export test_historical_game_1 : IO ()
test_historical_game_1 = run (historical_game_test "../files/hist1.csg")

      
||| Test setting up a correct final position from a .ssg file
export test_historical_game_2 : IO ()
test_historical_game_2 = run (historical_game_test "../files/hist2.csg")


  
