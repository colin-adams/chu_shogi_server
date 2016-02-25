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

%access private

historical_game_1_test : String -> Eff () [FILE_IO (), STDIO]
historical_game_1_test file_name = 
  do
    ei <- parse_csg file_name
    case ei of
      Left e  => putStrLn $ "Failed: " ++ e
      Right g => putStrLn "Passed"
      
||| Test setting up a correct initial position from a .fsy file
abstract test_historical_game_1 : IO ()
test_historical_game_1 = run (historical_game_1_test "../files/hist1.csg")


  
