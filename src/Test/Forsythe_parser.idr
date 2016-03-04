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

||| Tests of ../Forsythe_parser.idr
module Test.Forsythe_parser

import Forsythe_parser
import Effect.File
import Effect.StdIO
import Effects
import Board
import Data.Vect
import Piece

start_position_test : String -> Eff () [FILE_IO (), STDIO]
start_position_test file_name = 
  do
    ei <- Effect.File.Default.readFile file_name
    case ei of
      Left e  => putStrLn $ "Failed to read or open " ++ file_name 
      Right c => case from_forsythe c of
        Left e  => putStrLn $ "Failed: " ++ e
        Right b => putStrLn "Passed"

||| Test setting up a correct initial position from a .fsy file
export test_start_position : IO ()
test_start_position = run (start_position_test "../files/EVEN.fsy")



  
