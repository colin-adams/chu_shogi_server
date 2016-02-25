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

||| Coordinates of squares on the board
module Coordinate

import Data.Fin
import Piece
import Direction

%default total
%access private

||| Board coordinates
public record Coordinate where
  constructor Make_coordinate 
  rank, file: Fin 12

Eq Coordinate where
  (Make_coordinate r f) == (Make_coordinate r' f') = r == r' && f == f'

||| Standard TSA coordinate notation 
Show Coordinate where
  show (Make_coordinate rank file) = (singleton $ chr ((toIntNat (finToNat rank)) + ord 'a')) ++ (show $ (finToNat file) + 1)

||| Absolute difference between rank @source to rank @distant
rank_difference : (source : Coordinate) -> (distant : Coordinate) -> Nat
rank_difference c1 c2 = let r1 = toIntegerNat .finToNat $ rank c1
                            r2 = toIntegerNat . finToNat $ rank c2
                        in fromInteger $ abs (r1 - r2)

||| Absolute difference between file @source to file @distant
file_difference : (source : Coordinate) -> (distant : Coordinate) -> Nat
file_difference c1 c2 = let r1 = toIntegerNat . finToNat $ file c1
                            r2 = toIntegerNat . finToNat $ file c2
                        in fromInteger $ abs (r1 - r2)

||| Absolute distance from @source to @destination
|||
||| Metric is (forget the name - look it up in a topology primer)
abstract distance_between : Coordinate -> Coordinate -> Nat
distance_between c1 c2 = let r = rank_difference c1 c2
                             f = file_difference c1 c2
                         in maximum r f
                        
||| Distance from @source to @destination in a prinicpal direction
|||
||| Only non-zero if a straight line connects the two squares in one of the eight principal directions
abstract distance_to : Coordinate -> Coordinate -> Nat
distance_to c1 c2 = let r = rank_difference c1 c2
                        f = file_difference c1 c2
                    in case r of
                      Z => f
                      _ => case f of
                                Z => r
                                _ => if r == f then 
                                        r
                                     else
                                       0

||| Change in rank and file to move from @source to @target
path_to : (source : Coordinate) -> (target : Coordinate) -> (Integer, Integer)
path_to c1 c2 = let r  = toIntegerNat $ finToNat $ rank c1
                    r' = toIntegerNat $ finToNat $ rank c2
                    f  = toIntegerNat $ finToNat $ file c1
                    f' = toIntegerNat $ finToNat $ file c2
                 in (r' - r, f' - f)

||| Direction and range for piece-colour from @source to @destination
|||
||| If not one of the principal 8 directions we return Nothing
abstract direction_and_range : (source : Coordinate) -> (destination : Coordinate) -> Maybe (Direction, Nat)
direction_and_range c1 c2 = let (y, x) = path_to c1 c2 in
                                  case compare x 0 of
                                    GT => if y == 0 then
                                             Just (West, fromInteger x)
                                          else
                                            if x == y then
                                                Just (North_east, fromInteger x)
                                            else
                                              if x == -y then
                                                Just (South_east, fromInteger x)
                                              else
                                                Nothing
                                    LT => if y == 0 then
                                               Just (West, fromInteger (-x))
                                          else
                                            if x == y then
                                                Just (South_west, fromInteger (-x))
                                            else
                                              if x == (-y) then
                                                  Just (North_west, fromInteger y)         
                                              else
                                                Nothing
                                    EQ => if y == 0 then
                                             Nothing
                                          else
                                            if y > 0 then
                                                Just (North, fromInteger y)
                                            else
                                                Just (South, fromInteger (-y))
                                                                             
||| Direction for a white piece from @source to @destination
|||
||| If not one of the principal 8 directions we return Nothing
abstract direction_to : Piece -> (source : Coordinate) -> (destination : Coordinate) -> Maybe Direction
direction_to p c1 c2 = case direction_and_range c1 c2 of
                         Nothing => Nothing
                         Just (d, _) => Just d
 
||| Next square in @direction from @source
|||
||| @source - Square where we are currently located
||| @direction - Direction in which we are moving
abstract next_square : (source : Coordinate) -> (direction : Direction) -> Maybe Coordinate
next_square c d = case d of
  North => case natToFin (finToNat (rank c) + 1) 12 of
    Nothing => Nothing
    Just r  => Just (Make_coordinate r (file c))
  South => let r = finToNat (rank c) 
           in case r of
             Z => Nothing
             S n => case natToFin n 12 of
               Nothing => Nothing
               Just r' => Just (Make_coordinate r' (file c))
  East  => case natToFin (finToNat (file c) + 1) 12 of
    Nothing => Nothing
    Just f  => Just (Make_coordinate (rank c) f)                  
  West  => let f = finToNat (file c) 
           in case f of
             Z => Nothing
             S n => case natToFin n 12 of             
               Nothing => Nothing
               Just f' => Just (Make_coordinate (rank c) f')             
  North_east => case natToFin (finToNat (rank c) + 1) 12 of
    Nothing => Nothing
    Just r  => case natToFin (finToNat (file c) + 1) 12 of
      Nothing => Nothing
      Just f  => Just (Make_coordinate r f)
  North_west => case natToFin (finToNat (rank c) + 1) 12 of
    Nothing => Nothing
    Just r  => let f = finToNat (file c) 
               in case f of
                 Z => Nothing
                 S n => case natToFin n 12 of             
                   Nothing => Nothing
                   Just f' => Just (Make_coordinate r f')             
  South_east => let r = finToNat (rank c) 
                in case r of
                  Z => Nothing
                  S n => case natToFin n 12 of
                    Nothing => Nothing
                    Just r' => case natToFin (finToNat (file c) + 1) 12 of
                      Nothing => Nothing
                      Just f  => Just (Make_coordinate r' f)
  South_west => let r = finToNat (rank c) 
                in case r of
                  Z => Nothing
                  S n => case natToFin n 12 of
                    Nothing => Nothing
                    Just r' => let f = finToNat (file c) 
                               in case f of
                                 Z => Nothing
                                 S n => case natToFin n 12 of             
                                   Nothing => Nothing
                                   Just f' => Just (Make_coordinate r' f')   
 
