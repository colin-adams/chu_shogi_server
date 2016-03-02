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

||| Representation of a move that was made on the board
module Move

import Piece
import Coordinate

%default total

public export data Move = 
  ||| Double move by @origin to @empty-square then back to origin
  Pass Piece Coordinate Coordinate |
  ||| Capture by piece @origin of piece @target without moving
  Igui Piece Coordinate Piece Coordinate |
  ||| Other double move by piece @origin capturing piece @target1 then on to @target 2 (possibly capturing)
  Double_move Piece Coordinate Piece Coordinate (Maybe Piece) Coordinate |
  ||| Capture by piece @origin of piece @target @promoted? @declined to promote?
  Capture Piece Coordinate Piece Coordinate Bool Bool |
  ||| Move by piece @origin to @target @promoted? @declined to promote?
  Simple_move Piece Coordinate Coordinate Bool Bool
  
||| Is @m a non-lion capturing a lion?
is_lion_capture : (m : Move) -> Bool
is_lion_capture m = case m of
  Pass _ _ _                => False
  Igui _ _ _ _              => False
  Simple_move _ _ _ _ _     => False
  Capture p _ p2 _ _ _      => case is_lion (piece_type p2) of
    True  => not (is_lion (piece_type p))
    False => False
  Double_move p _ p2 _ p3 _ => case is_lion (piece_type p) of
    True  => False
    False => case p3 of
      Nothing  => is_lion (piece_type p2)
      Just p3' => is_lion (piece_type p2) || is_lion (piece_type p3')

||| Modified TSA notation for a move
public export Show Move where
  show (Pass p c1 c2)                 = (abbreviation $ piece_type p) ++ " " ++ (show c1) ++ " !" ++ (show c2)
  show (Igui p c1 _ c2)               = (abbreviation $ piece_type p) ++ " " ++ (show c1) ++ " x!" ++ (show c2)
  show (Capture p c1 _ c2 pr dec)     = let ind = case pr of
                                                   True  => "+"
                                                   False => case dec of
                                                                 True  => "="
                                                                 False => ""
                                        in (abbreviation $ piece_type p) ++ " " ++ (show c1) ++ " x " ++ (show c2) ++ ind
  show (Simple_move p c1 c2 pr dec)   = let ind = case pr of
                                                   True  => "+"
                                                   False => case dec of
                                                                 True  => "="
                                                                 False => ""
                                        in (abbreviation $ piece_type p) ++ " " ++ (show c1) ++ " - " ++ (show c2) ++ ind
  show (Double_move p c1 p2 c2 p3 c3) = let sep = case p3 of
                                                       Nothing => " - "
                                                       Just _  => " x "
                                        in (abbreviation $ piece_type p) ++ " " ++ (show c1) ++ " x " ++ (show c2) ++ 
                                          sep ++ (show c3)
                                            
