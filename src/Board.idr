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

||| Conceptual representation of (as opposed to a graphical display of) the board at a given instance in time
module Board

import Data.Vect
import Piece
import Coordinate

%default total
%access private

Square : Type
Square = Maybe (Piece, Promotion_status)

||| A 12 x 12 matrix of Squares
abstract Board : Type
Board = Vect 12 (Vect 12 Square)


append : String -> String -> String
append s s' = s ++ s'

||| Piece on @square of @board, if any, along with it's promotion status
public piece_at : Coordinate -> Board -> Square
piece_at c b = let r = index (rank c) b in
               index (file c) r

||| Copy of @board where @location is empty
|||
||| @location - square to be emptied
||| @board - the position we are copying
without_piece_at : (location : Coordinate) -> (board : Board) -> Board
without_piece_at c b = let r  = index (rank c) b
                           r' = updateAt (file c) (\sq => Nothing) r
                       in updateAt (rank c) (\rnk => r') b

||| Location on a specific board
public Location : Type
Location = (Coordinate, Board)

is_occupied : Location -> Bool
is_occupied (c, b) = case piece_at c b of
  Nothing => False
  Just _  => True

||| A piece paired with it's location on a specific board
public Occupied_location : Type
Occupied_location = (Location, Piece)
  
piece_at_flattened : Board -> Fin 144 -> Occupied_location
piece_at_flattened b n = let m = finToNat n 
                             r = divNatNZ m 12 SIsNotZ
                             f = modNatNZ m 12 SIsNotZ
                             c = Make_coordinate (fromMaybe FZ (natToFin r 12)) (fromMaybe FZ (natToFin f 12))
                             s = piece_at c b
                         in case s of
                           Nothing     => ((Make_coordinate (FZ) (FZ), b), Make_piece Pawn Black)
                           Just (p, _) => ((c, b), p) 
                           
||| All squares on @board containing a piece
|||
||| @board - position we are checking
pieces : (board : Board) -> List Occupied_location
pieces b = let sqs  = concat b
               inds = findIndices isJust sqs
           in map (piece_at_flattened b) inds

zero : Fin 12
zero = FZ

one : Fin 12
one = FS (FZ)

two : Fin 12
two = FS (FS (FZ))

three : Fin 12
three = FS (FS (FS (FZ)))

four : Fin 12
four = FS (FS (FS (FS (FZ))))

five : Fin 12
five = FS (FS (FS (FS (FS (FZ)))))

six : Fin 12
six = FS (FS (FS (FS (FS (FS (FZ))))))

seven : Fin 12
seven = FS (FS (FS (FS (FS (FS (FS (FZ)))))))

eight : Fin 12
eight = FS (FS (FS (FS (FS (FS (FS (FS (FZ))))))))

nine : Fin 12
nine = FS (FS (FS (FS (FS (FS (FS (FS (FS (FZ)))))))))

ten : Fin 12
ten = FS (FS (FS (FS (FS (FS (FS (FS (FS (FS (FZ))))))))))

eleven : Fin 12
eleven = FS (FS (FS (FS (FS (FS (FS (FS (FS (FS (FS (FZ)))))))))))

same_and_adjacent : Fin 12 -> List (Fin 12)
same_and_adjacent o = case o of
  FZ   => [zero, one]
  FS (FZ)   => [zero, one, two]
  FS (FS (FZ)) => [one, two, three]
  FS (FS (FS (FZ)))  => [two, three, four]
  FS (FS (FS (FS (FZ)))) => [three, four, five]
  FS (FS (FS (FS (FS (FZ))))) => [four, five, six]
  FS (FS (FS (FS (FS (FS (FZ)))))) => [five, six, seven]
  FS (FS (FS (FS (FS (FS (FS (FZ))))))) => [six, seven, eight]
  FS (FS (FS (FS (FS (FS (FS (FS (FZ)))))))) => [seven, eight, nine]
  FS (FS (FS (FS (FS (FS (FS (FS (FS (FZ))))))))) => [eight, nine, ten]
  FS (FS (FS (FS (FS (FS (FS (FS (FS (FS (FZ)))))))))) => [nine, ten, eleven]
  FS (FS (FS (FS (FS (FS (FS (FS (FS (FS (FS (FZ))))))))))) => [ten, eleven]
  

||| Empty squares 
abstract empty_squares_adjacent_to_except : (source : Coordinate) -> (origin : Coordinate) -> (bd : Board) ->  List Coordinate
empty_squares_adjacent_to_except c2 c1 b = let squares    = [Make_coordinate r f | r <- same_and_adjacent (rank c2), f <- same_and_adjacent (file c2)]
                                               neither_of = \c => c1 /= c && c2 /= c
  in filter neither_of squares

||| All squares on @board containing a piece of @colour
|||
||| @colour - subset of pieces
||| @board - position we are checking
abstract pieces_of_colour : (colour : Piece_colour) -> (board : Board) -> List Occupied_location
pieces_of_colour col b = filter (\ (_, p) => (piece_colour p) == col) (pieces b)

||| All squares on @board containing a piece of @colour other than @excpet
|||
||| @colour - subset of pieces
||| @except - square to exclude
||| @board - position we are checking
pieces_of_colour_except : (colour : Piece_colour) -> (except : Coordinate) -> (board : Board) -> List Occupied_location
pieces_of_colour_except col c b = filter (\ ((c', _), _) => c' /= c) (pieces_of_colour col b)

||| is @rank in @colour's promotion zone?
|||
||| @rank - rank of the board - White moves up the board (North) from a -> l
||| @colour - if Black, then we invert the numbering
in_promotion_zone : (rank : Fin 12) -> (colour : Piece_colour) -> Bool
in_promotion_zone r col = let r' = toIntNat $ finToNat r
                              r'' = if col == White then r' else 11 - r'
  in r'' > 7

||| Did piece of @colour moving from @start_rank to @end_rank of @status and @captruing have an opportunity to promote?
|||
||| @start_rank - the rank from which the piece commenced moving
||| @end_rank - the rank on which the piece finished its move
||| @colour - colour of the piece
||| @status - promotion status of the piece at start of move
||| @capturing - was the piece making a capture?
||| We also return a validity message
promotion_opportunity : (start_rank : Fin 12) -> (end_rank : Fin 12) -> (colour : Piece_colour) -> 
  (status : Promotion_status) -> (capturing : Bool) -> (Bool, String)
promotion_opportunity r1 r2 col st capt = let st_in  = in_promotion_zone r1 col
                                              end_in =  in_promotion_zone r2 col
  in case (st_in, end_in) of
    (False, False) => (False, "Can't promote when both starting square and finishing square are outside the promotion area")
    _              => case st of
      No_promotion        => (False, "This piece type does not have a promotion")
      Declined_to_promote => (capt, "Can't promote after declining except by capturing, or re-entering the promotion zone")
      Not_yet_promoted    => (True, "")

||| Is piece moving from @source to @destination on @board capable of @promoting and @declining when @capturing?
|||
||| @source - Location of piece at start of move
||| @destination - Location of piece at end of move
||| @board - the board on which the move is occurring
||| @promoting - is the piece claiming to promote?
||| @declining - is the piece declining to promote?
||| @capturing - has the piece claimed to have captured?
||| We also return a validity message
abstract check_promotion : (source : Coordinate) -> (destination : Coordinate) -> (board : Board) -> (promoting : Bool) ->
  (declining : Bool) -> (capturing : Bool) -> (Bool, String)
check_promotion c1 c2 b pr dec capt = case piece_at c1 b of
    Nothing => (False, "Starting square lacks a piece")
    Just (p, st)  => let col = piece_colour p
                         r   = rank c1
                         r'  = rank c2
                         opp = promotion_opportunity r r' col st capt
      in case (pr, dec) of
        (True, True)   => (False, "Can't simulataneously both promote and decline to promote")
        (False, True)  => opp
        (True, False)  => opp
        (False, False) => (not (fst opp), snd opp)
 
||| Can @piece jump to @destination?
|||
||| @piece - the piece that wants to jump
||| @source - the assumed location of @piece - precondition
||| @destination - the square which the piece wants to jump to
||| @board - the location of all pieces in the game
||| We also return a validity message
abstract can_jump_to : (piece : Piece) -> (source : Coordinate) -> (board : Board) -> (destination : Coordinate) -> (Bool, String)
can_jump_to p c1 b c2 = case direction_and_range (piece_colour p) c1 c2 of
  Nothing     => (False, "Starting and ending squares are not in in orthogonal or diagonal arrangement")
  Just (d, r) => case r == S (S Z) of
    False => (False, "Jumping is only allowed at a range of two")
    True  => jump (piece_type p) d

||| Can we reach @target from @source on @board by moving in @direction for at most @moves + 1
|||
||| @target - square we are trying to reach
||| @source - square we are currently on
||| @board - the board on which we are moving
||| @direction - direction in which we are moving
||| @moves - additional number of squares we will move after trying the first step
||| @colour - colour of the moving piece
||| @message - validity message to append to
||| We also return a validity message
can_reach_from : (target : Coordinate) -> (source : Coordinate) -> (board : Board) -> (direction : Direction) -> (moves : Nat) ->
 (colour : Piece_colour) -> (message :String) -> (Bool, String)
can_reach_from c2 c1 b d n col message = case next_square c1 d of
  Nothing => (False, message ++", No additional square on the board in that direction")
  Just c' => case piece_at c' b of
    Nothing => case c' == c2 of
      True => (True, "")
      False => case n of
        Z   => (False, message ++ ", can't move a zero distance")
        S m => can_reach_from c2 c' b d m col message
    Just (p, _) => case piece_colour p == col of
      True  => (False, message ++ ", Can't capture a piece of your own side")
      False => case n of
        Z   => (False, message ++ ", Can't reach that square within piece's range")
        S m => can_reach_from c2 c' b d m col message
                            
||| Can @piece range to @destination?
|||
||| @piece - the piece that wants to move
||| @source - the assumed location of @piece - precondition
||| @destination - the square which the piece wants to move to
||| @board - the location of all pieces in the game
||| @message - validity message to append to
||| We also return a validity message
abstract can_range_to : (piece : Piece) -> (source : Coordinate) -> (board : Board) -> (message : String) -> (destination : Coordinate) -> (Bool, String)
can_range_to p c1 b message c2 = case direction_and_range (piece_colour p) c1 c2 of
  Nothing     => (False, message ++ ", Destination is not on an orthogonal or diagonal direction")
  Just (d, r) => case r > Z of
    False => (False, message ++ ", Zero range")
    True  => case range (piece_type p) d of
      Z   => (False, message ++ ", Piece has zero range in that direction")
      S n => can_reach_from c2 c1 b d n (piece_colour p) message
      
||| Does non-lion @piece have lion moves in @direction?
|||
||| This does not apply to Lions - @direction is for a white piece.  
||| We also return a validity message
abstract has_lion_moves : Piece_type -> Direction -> (Bool, String)
has_lion_moves p d = case p of
                          Soaring_eagle => case d of
                            North_east => (True, "")
                            North_west => (True, "")
                            _          => (False, "Soaring Eagle does not have lion power in this direction")
                          Horned_falcon => case d of
                            North => (True, "")
                            _     => (False, "Horned Falcon does not have lion power in this direction")

                          _             => (False, "Only Soaring eagles and Horned falcons have directional lion power")


||| Does @piece have a one-square lion move from @source to @destination
|||
||| @source is the starting square
||| @destination is assumed to be empty or occupied by an enemy piece.
||| We also return a validity reason
abstract has_lion_a_to : Piece -> (source : Coordinate) -> (destination : Coordinate) -> (Bool, String)
has_lion_a_to p c1 c2 = case distance_to c1 c2 of
                               S Z => let d = direction_to p c1 c2 in
                                      case d of
                                        Nothing => (False, "The square at a distance of one does not lie on an orthogonal or diagonal line (impossible!)")
                                        Just dir => has_lion_moves (piece_type p) dir
                               _ => (False, "Lion A moves must be to a square at a distance of 1 from the origin")


||| Does @piece have a direct one-square jump to lion move from @source to @destination
|||
||| @source is the starting square
||| @destination is assumed to be empty or occupied by an enemy piece.
||| We also return a validity reason
abstract has_lion_b_to : Piece -> (source : Coordinate) -> (destination : Coordinate) -> (Bool, String)
has_lion_b_to p c1 c2 = case distance_to c1 c2 of
                               S (S Z) => let d = direction_to p c1 c2 in
                                      case d of
                                        Nothing => (False, "The square at a distance of one does not lie on an orthogonal or diagonal line (impossible!)")
                                        Just dir => has_lion_moves (piece_type p) dir
                               _       => (False, "Direct jump lion B moves must be to a square at a distance of 2 from the origin")


||| Can piece @occ move to @target on @board?
|||
||| @target - empty square which we try to move to
||| @board - the position we are examining
||| @occ - the piece and it's location
protects : (target : Coordinate) -> (board : Board) -> (occ : Occupied_location) -> Bool
protects c b o = case o of
  ((c', b'), p) => case can_jump_to p c' b c of
    (True, _)  => True
    (False, _) => case can_range_to p c' b "" c of
      (True, _)  => True
      (False, _) => case (distance_between c' c) <= 2 of
        False => False
        True  => case is_lion (piece_type p) of
          True  => True
          False => case direction_and_range (piece_colour p) c' c of
            Nothing     => False
            Just (d, _) => fst $ has_lion_moves (piece_type p) d
        
||| Is a piece of @colour in @location on @board protected?
|||
||| @location - square of the piece which we are checking
||| @colour - colour of the piece which we are checking
||| @board - the position we are checking
abstract is_protected : (location : Coordinate) -> (colour : Piece_colour) -> (board : Board) -> Bool
is_protected c col b = let b2 = without_piece_at c b
                           pieces = pieces_of_colour_except col c b2
  in case find (protects c b2) pieces of
    Nothing => False
    Just _  => True

forsythe_rest : Nat -> String -> String
forsythe_rest n rest = (show n) ++ case length rest of
                          Z => show n
                          _ => (show n) ++ ","
                       
forsythe_cell : Square -> String
forsythe_cell c = case c of
  Nothing => ""
  Just (p, st) => let abbrev = abbreviation (piece_type p)
                      colour = piece_colour p
                  in case colour of
                    White => toUpper abbrev
                    Black => toLower abbrev

                       
leading_blanks : List Square -> List Square -> String
leading_blanks b r = case length b of
  Z => ""
  n => (show n) ++ (trailer r) 
 where
    trailer : List Square -> String
    trailer rest = case rest of
      [] => ""
      _ => ","
    
||| Forsythe representation of @rank minus leading and trailing /s
|||
||| @n length of @rank
||| @rank - row of the board being described
forsythe_cells : (n : Nat) -> (rank : Vect n Square) -> String
forsythe_cells n r = let (blanks, rest) = break isNothing (toList r)
                   in append (leading_blanks blanks rest) (suffix (rest)) where
                     suffix : List Square -> String
                     suffix s = case head' s of
                       Nothing   => ""
                       Just c => case tail' s of
                         Nothing => forsythe_cell c
                         Just t  => (forsythe_cell c) ++ "," ++ (forsythe_cells (assert_smaller n (length t)) (fromList $ t))
  

                         
||| Forsythe representation of @rank
|||
||| @rank - row of the board being described
forsythe_rank : (rank : Vect 12 Square) -> String
forsythe_rank r = "/" ++ (forsythe_cells 12 r) ++ "/"

||| George Hodge's modified Forsythe notation for @board
abstract forsythe : Board -> String
forsythe b = concatMap forsythe_rank b

