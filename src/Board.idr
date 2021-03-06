
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

import Data.Matrix
import Data.Fin
import Data.Vect
import Piece
import Direction
import Coordinate

%default total

public export Square : Type
Square = Maybe (Piece, Promotion_status)

||| A 12 x 12 matrix of Squares
public export Board : Type
Board = Matrix 12 12 Square

||| Piece on @square of @board, if any, along with it's promotion status
public export piece_at : Coordinate -> Board -> Square
piece_at c b = indices (rank c) (file c) b

||| Copy of @board where @piece sits on  @location
|||
||| @piece - piece to be added
||| @location - square to be modified
||| @board - the position we are copying
export with_piece_at : (piece: (Piece, Promotion_status)) -> (location : Coordinate) -> (board : Board) -> Board
with_piece_at (p, st) c b = let r  = getRow (rank c) b
                                r' = updateAt (file c) (\sq => Just (p, st)) r
                            in updateAt (rank c) (\rnk => r') b

||| promotion status of piece (if any) @source on @bd
|||
||| @source - location of piece
||| @bd - position we examine
export promotion_status_at : (source : Coordinate) -> (bd : Board) -> Promotion_status
promotion_status_at source bd = case piece_at source bd of
  Nothing => No_promotion
  Just (_, st) => st
  
||| Copy of @board where @location is empty
|||
||| @location - square to be emptied
||| @board - the position we are copying
export without_piece_at : (location : Coordinate) -> (board : Board) -> Board
without_piece_at c b = let r  = getRow (rank c) b
                           r' = updateAt (file c) (\sq => Nothing) r
                       in updateAt (rank c) (\rnk => r') b

||| Location on a specific board
public export Location : Type
Location = (Coordinate, Board)

||| A piece paired with it's location on a specific board
public export Occupied_location : Type
Occupied_location = (Location, Piece)

||| Returns the piece, coupled with it's location, from the board using a single
||| flattened index. Index is supposed to be rank * 12 + file. This wants testing. 
||| Should only be called on an index of an occupied square (proof needed).
||| A white Pawn at (0, 0) is returned - an impossibility, if the square is actually emtpy. We should have a proof that this cannot occur. I.e. a proof that piece_at reurns Just.
piece_at_flattened : Board -> Fin 144 -> Occupied_location
piece_at_flattened b n = let m = finToNat n 
                             r = divNatNZ m 12 SIsNotZ
                             f = modNatNZ m 12 SIsNotZ
                             c = Make_coordinate (fromMaybe FZ (natToFin r 12)) (fromMaybe FZ (natToFin f 12))
                             s = piece_at c b
                         in case s of
                           Nothing     => ((Make_coordinate (FZ) (FZ), b), Make_piece Pawn White)
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

||| List of argument and adjacent values
|||
||| Result always contains the argument.
same_and_adjacent : Fin 12 -> List (Fin 12)
same_and_adjacent origin = case origin of
  FZ   => [zero, one]
  n    => if (finToNat n) == 1 then [zero, one, two]
       	  else (if (finToNat n) == 2 then [one, two, three]
	  else (if (finToNat n) == 3 then [two, three, four]
  	  else (if (finToNat n) == 4 then [three, four, five]
  	  else (if (finToNat n) == 5 then [four, five, six]
  	  else (if (finToNat n) == 6 then [five, six, seven]
  	  else (if (finToNat n) == 7 then [six, seven, eight]
  	  else (if (finToNat n) == 8 then [seven, eight, nine]
  	  else (if (finToNat n) == 9 then [eight, nine, ten]
  	  else (if (finToNat n) == 10 then [nine, ten, eleven]
  	  else [ten, eleven])))))))))
  

||| Empty squares adjacent to @source @source except for @origin
|||
||| Use to find possible non-capture second moves for a lion (other than igui)
||| @source - square to which a lion has just moved by 1 step
||| @origin - square from which a lion has just moved by 1 step
export empty_squares_adjacent_to_except : (source : Coordinate) -> (origin : Coordinate) -> (bd : Board) ->  List Coordinate
empty_squares_adjacent_to_except c2 c1 b = let squares    = [Make_coordinate r f | r <- same_and_adjacent (rank c2), f <- same_and_adjacent (file c2)]
                                               neither_of = \c => c1 /= c && c2 /= c
  in filter neither_of squares

||| All squares on @board containing a piece of @colour
|||
||| @colour - subset of pieces
||| @board - position we are checking
export pieces_of_colour : (colour : Piece_colour) -> (board : Board) -> List Occupied_location
pieces_of_colour col b = filter (\ (_, p) => (piece_colour p) == col) (pieces b)

||| All squares on @board containing a piece of @colour other than @except
|||
||| @colour - subset of pieces
||| @except - square to exclude
||| @board - position we are checking
pieces_of_colour_except : (colour : Piece_colour) -> (except : Coordinate) -> (board : Board) -> List Occupied_location
pieces_of_colour_except col c b = filter (\ ((c', _), _) => c' /= c) (pieces_of_colour col b)

||| is @rank in @colour's promotion zone?
|||
||| @rank - rank of the board - White moves up the board (North) from a -> l (0 - 11)
||| @colour - if Black, then we invert the numbering
in_promotion_zone : (rank : Fin 12) -> (colour : Piece_colour) -> Bool
in_promotion_zone r col = let r' = toIntNat $ finToNat r
                              r'' = if col == White then r' else 11 - r'
  in r'' > 7

||| Did piece of @colour moving from @start_rank to @end_rank of @status and @capturing have an opportunity to promote?
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
      Not_yet_promoted    => (True, "Not yet promoted")

||| Is piece moving from @source to @destination on @board capable of @promoting and @declining when @capturing?
|||
||| @source - Location of piece at start of move
||| @destination - Location of piece at end of move
||| @board - the board on which the move is occurring
||| @promoting - is the piece claiming to promote?
||| @declining - is the piece declining to promote?
||| @capturing - has the piece claimed to have captured?
||| We also return a validity message
export check_promotion : (source : Coordinate) -> (destination : Coordinate) -> (board : Board) -> (promoting : Bool) ->
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
        (False, False) => (True, snd opp)
 
||| Can @piece jump to @destination?
|||
||| @piece - the piece that wants to jump
||| @source - the assumed location of @piece - precondition - encode this as an erased argument (implicit?)
||| @board - the location of all pieces in the game
||| @destination - the square which the piece wants to jump to
||| We also return a validity message
export can_jump_to : (piece : Piece) -> (source : Coordinate) -> (board : Board) -> (destination : Coordinate) -> (Bool, String)
can_jump_to p c1 b c2 = case direction_and_range c1 c2 of
  Nothing     => (False, "Starting and ending squares are not in in orthogonal or diagonal arrangement")
  Just (d, r) => case r == 2 of
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
can_reach_from c2 c1 b d n col message = 
  case length message of
    Z => (False, "Precondition violation in can_reach_from - zero length message")
    _ => case next_square c1 d of
      Nothing => (False, message ++ ", No additional square on the board in direction " ++ (show d))
      Just c' => case piece_at c' b of
        Nothing => case c' == c2 of
          True => (True, "")
          False => case n of
            Z   => (False, message ++ ", can't reach that square within piece's range")
            S m => can_reach_from c2 c' b d m col message
        Just (p, _) => case piece_colour p == col of
          True  => (False, message ++ ", Can't capture a piece of your own side")
          False =>  case c' == c2 of
            True  => (True, "")
            False => case n of
              Z   => (False, message ++ ", Can't reach that square within piece's range")
              S m => can_reach_from c2 c' b d m col message
                            
||| Can @piece range to @destination?
|||
||| @piece - the piece that wants to move
||| @source - the assumed location of @piece - precondition - encode this as an erased argument (implicit?)
||| @destination - the square which the piece wants to move to
||| @board - the location of all squares in the game
||| @message - validity message to append to
||| We also return a validity message appended to @message
export can_range_to : (piece : Piece) -> (source : Coordinate) -> (board : Board) -> (message : String) -> (destination : Coordinate) -> (Bool, String)
can_range_to p c1 b message c2 = case direction_and_range c1 c2 of
  Nothing     => (False, message ++ ", Destination is not on an orthogonal or diagonal direction")
  Just (d, r) => case r > 0 of
    False => (False, message ++ ", Zero range in direction " ++ show d)
    True  => let col = piece_colour p
                 d'  = if col == Black then opposite_direction d else d
             in case range (piece_type p) d' of
      Z   => (False, message ++ ", Piece has zero range in direction " ++ show d')
      S n => can_reach_from c2 c1 b d n (piece_colour p) message
      
||| Does non-lion @piece have lion moves in @direction?
|||
||| This does not apply to Lions - @direction is for a white piece.  
||| We also return a validity message
export has_lion_moves : Piece_type -> Direction -> (Bool, String)
has_lion_moves p d = case p of
                          Soaring_eagle => case d of
                            North_east => (True, "")
                            North_west => (True, "")
                            _          => (False, "Soaring Eagle does not have lion power in direction " ++ (show d))
                          Horned_falcon => case d of
                            North => (True, "")
                            _     => (False, "Horned Falcon does not have lion power in direction " ++ (show d))

                          _             => (False, "Only Soaring eagles and Horned falcons have directional lion power")

     
||| Does @piece have a one-square lion move from @source to @destination
|||
||| @source is the starting square
||| @destination is assumed to be empty or occupied by an enemy piece.
||| We also return a validity reason
export has_lion_a_to : Piece -> (source : Coordinate) -> (destination : Coordinate) -> (Bool, String)
has_lion_a_to p c1 c2 = if distance_to c1 c2 == 1 then
                           let d = direction_to p c1 c2 
                           in case d of
                             Nothing => (False, "The square at a distance of one does not lie on an orthogonal or diagonal line (impossible!)") --  prove this
                             Just dir => has_lion_moves (piece_type p) dir
                        else
                            (False, "Lion A moves must be to a square at a distance of 1 from the origin")


||| Does @piece have a direct two-square jump to lion move from @source to @destination
|||
||| @source is the starting square
||| @destination is assumed to be empty or occupied by an enemy piece.
||| We also return a validity reason
export has_lion_b_to : Piece -> (source : Coordinate) -> (destination : Coordinate) -> (Bool, String)
has_lion_b_to p c1 c2 = if distance_to c1 c2 == 2 then
                           let d = direction_to p c1 c2 
                           in case d of
                             Nothing => (False, "The square at a distance of two does not lie on an orthogonal or diagonal line (impossible!)") -- proof
                             Just dir => has_lion_moves (piece_type p) dir
                        else
                          (False, "Direct jump lion B moves must be to a square at a distance of 2 from the origin")

||| Does @piece (assumed by precondition to be a lion) have a a valid one-stage move from @source to @destination?
||| Special lion-capture rules are not considered.
|||
||| @piece - the Lion that is moving
||| @source - the location of @piece (assumed, not checked - a precondition)
||| @destination - target of the move
||| @board - the position we consider
||| @capturing - are we testing for a capture?
export is_lion_move : (piece : Piece) -> (source : Coordinate) -> (destination : Coordinate) -> (board: Board) -> (capturing : Bool) -> (Bool, String)
is_lion_move p c1 c2 b capt =
  let p2 = piece_at c2 b
      ok = consistent p2 capt
      d  = distance_between c1 c2 
  in case d == 1 || d == 2 of
    True  => (ok, "Capture claim and square occupancy inconsistent")
    False => (False, "Start and end squares are not in a lion-move relation")
 where
   consistent : Maybe (Piece, Promotion_status) -> Bool -> Bool
   consistent p2' capt = case p2' of
     Nothing        => not capt
     Just (p2'', _) => capt && (piece_colour p /= piece_colour p2'')


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
          False => case direction_and_range c' c of
            Nothing     => False
            Just (d, _) => let d' = if piece_colour p == Black then opposite_direction d else d
                           in fst $ has_lion_moves (piece_type p) d'
        
||| Is a piece of @colour in @location on @board protected?
|||
||| @location - square of the piece which we are checking
||| @colour - colour of the piece which we are checking
||| @board - the position we are checking
export is_protected : (location : Coordinate) -> (colour : Piece_colour) -> (board : Board) -> Bool
is_protected c col b = let b2 = without_piece_at c b
                           pieces = pieces_of_colour_except col c b2
  in case find (protects c b2) pieces of
    Nothing => False
    Just _  => True

||| Forsythe notation for an  (occupied) square
|||
||| We could usefully add a proof that the square is occupied (by transforming the type)
forsythe_cell : Square -> String
forsythe_cell c = case c of
  Nothing => ""
  Just (p, st) => let abbrev  = abbreviation (piece_type p)
                      abbrev' = if st == Declined_to_promote then "=" ++ abbrev else abbrev
                      colour  = piece_colour p
                  in case colour of
                    White => toUpper abbrev'
                    Black => toLower abbrev'
                     
||| Forsythe notation for a consecutive series of blanks, followed by a comma if there are occupied cells following                         
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
                   in (leading_blanks blanks rest) ++ (suffix (rest)) where
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

||| George Hodge's modified Forsythe notation for Chu Shogi, suplemented by a prefix of = to indicate deferred promotion, of @board
|||
||| @board - position being described
export forsythe : (board : Board) -> String
forsythe b = concatMap forsythe_rank b

||| Number of kings/crown princes on board of given colour
king_count : Piece_colour -> Board -> Fin 3
king_count col b = let ocl  = pieces_of_colour col b
                       wps  = map snd ocl
                       kgs  = findIndices is_king wps
                       cnt  = length kgs
                   in case natToFin cnt 3 of
                       Nothing => FZ -- this would be a bug
                       Just c  => c

||| Number of white kings/crown princes on board
export white_king_count : Board -> Fin 3
white_king_count b = king_count White b
                       
||| Number of black kings/crown princes on board
export black_king_count : Board -> Fin 3
black_king_count b = king_count Black b

||| Number of non-kings/crown princes on board
export non_king_count : Board -> Fin 95
non_king_count b = let ocl = pieces b
                       pcs = map snd ocl
                       nks = findIndices (not . is_king) pcs
                       cnt = length nks
                   in case natToFin cnt 95 of
                       Nothing => FZ -- this would be a bug
                       Just c  => c                       

display_square : Square -> String
display_square sq = case sq of
  Nothing => "   |"
  Just _  => (substr 0 3 ((forsythe_cell sq) ++ "  ")) ++ "|"

display_rank : Nat -> Board -> String
display_rank rnk bd = let r = natToFin rnk 12
                      in case r of
                        Nothing => ""
                        Just r' => let rank = index r' bd
                                       rk   = chr ((toIntNat rnk) + (ord 'a'))
                                   in (singleton rk) ++ " |" ++ (concatMap display_square rank) ++ "\n"

||| Ascii graphics display of a Board
|||
||| We display from Black's point of view, simply because it is easier to do. This is just a debugging aid
export display_board : Board -> String
display_board bd = "    12  11  10  9   8   7   6   5   4   3   2   1\n" ++
                   "  +---+---+---+---+---+---+---+---+---+---+---+---+\n" ++
                   (display_rank 0 bd) ++ 
                   "  +---+---+---+---+---+---+---+---+---+---+---+---+\n" ++
                   (display_rank 1 bd) ++                  
                   "  +---+---+---+---+---+---+---+---+---+---+---+---+\n" ++
                   (display_rank 2 bd) ++
                   "  +---+---+---+---+---+---+---+---+---+---+---+---+\n" ++
                   (display_rank 3 bd) ++
                   "  +---+---+---+---+---+---+---+---+---+---+---+---+\n" ++                   
                   (display_rank 4 bd) ++
                   "  +---+---+---+---+---+---+---+---+---+---+---+---+\n" ++                   
                   (display_rank 5 bd) ++
                   "  +---+---+---+---+---+---+---+---+---+---+---+---+\n" ++                   
                   (display_rank 6 bd) ++
                   "  +---+---+---+---+---+---+---+---+---+---+---+---+\n" ++                   
                   (display_rank 7 bd) ++
                   "  +---+---+---+---+---+---+---+---+---+---+---+---+\n" ++                   
                   (display_rank 8 bd) ++
                   "  +---+---+---+---+---+---+---+---+---+---+---+---+\n" ++                   
                   (display_rank 9 bd) ++
                   "  +---+---+---+---+---+---+---+---+---+---+---+---+\n" ++                   
                   (display_rank 10 bd) ++
                   "  +---+---+---+---+---+---+---+---+---+---+---+---+\n" ++                   
                   (display_rank 11 bd) ++                                                                                                                                                                                              
                   "  +---+---+---+---+---+---+---+---+---+---+---+---+\n"                   
