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

||| Chu-shogi pieces
module Piece

import Data.SortedMap
import Direction

%default total

||| Can this piece promote without capturing?
public export data Promotion_status = Not_yet_promoted
  | No_promotion
  | Declined_to_promote

public export Eq Promotion_status where
  Not_yet_promoted == Not_yet_promoted = True
  No_promotion == No_promotion = True
  Declined_to_promote == Declined_to_promote = True
  _ == _ = False

||| Designation of a player and owner of pieces
public export data Piece_colour =
  ||| First player in even games
  Black |
  ||| Second player in even games (and technically in handicap games too - Black's first move is to take the handicap)
  White

public export Eq Piece_colour where
  Black == Black = True
  White == White = True
  _ == _ = False
  
||| opposite colour to that of @colour
export opposite_colour : Piece_colour -> Piece_colour
opposite_colour c = case c of
                         Black => White
                         White => Black

||| Classification of piece (i.e. movement and capturing capabilites, and special status for kings)
public export data Piece_type = Lance
    | Reverse_chariot 
    | Side_mover
    | Vertical_mover
    | White_horse
    | Rook 
    | Gold_general
    | Copper_general 
    | Silver_general
    | Bishop 
    | King
    | Drunk_elephant
    | Crown_prince 
    | Promoted_rook 
    | Promoted_gold_general 
    | Promoted_silver_general
    | Promoted_copper_general
    | Promoted_bishop
    | Ferocious_leopard 
    | Phoenix
    | Kylin 
    | Lion 
    | Free_king
    | Promoted_phoenix
    | Promoted_kylin
    | Blind_tiger
    | Flying_stag
    | Flying_ox
    | Whale 
    | Promoted_ferocious_leopard
    | Free_boar 
    | Dragon_horse 
    | Dragon_king
    | Horned_falcon
    | Soaring_eagle
    | Pawn
    | Go_between
    | Promoted_pawn
    | Promoted_go_between;
    
public export Eq Piece_type where
   (==) p q = case p of
     Lance => case q of
       Lance => True
       _ => False
     Reverse_chariot => case q of
       Reverse_chariot => True
       _ => False
     Side_mover => case q of
       Side_mover => True
       _ => False
     Vertical_mover => case q of
       Vertical_mover => True
       _ => False
     White_horse => case q of
       White_horse => True
       _ => False
     Rook => case q of
       Rook => True
       _ => False
     Gold_general => case q of
      Gold_general  => True
      _ => False
     Copper_general => case q of
       Copper_general => True
       _ => False
     Silver_general=> case q of
       Silver_general => True
       _ => False
     Bishop => case q of
       Bishop => True
       _ => False
     King => case q of
       King => True
       _ => False
     Drunk_elephant => case q of
       Drunk_elephant => True
       _ => False
     Crown_prince => case q of
       Crown_prince => True
       _ => False
     Promoted_rook => case q of
       Promoted_rook => True
       _ => False
     Promoted_gold_general => case q of
       Promoted_gold_general => True
       _ => False
     Promoted_silver_general => case q of
       Promoted_silver_general => True
       _ => False
     Promoted_copper_general => case q of
       Promoted_copper_general => True
       _ => False
     Promoted_bishop => case q of
       Promoted_bishop => True
       _ => False
     Ferocious_leopard => case q of
       Ferocious_leopard => True
       _ => False
     Phoenix => case q of
       Phoenix => True
       _ => False
     Kylin => case q of
       Kylin => True
       _ => False
     Lion => case q of
       Lion => True
       _ => False
     Free_king  => case q of
       Free_king  => True
       _ => False
     Promoted_phoenix => case q of
       Promoted_phoenix => True
       _ => False
     Promoted_kylin => case q of
       Promoted_kylin => True
       _ => False
     Blind_tiger => case q of
       Blind_tiger => True
       _ => False
     Flying_stag => case q of
       Flying_stag => True
       _ => False
     Flying_ox => case q of
       Flying_ox => True
       _ => False
     Whale => case q of
       Whale => True
       _ => False
     Promoted_ferocious_leopard => case q of
       Promoted_ferocious_leopard => True
       _ => False
     Free_boar => case q of
       Free_boar => True
       _ => False
     Dragon_horse => case q of
       Dragon_horse => True
       _ => False
     Dragon_king => case q of
       Dragon_king => True
       _ => False
     Horned_falcon => case q of
       Horned_falcon => True
       _ => False
     Soaring_eagle => case q of
       Soaring_eagle => True
       _ => False
     Pawn => case q of
       Pawn => True
       _ => False
     Go_between => case q of
       Go_between => True
       _ => False
     Promoted_pawn => case q of
       Promoted_pawn => True
       _ => False
     Promoted_go_between => case q of
       Promoted_go_between => True
       _ => False

||| Standard TSA abbreviation
export abbreviation : Piece_type -> String
abbreviation pc = case pc of
  Lance => "L"
  Reverse_chariot => "RC"
  Side_mover => "SM"
  Vertical_mover => "VM"
  White_horse => "+L"
  Rook => "R"
  Gold_general => "G"
  Copper_general => "C"
  Silver_general => "S"
  Bishop => "B"
  King => "K"
  Drunk_elephant => "DE"
  Crown_prince => "+DE"
  Promoted_rook => "+R"
  Promoted_gold_general => "+G"
  Promoted_silver_general => "+S"
  Promoted_copper_general => "+C"
  Promoted_bishop => "+B"
  Ferocious_leopard => "FL"
  Phoenix => "Ph"
  Kylin => "Ky"
  Lion => "Ln"
  Free_king => "FK"
  Promoted_phoenix => "+Ph"
  Promoted_kylin => "+Ky"
  Blind_tiger => "BT"
  Flying_stag => "+BT"
  Flying_ox => "+VM"
  Whale => "+RC"
  Promoted_ferocious_leopard => "+FL"
  Free_boar => "+SM"
  Dragon_horse => "DH"
  Dragon_king => "DK"
  Horned_falcon => "+DH"
  Soaring_eagle => "+DK"
  Pawn => "P"
  Go_between => "GB"
  Promoted_pawn => "+P"
  Promoted_go_between => "+GB"

||| Map of abbreviation to Piece_type
abbreviation_map : SortedMap String Piece_type
abbreviation_map =
    fromList [("Ln", Lion),
              ("L", Lance),
              ("RC", Reverse_chariot),
              ("SM", Side_mover),
              ("VM", Vertical_mover),
              ("+L", White_horse),
              ("R", Rook),
              ("G", Gold_general),
              ("C", Copper_general),
              ("S", Silver_general),
              ("B", Bishop),
              ("K", King),
              ("DE", Drunk_elephant),
              ("+DE", Crown_prince),
              ("+R", Promoted_rook),
              ("+G", Promoted_gold_general),
              ("+S", Promoted_silver_general),
              ("+C", Promoted_copper_general),
              ("+B", Promoted_bishop),
              ("FL", Ferocious_leopard),
              ("Ph", Phoenix),
              ("Ky", Kylin),
              ("FK", Free_king),
              ("+Ph", Promoted_phoenix),
              ("+Ky", Promoted_kylin),
              ("BT", Blind_tiger),
              ("+BT", Flying_stag),
              ("+VM", Flying_ox),
              ("+RC", Whale),
              ("+FL", Promoted_ferocious_leopard),
              ("+SM", Free_boar),
              ("DH", Dragon_horse ),
              ("DK", Dragon_king),
              ("+DH", Horned_falcon),
              ("+DK", Soaring_eagle),
              ("P", Pawn),
              ("GB", Go_between),
              ("+P", Promoted_pawn),
              ("+GB", Promoted_go_between)
             ]
||| Map of uppercase abbreviation to abbreviation
canonical_abbreviation_map : SortedMap String String
canonical_abbreviation_map =
    fromList [("LN", "Ln"),
              ("L", "L"),
              ("RC", "RC"),
              ("SM", "SM"),
              ("VM", "VM"),
              ("+L", "+L"),
              ("R", "R"),
              ("G", "G"),
              ("C", "C"),
              ("S", "S"),
              ("B", "B"),
              ("K", "K"),
              ("DE", "DE"),
              ("+DE", "+DE"),
              ("+R", "+R"),
              ("+G", "+G"),
              ("+S", "+S"),
              ("+C", "+C"),
              ("+B", "+B"),
              ("FL", "FL"),
              ("PH", "Ph"),
              ("KY", "Ky"),
              ("FK", "FK"),
              ("+PH", "+Ph"),
              ("+KY", "+Ky"),
              ("BT", "BT"),
              ("+BT", "+BT"),
              ("+VM", "+VM"),
              ("+RC", "+RC"),
              ("+FL", "+FL"),
              ("+SM", "+SM"),
              ("DH", "DH"),
              ("DK", "DK"),
              ("+DH", "+DH"),
              ("+DK", "+DK"),
              ("P", "P"),
              ("GB", "GB"),
              ("+P", "+P"),
              ("+GB", "+GB")
             ]

canononical_abbreviation : String -> String
canononical_abbreviation abbrev = case lookup abbrev canonical_abbreviation_map of
  Nothing => ""
  Just ab => ab

||| Is @abbrev the case-insensitive abbreviation of a piece-type?
|||
||| @abbrev - the abbreviation to test
export is_abbrev : (abbrev : String) -> Bool
is_abbrev abbrev = let abbrevs = map (toLower . fst) (toList abbreviation_map)
                   in elem abbrev abbrevs
                   
||| Creates a Piece_type given it's standard TSA abbreviation
export piece_from_abbreviation : String -> Maybe Piece_type 
piece_from_abbreviation abbrev = let abbrev'  = toUpper abbrev
                                     abbrev'' = canononical_abbreviation abbrev'
                                 in  lookup abbrev'' abbreviation_map


||| Is @ piece subject to the special rules for Lions?
export is_lion : Piece_type -> Bool
is_lion piece =
    case piece of
      Lion => True
      Promoted_kylin => True
      _ => False
      

||| Is @ dir vaguely North-heading?
is_northerly : Direction -> Bool
is_northerly dir =
    case dir of
      North => True
      North_east => True
      North_west => True
      _ => False

||| Distance @ piece (if White) can move in a northerly direction on an open board
northern_range : Piece_type -> Nat
northern_range piece = if elem piece  [Lance, Reverse_chariot, Vertical_mover, White_horse,
                               Rook, Promoted_rook, Promoted_gold_general, Promoted_silver_general,
                               Free_king, Promoted_phoenix, Flying_stag, Flying_ox,
                               Whale, Dragon_king, Soaring_eagle] then
                       11
                    else if elem piece [Bishop, Kylin, Lion, Promoted_kylin, Blind_tiger,
                                 Promoted_ferocious_leopard, Free_boar, Horned_falcon] then
                         0
                      else 1
    
||| Distance @ piece (if White) can move in a southerly direction on an open board
southern_range : Piece_type -> Nat
southern_range piece = if elem piece [Reverse_chariot, Vertical_mover, White_horse, Rook,
                               Promoted_rook, Promoted_gold_general, Promoted_silver_general,
                               Free_king, Promoted_phoenix, Flying_stag, Flying_ox, Whale,
                               Dragon_king, Horned_falcon, Soaring_eagle] then
                       11
                    else if elem piece [Lance, Silver_general, Bishop, Drunk_elephant, Kylin,
                                       Lion, Promoted_kylin, Promoted_ferocious_leopard,
                                       Pawn, Promoted_go_between] then
                         0
                      else 1
                      
||| Distance @ piece  can move in a westerly or easterly direction on an open board                      
lateral_range : Piece_type -> Nat
lateral_range piece = if elem piece [Side_mover, Rook, Promoted_rook, Promoted_gold_general,
                              Promoted_copper_general, Free_king, Promoted_phoenix,
                              Free_boar, Dragon_king, Horned_falcon, Soaring_eagle] then
                       11
                      else if elem piece [Gold_general, King, Drunk_elephant, Crown_prince, Vertical_mover,
                                   Promoted_silver_general, Promoted_bishop, Phoenix,
                                   Blind_tiger, Flying_stag, Dragon_horse, Promoted_pawn,
                                   Promoted_go_between] then
                        1
                      else 0

||| Distance @ piece  can move in a north-westerly or north-easterly direction on an open board                      
northern_diagonal_range : Piece_type -> Nat
northern_diagonal_range piece = if elem piece [White_horse, Bishop, Promoted_bishop, Free_king, Promoted_phoenix, Flying_ox, 
                                        Promoted_ferocious_leopard, Free_boar, Dragon_horse, Horned_falcon] then
                                 11
                                else if elem piece [Gold_general, Copper_general, Silver_general, King, Drunk_elephant,
                                             Crown_prince, Promoted_rook, Ferocious_leopard, Kylin, Blind_tiger, Flying_stag,
                                             Dragon_king, Promoted_pawn, Promoted_go_between] then
                                  1
                                else 0

||| Distance @ piece  can move in a soth-westerly or south-easterly direction on an open board                      
southern_diagonal_range : Piece_type -> Nat
southern_diagonal_range piece = if elem piece [Bishop, Promoted_bishop, Free_king, Promoted_phoenix, Flying_ox, Whale,
                                        Promoted_ferocious_leopard, Free_boar, Dragon_horse, Horned_falcon, Soaring_eagle] then
                                  11
                                else if elem piece [Silver_general, King, Drunk_elephant, Crown_prince, Promoted_rook, Ferocious_leopard,
                                             Kylin, Blind_tiger, Flying_stag, Dragon_king, Promoted_go_between] then
                                 1
                                else 0
    
||| Can @ piece jump to the second square in a cardinal direction?
cardinal_jump : Piece_type -> Bool
cardinal_jump piece =
    case piece of
      Kylin => True
      _ => False

||| Can @ piece jump to the second square in a diagonal direction?
diagonal_jump : Piece_type -> Bool
diagonal_jump piece =
    case piece of
      Phoenix => True
      _ => False
      
||| Number of squares @ piece is capable of moving on an empty board in @ direction
export range : Piece_type -> Direction -> Nat
range piece direction =
  case direction of
       South => southern_range piece
       North => northern_range piece
       East => lateral_range piece
       West => lateral_range piece
       North_east => northern_diagonal_range piece
       North_west => northern_diagonal_range piece
       South_east => southern_diagonal_range piece
       South_west => southern_diagonal_range piece
       

||| Does @ piece have a two-space jumping capability towards @ direction?
||| We also return a validity message
export jump : Piece_type -> Direction -> (Bool, String)
jump piece direction = 
    case direction of
       South => (cardinal_jump piece, "Can't jump backwards")
       North => (cardinal_jump piece, "Can't jump forwards")
       East  => (cardinal_jump piece, "Can't jump right")
       West  => (cardinal_jump piece, "Can't jump left")
       North_east => (diagonal_jump piece, "Can't jump forward-right")
       North_west => (diagonal_jump piece, "Can't jump forward-left")
       South_east => (diagonal_jump piece, "Can't jump backward-right")
       South_west => (diagonal_jump piece, "Can't jump backward-left")
       
||| A Chu Shogi piece
public export record Piece where
   constructor Make_piece
   piece_type : Piece_type
   piece_colour : Piece_colour

public export Eq Piece where
  (Make_piece pt1 pc1) == (Make_piece pt2 pc2) = pt1 == pt2 && pc1 == pc2
  
||| Is @ piece a monarch? I.e. does it affect victory conditions?
export is_king : Piece -> Bool
is_king piece =
    case piece_type piece of
      King => True
      Crown_prince => True
      _ => False      

export is_pawn_or_go_between : Piece -> Bool
is_pawn_or_go_between p = case piece_type p of
  Pawn       => True
  Go_between => True
  _          => False
  

||| All Piece_types that possess promoted forms
promotable_types : List Piece_type
promotable_types = [Lance, Reverse_chariot, Side_mover, Vertical_mover,
                 Rook, Gold_general, Copper_general, Silver_general, Bishop, Drunk_elephant,
                 Ferocious_leopard, Phoenix, Kylin, Blind_tiger,
                 Dragon_horse, Dragon_king, Pawn, Go_between]

||| Is @ piece a type that promotes?
export has_promotion : Piece_type -> Bool
has_promotion piece = elem piece promotable_types

||| Promoted version of @ piece
promotion : Piece -> Piece
promotion piece =
    case piece_type piece of
      Lance => record {piece_type = White_horse} piece
      Reverse_chariot => record {piece_type = Whale} piece
      Side_mover => record {piece_type = Free_boar} piece
      Vertical_mover => record {piece_type = Flying_ox} piece
      Rook => record {piece_type = Promoted_rook} piece
      Gold_general => record {piece_type = Promoted_gold_general} piece
      Silver_general => record {piece_type = Promoted_silver_general} piece
      Copper_general => record {piece_type = Promoted_copper_general} piece
      Bishop => record {piece_type = Promoted_bishop} piece
      Drunk_elephant => record {piece_type = Crown_prince} piece
      Ferocious_leopard => record {piece_type = Promoted_ferocious_leopard} piece
      Phoenix => record {piece_type = Promoted_phoenix} piece
      Kylin => record {piece_type = Promoted_kylin} piece
      Blind_tiger => record {piece_type = Flying_stag} piece
      Dragon_horse => record {piece_type = Horned_falcon} piece
      Dragon_king => record {piece_type = Soaring_eagle} piece
      Pawn => record {piece_type = Promoted_pawn} piece
      Go_between => record {piece_type = Promoted_go_between} piece
      _ => piece

||| Did @p promote?
|||
||| @p - piece before move
||| @st - promotion status before move
||| @pr - did @p promote?
||| @dec - did @p decline to promote
export promoted_piece : (p : Piece) -> (st : Promotion_status) -> (pr: Bool) -> (dec : Bool) -> (Piece, Promotion_status)
promoted_piece p st pr dec = case pr of
  True  => ((promotion p), No_promotion)
  False => case dec of
    True  => (p, Declined_to_promote)
    False => (p, st)
