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

||| Parser for format-3 .csg files (as produced by Chu_shogi 2.10 (?) by Colin Adams) 
|||
||| Only the notation present in the six historical games has been parsed. Passes therefore aren't dealt with, nor Igui
module Csg_parser

import Lightyear
import Lightyear.Strings 
import Lightyear.Char
import Board
import Piece
import Coordinate
import Move_validity
import Move
import Move_state
import Game_state
import Effect.File
import Effects
import Data.AVL.Dict
import Control.Monad.State
import Data.Stack
import Data.So
import Debug.Trace

%hide parse

||| State parser for threading the current game state through the parser. The final state will be the result of the parser
My_parser : Type -> Type
My_parser = ParserT String (StateT (Game_state, Dict String Game_state) Identity)

parse : My_parser a -> (string_to_parse : String) -> (initial_game_state: (Game_state, Dict String Game_state)) -> Either String a
parse p i gs = let res = evalState (execParserT p i) gs
  in case res of
    Success _ x => Right x
    Failure es  => Left $ formatError i es
  
parse_format : My_parser Int
parse_format = do
  fmt <- integer
  nl  <- endOfLine
  pure $ fmt

parse_single_string : My_parser String
parse_single_string = do
  name  <-many letter
  nl    <- endOfLine
  pure $ pack name

parse_name : My_parser String
parse_name = do
  name  <- many (noneOf "\n")
  nl    <- endOfLine
  pure $ pack name

parse_handicap : My_parser String
parse_handicap = do
  board <- many letter
  sep   <- dot
  dat   <- many letter
  nl    <- endOfLine
  parse_single_string
 
||| Combine one or two characters into a list
abbreviation : Char -> Maybe Char -> List Char
abbreviation abbrev1 abbrev2 = case abbrev2 of
    Nothing => [abbrev1]
    Just l  => abbrev1 :: [l]
 

put_move : Move -> Game_state -> Dict String Game_state -> My_parser Valid_move
put_move mv gs hcps = do
  let truth = is_move_valid (mv, gs)
  case choose truth of
    Right _  => do
      case is_valid_move mv gs of
        (_, reas) => fail $ "Move (" ++ show mv ++ ") is invalid at that point in the game. Reason is " ++ reas
    Left Oh => do
      let vm = (mv, gs, Oh)
      let gs' = update_with_valid_move vm
      put (gs', hcps)
      pure vm
     
capture_move : Board -> Move_state -> King_count -> Move_stack -> Piece -> Coordinate -> Coordinate -> Piece_colour -> Maybe Char -> Maybe Char -> My_parser Valid_move
capture_move b mv_st kc stk p c1 c2 col prm dcl = do
  let mp = piece_at c1 b
  case mp of
    Nothing         => fail "No piece on source square for capture"
    Just (p, pr_st) => case piece_at c2 b of
      Nothing => fail "No piece on target square for capture"
      Just (p2, _) => do
        let pr = the Bool (case prm of 
          Just _ => True
          Nothing => False )
        let nopr = the Bool (case dcl of 
          Just _ => True
          Nothing => False )          
        let mv = Capture p c1 p2 c2 pr nopr
        (gs, hcps) <- get
        put_move mv gs hcps

capture_and_move : Piece -> Coordinate -> Piece -> Coordinate -> Coordinate -> My_parser Valid_move
capture_and_move p c1 p2 c2 c3 = do
  let mv = Double_move p c1 p2 c2 Nothing c3 
  (gs, hcps) <- get
  put_move mv gs hcps

double_capture : Piece -> Coordinate -> Piece -> Coordinate -> Piece -> Coordinate -> My_parser Valid_move
double_capture p c1 p2 c2 p3 c3 = do
  let mv = Double_move p c1 p2 c2 (Just p3) c3 
  (gs, hcps) <- get
  put_move mv gs hcps

non_capture_move : Board -> Move_state -> King_count -> Move_stack -> Piece -> Coordinate -> Coordinate -> Piece_colour -> Maybe Char -> Maybe Char -> My_parser Valid_move
non_capture_move b mv_st kc stk p c1 c2 col prm dcl = do
  let mp = piece_at c1 b
  case mp of
    Nothing         => fail "No piece on source square for capture"
    Just (p, pr_st) => case piece_at c2 b of
      Just _  => fail $ "target square " ++ (show c2) ++  " is occupied"
      Nothing => do
        let pr = the Bool (case prm of 
          Just _ => True
          Nothing => False )
        let nopr = the Bool (case dcl of 
          Just _ => True
          Nothing => False )          
        let mv = Simple_move p c1 c2 pr nopr
        (gs, hcps) <- get
        put_move mv gs hcps
 
piece_colour_from_state : Move_state -> Piece_colour
piece_colour_from_state mv_st =
  case black_to_play mv_st of
    True  => Black
    False => White
   
is_capturing : String -> Bool
is_capturing move_type = if move_type == "x" then True else False
                         
single_move : (abbrev : String) -> (c1 : Coordinate) -> (move_type : String) -> (c2: Coordinate) -> Maybe Char -> Maybe Char -> My_parser Valid_move
single_move abbrev c1 move_type c2 prm dcl = do
  (Running b mv_st kc stk, hcps) <- get | (Not_running reas, hcps2) => fail "Impossible game state"
  case trace (display_board b) (piece_at c1 b) of
    Nothing => fail "No piece at source square"
    Just (p, pr_st) => do
      if abbrev /= (abbreviation $ piece_type p) then
        fail $ "Wrong abbreviation: " ++ abbrev ++ " at " ++ show c1 ++ ", actual piece on board is " ++  (abbreviation $ piece_type p) ++ "\n" ++ (display_board b)
      else do
        let col = piece_colour_from_state mv_st
        if piece_colour p == col then
          if is_capturing move_type then
            capture_move b mv_st kc stk p c1 c2 col prm dcl
          else
            non_capture_move b mv_st kc stk p c1 c2 col prm dcl
        else
          fail "Incorrect piece colour for move"


double_move : (abbrev : String) -> (c1 : Coordinate) -> (move_type : String) -> (c2 : Coordinate) -> My_parser Valid_move
double_move abbrev c1 move_type c2 = do
  (Running b mv_st kc stk, hcps) <- get | (Not_running reas, hcps2) => fail "Impossible game state"
  case piece_at c1 b of
    Nothing => fail "No piece at source square"
    Just (p, pr_st) => do
      if abbrev /= (abbreviation $ piece_type p) then
        fail $ "Wrong abbreviation: " ++ abbrev ++ ", actual piece on board is " ++  (abbreviation $ piece_type p)
      else do
        let col = piece_colour_from_state mv_st
        if piece_colour p == col then
          if is_capturing move_type then do
            case piece_at c2 b of
              Nothing => fail "No piece on first target square"
              Just (p2, _) => do
                spaces
                mt2 <- (string "-" <|> string "x")
                spaces
                file3 <- integer
                rank3 <- letter
                endOfLine
                let c3 = coordinate_from_rank_and_file rank3 file3
                case c3 of
                  Nothing => fail "No piece on second target for double capture"
                  Just c3' => do
                    let capt2 = is_capturing mt2
                    case piece_at c3' b of
                      Nothing     => if capt2 then
                                  fail "Second capture on empty square"
                        else
                          capture_and_move p c1 p2 c2 c3'
                      Just (p3, _) => if capt2 then
                           double_capture p c1 p2 c2 p3 c3'
                        else
                          fail "Capture and move on empty square"            
          else
            fail "Double move must capture on the first move"
        else
          fail "Incorrect piece colour for move"
        
||| Parse + prefix
parse_promoted : My_parser (Maybe Char)
parse_promoted = do
  opt (char ' ')
  opt (char '+')

||| Parse + suffix
parse_promote : My_parser (Maybe Char)
parse_promote = opt (char '+')

||| Parse = suffix
parse_decline : My_parser (Maybe Char)
parse_decline = opt (char '=')
    
parse_move : My_parser Valid_move
parse_move = do
  skip integer
  spaces
  pr <- parse_promoted
  abbrev1 <- letter
  abbrev2 <- opt letter
  let abbrev = (abbreviation abbrev1 abbrev2)
  spaces
  file <- integer
  rank <- letter
  spaces
  mv_type <- (string "-" <|> string "x" <|> string "x!")
  spaces
  file2 <- integer
  rank2 <- letter
  let c1 = coordinate_from_rank_and_file rank file
  let c2 = coordinate_from_rank_and_file rank2 file2
  prm <- parse_promoted
  dcl <- parse_decline
  case (c1, c2) of
    (Just c1', Just c2') => do
      (endOfLine >! (single_move (abbrev' pr abbrev) c1' mv_type c2' prm dcl) <|> 
                (double_move (abbrev' pr abbrev) c1' mv_type c2'))
    _ => fail "Invalid coordinates"
 where abbrev' : Maybe Char -> List (Char) -> String
       abbrev' p abb = case p of
          Nothing => pack abb
          Just p' => pack $ p' :: abb

||| Parse a String (the contents of a .csg file) yielding a Game_state
from_csg : My_parser Game_state
from_csg = do
  fmt <- parse_format
  case fmt == 3 of
    False => fail "Bad format - only format 3 is supported"
    True  => do
      hdcp <- parse_handicap
      (_, handicaps) <- get
      case Data.AVL.Dict.lookup hdcp handicaps of
        Nothing => fail $ "Bad handicap name: " ++ hdcp
        Just gs => do
          put (gs, handicaps)
          blk_nm <- parse_name
          wht_nm <- parse_name
          skip parse_name  -- The next lines are some kind of comment facility I think - TODO need to check.
          skip endOfLine
          skip endOfLine
          skip endOfLine
          skip endOfLine
          z <- char '0'  -- "Move zero" - an artifact of the implementation. We don't need this
          skip parse_name
          moves <- many parse_move
          (gs', _) <- get
          pure gs'
      
       
||| Re-construct the state of the game at the end of a Format-3 .csg file
|||
||| @contents - contents of a .csg file
||| @handicaps - map of handicap names to initial game_states
export parse_csg : (contents : String) -> (handicaps : Dict String Game_state) -> Either String Game_state
parse_csg contents handicaps = let st = Not_running Not_yet_started
  in parse from_csg contents (st, handicaps)

||| Re-construct the state of the game at the end of a Format-3 .csg file
|||
||| @file_name - absolute or relative file-system path to .csg file e.g. /home/colin/Downlads/hist.csg 
||| @handicaps - map of handicap names to initial game_states
export parse_csg_file : (file_name : String) -> (handicaps : Dict String Game_state) -> Eff (Either String Game_state) [FILE_IO ()]
parse_csg_file file_name handicaps = do
  ei <- Effect.File.Default.readFile file_name
  case ei of
    Left e  => pure $ Left e
    Right c => pure $ parse_csg c handicaps

