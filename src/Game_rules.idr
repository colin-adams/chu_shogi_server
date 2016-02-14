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

||| the high-level view of the rules of Chu Shogi
module Game_rules

import Game_state
import Board
import Move_state
import Move
import Effects
import Data.Fin
import Data.So
import Data.AVL.Dict

%default total
%access private

{-

Possible Actions are:

{Preconditions}                                                                                                     Action             {Postconditions}

{Game is not running with a reason of Not_yet_started}                                                              Start a game       {Game is running with an empty move history, both king counts and non_kings > FZ}

{Game is running and Black_kings = FS n and White_kings = FZ}                                                       Black wins         {Game is not running with a reason of Black_won}

{Game is running and Black_kings = FZ and White_kings = FS n}                                                       White wins         {Game is not running with a reason of White_won}

{Game is running and Black_kings = FS n == White_kings = FS n and black_kings = White_kings and non_kings = FZ}     Declare draw       {Game is not running with a reason of Draw}

{Game is running and Black_kings = FS n and White_kings = FS n and non_kings = FS n}                                Agree draw         {Game is not running with a reason of Draw}

{Game is running and Black_kings = FS n and White_kings = FS n and white is on move}                                White resigns      {Game is not running with a reason of Black_won}

{Game is running and Black_kings = FS n and White_kings = FS n and black is on move}                                Black resigns      {Game is not running with a reason of White_won}

{Game is running and Black_kings = FS n and White_kings = FS n and black is on move}                                Black makes a move {Game is running and white is on move or Game is not running with a reason of Black_won}

{Game is running and Black_kings = FS n and White_kings = FS n and white is on move}                                White makes a move {Game is running and black is on move or Game is not running with a reason of White_won}

{}                                                                                                                  Display the game   {}
-}

||| Bounded natural numbers for enforcing preconditions below
Bounded : Nat -> Type
Bounded x = (n : Nat ** So (n > 0 && n < x))

||| Auxiliary for preconditions
king_count_from_bounded : Bounded 3 -> Fin 3
king_count_from_bounded (k_count ** prf) = case natToFin k_count 3 of
                      Just n => n
                      Nothing => FS FZ -- We should be able to prove this branch doesn't occur

||| Auxiliary for preconditions
other_count_from_bounded : Bounded 95 -> Fin 95
other_count_from_bounded (o_count ** prf) = case natToFin o_count 95 of
                      Just n => n
                      Nothing => FS FZ -- We should be able to prove this branch doesn't occur

||| precondition for Black_wins
black_winning : Bounded 3 -> Fin 95 -> King_count
black_winning wkc nkc = Make_king_count FZ (king_count_from_bounded wkc) nkc

||| precondition for White_wins
white_winning : Bounded 3 -> Fin 95 -> King_count
white_winning bkc nkc = Make_king_count (king_count_from_bounded bkc) FZ nkc

||| precondition for Declare_draw
compulsory_draw : Bounded 3 -> King_count
compulsory_draw bw = Make_king_count (king_count_from_bounded bw) (king_count_from_bounded bw) FZ

||| precondition for Agree_draw
voluntary_draw : Bounded 3 -> Bounded 3 -> Bounded 95 -> King_count
voluntary_draw b w oth = Make_king_count (king_count_from_bounded b) (king_count_from_bounded w) (other_count_from_bounded oth)

||| precondition for Black_resigns or White_resigns
resignation : Bounded 3 -> Bounded 3 -> Fin 95 -> King_count
resignation b w oth = Make_king_count (king_count_from_bounded b) (king_count_from_bounded w) oth

{- ||| precondition for white to resign
white_to_move : Move_state
white_to_move = (Make_move_state False _ _ _ _)

||| precondition for black to resign
black_to_move : Move_state
black_to_move = (Make_move_state True _ _ _ _) -}

||| postcondition for game_started
no_result_yet : Bounded 3 -> Bounded 3 -> Bounded 95 -> King_count
no_result_yet b w oth = Make_king_count (king_count_from_bounded b) (king_count_from_bounded w) (other_count_from_bounded oth)

data Game_rules : Effect where

  Start_game    : (b : Board, m : Move_state) -> sig Game_rules (Board, Move_state) (Chu_game (Not_running Not_yet_started)) 
    (\bm => Chu_game (Running (fst bm) (snd bm) no_result_yet _)) -- TODO arguments to no_result_yet and validity conditions on arguments and initial move stack
  Black_wins    : sig Game_rules () (Chu_game (Running _ _ black_winning _)) (Chu_game (Not_running Black_won))
  White_wins    : sig Game_rules () (Chu_game (Running _ _ white_winning _)) (Chu_game (Not_running White_won))
  Declare_draw  : sig Game_rules () (Chu_game (Running _ _ compulsory_draw _)) (Chu_game (Not_running Draw))
  Agree_draw    : sig Game_rules () (Chu_game (Running _ _ voluntary_draw _)) (Chu_game (Not_running Draw))
  White_resigns : sig Game_rules () (Chu_game (Running _ (Make_move_state False _ _ _ _) resignation _)) (Chu_game (Not_running Black_won))
  Black_resigns : sig Game_rules () (Chu_game (Running _ black_to_move resignation _)) (Chu_game (Not_running White_won))  
  Display_game  : sig Game_rules () (Chu_game h) -- very unsure if this belongs here
  Black_moves   : (m : Move) -> sig Game_rules () (Chu_game (Running _ black_to_move _ _)) (Chu_game (Running _ white_to_move _ _))
  White_moves   : (m : Move) -> sig Game_rules () (Chu_game (Running _ white_to_move _ _)) (Chu_game (Running _ black_to_move _ _)) -- these all need more prescision to show the state changes -- should we have a Bool result for validity? or a dependent pair on input, with a witness to validity?
  
CHU_SHOGI : Game_state -> EFFECT
CHU_SHOGI h = MkEff (Chu_game h) Game_rules

