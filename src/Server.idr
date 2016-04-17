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

||| Server executable
module Main

import MHD.Daemon
import MHD.Response
import MHD.POST
import HTTP.Status_codes
import System
import Data.String
import Effects
import Effect.File
import Static_pages
import CFFI

%include C "server.h"
%link C "lmh.o"
%link C "/usr/lib64/libmicrohttpd.so"

string_to_c : String -> IO Ptr
string_to_c str = foreign FFI_C "string_to_c" (String -> IO Ptr) str

||| maximum number of bytes to use for internal buffering by POST processor
post_buffer_size : Bits64
post_buffer_size = 256


||| Response for successful POST
complete_page : String                                     
complete_page = "<html><body>Login processing - TODO.</body></html>"

||| Load text of static HTML page corresponding to @url
|||
||| @url - requested path to static page
load_page : (url : String) -> IO (String, Int)
load_page url = run (load_static_page url)
   
||| Format of data passed between invocations of the request handler callback
|||
||| Fields are:
||| Connection type
||| Answer string
||| POST processor
||| Response code to accompany answer string
||| login name
||| email
||| password
||| guest-flag
||| remember-flag
connection_information_struct : Composite
connection_information_struct = STRUCT [I8, PTR, PTR, I32, PTR, PTR, PTR, I8, I8]
  
connection_type_field_index : Nat
connection_type_field_index = 0

answer_string_field_index : Nat
answer_string_field_index = 1

pp_field_index : Nat
pp_field_index = 2

code_field_index : Nat
code_field_index = 3

login_name_field_index : Nat
login_name_field_index = 4

email_field_index : Nat
email_field_index = 5

password_field_index : Nat
password_field_index = 6

guest_field_index : Nat
guest_field_index = 7

remember_field_index : Nat
remember_field_index = 8

||| Access to data passed between invocations of the request handler callback
connection_information : IO Ptr
connection_information = foreign FFI_C "&connection_information" (IO Ptr)

||| Request method
POST_type : Bits8
POST_type = 1

||| Request method
GET_type : Bits8
GET_type = 0
 
||| Termination notification handler
request_completed : Request_completed_handler
request_completed cls conn con_cls toe = unsafePerformIO $ do
  putStrLn "Request completed"
  conn_info <- peek PTR con_cls
  if conn_info == null then
    pure ()
  else do
    conn_fld <- pure $ (connection_information_struct#connection_type_field_index) conn_info
    connection_type <- peek I8 conn_fld
    if connection_type == POST_type then do
      pp_fld <- pure $ (connection_information_struct#pp_field_index) conn_info
      pp <- peek PTR pp_fld
      if pp /= null then do
        destroy_post_processor pp
      else
        pure ()
      clean_up conn_info
    else clean_up conn_info
 where
   clean_up : Ptr -> IO ()
   clean_up conn_info = do
     free conn_info
     poke PTR con_cls null
     pure ()
 
notify_completed_wrapper : IO Ptr
notify_completed_wrapper = foreign FFI_C "%wrapper" ((CFnPtr Request_completed_handler) -> IO Ptr) (MkCFnPtr request_completed)
 
start_options : Start_options
start_options = unsafePerformIO $ do 
  wr <- notify_completed_wrapper
  pure $ record {notify_completed = (wr, null), thread_pool_size=9} default_options

||| POST processor
iterate_post : POST_processor
iterate_post cls kind key filename content_type transfer_encoding post_data offset size = unsafePerformIO $ do
  putStrLn key
  answer_fld <- pure $ (connection_information_struct#1) cls
  code_fld <- pure $ (connection_information_struct#3) cls
  str <- string_to_c "What are we going to use this for?" 
  poke PTR answer_fld str
  poke I32 code_fld (prim__zextInt_B32 HTTP_ok)
  case key of
    "login"       => store_string ((connection_information_struct#login_name_field_index) cls)
    "email"       => store_string ((connection_information_struct#email_field_index) cls)
    "password"    => store_string ((connection_information_struct#password_field_index) cls)      
    "guest"       => store_boolean ((connection_information_struct#guest_field_index) cls)      
    "remember_me" => store_boolean ((connection_information_struct#remember_field_index) cls)    
    "commit"      => pure MHD_YES
    _ => do
      putStrLn "Unmatched key"
      pure MHD_NO
 where
    store_string : CPtr -> IO Int
    store_string field_index = do
      fld <- pure $ field_index
      str <- string_to_c post_data
      poke PTR fld str
      pure MHD_YES
    store_boolean : CPtr -> IO Int
    store_boolean field_index = do
      fld <- pure $ field_index
      case post_data of
        "on" => do
          putStrLn "Got checkbox"
          poke I8 fld 1
        _ => do
          poke I8 fld 0
          -- doesn't happen in practice
      pure MHD_YES
       
iterator_wrapper : IO Ptr
iterator_wrapper = foreign FFI_C "%wrapper" ((CFnPtr POST_processor) -> IO Ptr) (MkCFnPtr iterate_post)

||| Create the connection_information_struct
|||
||| @conn    - the connection the request is running on
||| @method  - the request method
||| @con_cls - request callback-specific data
create_connection_information : (conn : Ptr) -> (method : String) -> (con_cls : Ptr) -> IO Int
create_connection_information conn method con_cls = do
  CPt conn_info _ <- alloc connection_information_struct
  if conn_info == null then
    pure MHD_NO
  else do
    if method == "POST" then do
      wr <- iterator_wrapper
      pp <- create_post_processor conn post_buffer_size wr conn_info
      if pp == null then do
        free conn_info
        pure MHD_NO
      else do
        pp_fld <- pure $ (connection_information_struct#pp_field_index) conn_info
        poke PTR pp_fld pp
        answer_fld <- pure $ (connection_information_struct#answer_string_field_index) conn_info
        str <- string_to_c complete_page 
        poke PTR answer_fld str        
        success conn_info con_cls POST_type
    else
      success conn_info con_cls GET_type
 where
   success : Ptr -> Ptr -> Bits8 -> IO Int
   success conn_info con_cls type = do 
     conn_fld <- pure $ (connection_information_struct#connection_type_field_index) conn_info
     poke I8 conn_fld type
     poke PTR con_cls conn_info
     pure MHD_YES
     
||| Top-level URL handler
answer_to_connection : Request_handler
answer_to_connection cls conn url method version up_d up_d_sz con_cls = unsafePerformIO $ do
  conn_info <- peek PTR con_cls
  if conn_info == null then do
    putStrLn $ concat ["Request created ", method, " ", url]
    create_connection_information conn method con_cls
  else do
    putStrLn $ concat ["Request started ", method, " ", url]
    case method of
      "GET" => do
        (page, resp_code) <- load_page url
        response <- create_response_from_buffer page MHD_RESPMEM_PERSISTENT
        ret <- queue_response conn resp_code response
        destroy_response response
        pure ret
      "POST" => do
        putStrLn "Answering POST"
        size <- peek I64 up_d_sz
        if size /= 0 then do
          pp_fld <- pure $ (connection_information_struct#pp_field_index) conn_info
          pp <- peek PTR pp_fld
          ret <- post_process pp up_d size
          poke I64 up_d_sz 0
          pure MHD_YES
        else do
          pure MHD_NO
      _ => do
        pure MHD_NO
    
wrapper : IO Ptr
wrapper = foreign FFI_C "%wrapper" ((CFnPtr Request_handler) -> IO Ptr) (MkCFnPtr  answer_to_connection)

||| Run the server listening on @port
run_main : Bits16 -> IO ()
run_main port = do
  wr <- wrapper
  daemon <- start_daemon_with_options MHD_USE_SELECT_INTERNALLY port null null (wr) null start_options
  case daemon == null of
    True  => exit 1
    False => do
      x <- getChar
      stop_daemon daemon
      pure ()
      
main : IO ()
main = do
  args <- getArgs
  case index' 1 args of
    Nothing => do
      putStrLn $ "Usage: chu_shogi_server PORT" 
      exit 1
    (Just port) => do
      case length args == 2 of
        True => do
          case parsePositive {a=Bits16} port of
            Nothing => do
              putStrLn $ "Usage: chu_shogi_server PORT" 
              exit 1
            Just p => run_main p        
        False => do
          putStrLn $ "Usage: chu_shogi_server PORT" 
          exit 1


