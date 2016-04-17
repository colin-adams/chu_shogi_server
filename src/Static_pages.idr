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

||| Router for requests to static files
module Static_pages

import Data.AVL.Dict
import Effects
import Effect.File
import HTTP.Status_codes

||| HTML page for Not Found errors
not_found_page : String -> String
not_found_page url = concat ["<html><body>Requested URL (", url, ") could not be found</body></html>"]

||| HTML page for internal server errors
internal_error_page : String -> String -> String
internal_error_page url reason = 
  concat  ["<html><body>The server encountered an error (", reason , ") when trying to serve ", url, "</body></html>"]
  
||| map of URLs to page texts
known_pages : Dict String String
known_pages = fromList [("/", "login.html"), ("/index.html", "login.html"), ("/login.html", "login.html")]

||| Load text of static HTML page corresponding to @url
|||
||| @url - requested path to static page
export load_static_page : (url : String) -> Eff (String, Int) [FILE_IO ()]
load_static_page url = do
  case lookup url known_pages of
    Nothing => pure ((not_found_page url), HTTP_not_found)
    Just fn => do
      ei <- Effect.File.Default.readFile (concat ["files/", fn])
      case ei of
        Left e  => pure ((internal_error_page url "Could not read file"), HTTP_internal_server_error)
        Right c => pure (c, HTTP_ok)
