
open Pipe.Sig

let file_of_string file = {
  file;
  tail = [];
}

let of_string s =
  let open String in
  let l = length s in
  if l > 1 && s.[0] = '/' && s.[1] = '/' then
    Meta (sub s 2 (l-2))
  else
    File (file_of_string s)

let file_to_string r =
  r.file

let to_string = function
  | File r -> file_to_string r
  | Meta r -> r

let record_separator = '\000'
