
open Sexplib

type domain =
  | File of string
  | Argument
  | Virtual

type range = {
  domain : domain;
  start_pos : Sexp.Annotated.pos;
  end_pos : Sexp.Annotated.pos;
}

type t =
  | Atom of range * string
  | List of range * t list

let string_of_domain = function
  | File s -> "file " ^ s
  | Argument -> "command line argument"
  | Virtual -> "root of tree"

let rec of_annotated domain = function
  | Sexp.Annotated.Atom (range, Type.Atom t) ->
    let range = {
      domain;
      start_pos = range.Sexp.Annotated.start_pos;
      end_pos = range.Sexp.Annotated.end_pos;
    } in
    Atom (range, t)
  | Sexp.Annotated.Atom (range, Type.List _) ->
    raise Exit
  | Sexp.Annotated.List (range, lst, _) ->
    let range = {
      domain;
      start_pos = range.Sexp.Annotated.start_pos;
      end_pos = range.Sexp.Annotated.end_pos;
    } in
    let lst = List.map (of_annotated domain) lst in
    List (range, lst)
    
let rec to_sexp = function
  | Atom (_, t) ->
    Sexp.Atom t
  | List (_, lst) ->
    let lst = List.map to_sexp lst in
    Sexp.List lst

let to_string t =
  to_sexp t |> Sexp.to_string_hum

let print t =
  to_string t |> print_endline
