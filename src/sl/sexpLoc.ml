
open Sexplib

type domain =
  | File of string

type range = {
  domain : domain;
  start_pos : Sexp.Annotated.pos;
  end_pos : Sexp.Annotated.pos;
}

type t =
  | Atom of range * Type.t
  | List of range * t list * Type.t

let string_of_domain = function
  | File s -> "file " ^ s

let rec of_annotated domain = function
  | Sexp.Annotated.Atom (range, t) ->
    let range = {
      domain;
      start_pos = range.Sexp.Annotated.start_pos;
      end_pos = range.Sexp.Annotated.end_pos;
    } in
    Atom (range, t)
  | Sexp.Annotated.List (range, lst, t) ->
    let range = {
      domain;
      start_pos = range.Sexp.Annotated.start_pos;
      end_pos = range.Sexp.Annotated.end_pos;
    } in
    let lst = List.map (of_annotated domain) lst in
    List (range, lst, t)
    
let rec to_annotated = function
  | Atom (range, t) ->
    let range = {
      Sexp.Annotated.start_pos = range.start_pos;
      Sexp.Annotated.end_pos = range.end_pos;
    } in
    Sexp.Annotated.Atom (range, t)
  | List (range, lst, t) ->
    let range = {
      Sexp.Annotated.start_pos = range.start_pos;
      Sexp.Annotated.end_pos = range.end_pos;
    } in
    let lst = List.map to_annotated lst in
    Sexp.Annotated.List (range, lst, t)

let rec to_sexp = function
  | Atom (_, Sexp.Atom t) ->
    Sexp.Atom t
  | Atom (_, _) ->
    raise Exit
  | List (_, lst, _) ->
    let lst = List.map to_sexp lst in
    Sexp.List lst

let to_string t =
  to_sexp t |> Sexp.to_string_hum

let print t =
  to_string t |> print_endline
