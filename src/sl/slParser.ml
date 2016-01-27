
open Sexplib

type t = Sexp.Annotated.t

let of_string s =
  Sexp.Annotated.of_string s

exception SyntaxError of Sexp.Annotated.range * string * string

let error pos msg =
  let open Sexp.Annotated in
  let s = Printf.sprintf "%s at line %i, pos %i\n" msg pos.start_pos.line pos.start_pos.col in
  raise (SyntaxError (pos, msg, s))

let rec bool_to_ast t =
  let open Sexp.Annotated in
  match t with
  | List (pos, [Atom (_, Type.Atom "and"); lst], _) -> (
      match lst with
      | List (pos, [], _) ->
        error pos "'and' expects non-empty list argument"
      | List (pos, lst, _) ->
        let lst = List.map bool_to_ast lst |> List.rev in
        AST.And lst
      | Atom (pos, _) ->
        error pos "'and' expects list argument"
    )

  | List (pos, [Atom (_, Type.Atom "or"); lst], _) -> (
      match lst with
      | List (pos, [], _) ->
        error pos "'or' expects non-empty list argument"
      | List (pos, lst, _) ->
        let lst = List.map bool_to_ast lst |> List.rev in
        AST.Or lst
      | Atom (pos, _) ->
        error pos "'or' expects list argument"
    )

  | List (pos, [Atom (_, Type.Atom "not"); expr], _) ->
    let expr = bool_to_ast expr in
    AST.Not expr

  | Atom (pos, Type.Atom "true") -> AST.True

  | Atom (pos, Type.Atom "false") -> AST.False

  | List (pos, [Atom (_, Type.Atom "filemask"); lst], _) -> (
      match lst with
      | List (pos, [], _) ->
        error pos "'filemask' expects non-empty list argument"
      | List (pos, lst, _) ->
        let lst = List.map
            (function
              | Atom (pos, Type.Atom mask) -> mask
              | List (pos, _, _)
              | Atom (pos, _) -> error pos "'filemask' expects strings list argument"
            ) lst |> List.rev
        in
        AST.Filemask lst
      | Atom (pos, _) ->
        error pos "'filemask' expects list argument"
    )

  | List (pos, [Atom (_, Type.Atom "bodymask"); lst], _) -> (
      match lst with
      | List (pos, [], _) ->
        error pos "'bodymask' expects non-empty list argument"
      | List (pos, lst, _) ->
        let lst = List.map
            (function
              | Atom (pos, Type.Atom mask) -> mask
              | List (pos, _, _)
              | Atom (pos, _) -> error pos "'bodymask' expects strings list argument"
            ) lst |> List.rev
        in
        AST.Bodymask lst
      | Atom (pos, _) ->
        error pos "'bodymask' expects list argument"
    )

  | List (pos, _, _)
  | Atom (pos, _) ->
    error pos "boolean argument expected"
  
and t_to_ast t =
  let open Sexp.Annotated in
  match t with

  | List (pos, [Atom (_, Type.Atom "notify"); Atom (_, Type.Atom msg)], _) ->
    AST.Notify msg

  | List (pos, [Atom (_, Type.Atom "if"); expr; app], _) ->
    let expr = bool_to_ast expr in
    let app = t_to_ast app in
    AST.If (expr, app)

  | List (pos, [Atom (_, Type.Atom "set-context"); Atom (_, Type.Atom filename); app], _) ->
    let app = t_to_ast app in
    AST.SetContext (filename, app)

  | List (pos, (Atom (_, Type.Atom "seq") :: lst), _) -> (
      match lst with
      | [] ->
        error pos "'seq' cannot be empty"
      | lst ->
        let lst = List.map t_to_ast lst |> List.rev in
        AST.Seq lst
    )
    

  | Atom (pos, Type.Atom name) ->
    error pos ("unexpected command: " ^ name)
  | Atom (pos, _)
  | List (pos, [List _], _) ->
    error pos "list found where expression expected"
  | List (pos, _, _) ->
    error pos "undefined expression"
