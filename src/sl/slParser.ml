
open Sexplib

type t = Sexp.Annotated.t

let of_string domain s =
  Sexp.Annotated.of_string s
  |> SexpLoc.of_annotated domain

exception SyntaxError of SexpLoc.range * string * string

let error range msg =
  let open SexpLoc in
  let s = Printf.sprintf "%s at line %i, pos %i in %s\n"
      msg
      range.start_pos.Sexp.Annotated.line
      range.start_pos.Sexp.Annotated.col
      (string_of_domain range.domain)
  in
  raise (SyntaxError (range, msg, s))

let pcre_flag_of_c range = function
  | "i" -> `CASELESS
  | "m" -> `MULTILINE
  | "s" -> `DOTALL
  | "x" -> `EXTENDED
  | "u" -> `UTF8
  | c -> error range (Printf.sprintf "Unknown pcre flag: %s" c)

let pcre_flag_of_ast = function
  | SexpLoc.Atom (range, Sexplib.Type.Atom flag) -> pcre_flag_of_c range flag
  | SexpLoc.Atom (range, _)
  | SexpLoc.List (range, _, _) ->
    error range "PCRE flag expected"

let pcre_flags_of_ast = function
  | SexpLoc.List (range, flags, _) ->
    List.map pcre_flag_of_ast flags
  | SexpLoc.Atom (range, _) ->
    error range "PCRE flags list expected (or empty list: '()')"

let rec bool_to_ast t =
  let open SexpLoc in
  match t with
  | List (pos, (Atom (_, Type.Atom "and") :: lst), _) -> (
      match lst with
      | [] ->
        error pos "'and' cannot be empty"
      | lst ->
        let lst = List.map bool_to_ast lst |> List.rev in
        AST.And lst
    )

  | List (pos, (Atom (_, Type.Atom "or") :: lst), _) -> (
      match lst with
      | [] ->
        error pos "'or' cannot be empty"
      | lst ->
        let lst = List.map bool_to_ast lst |> List.rev in
        AST.Or lst
    )
    
  | List (pos, [Atom (_, Type.Atom "not"); expr], _) ->
    let expr = bool_to_ast expr in
    AST.Not expr

  | Atom (pos, Type.Atom "true") -> AST.True

  | Atom (pos, Type.Atom "false") -> AST.False


  | List (pos, (Atom (_, Type.Atom "filemask") :: flags :: lst), _) -> (
      let flags = pcre_flags_of_ast flags in
      match lst with
      | [] ->
        error pos "'filemask' cannot be empty"
      | lst ->
        let lst = List.map
            (function
              | Atom (pos, Type.Atom mask) -> mask
              | List (pos, _, _)
              | Atom (pos, _) -> error pos "'filemask' expects strings list argument"
            ) lst |> List.rev
        in
        AST.Filemask (flags, lst)
    )

  | List (pos, (Atom (_, Type.Atom "bodymask") :: flags :: lst), _) -> (
      let flags = pcre_flags_of_ast flags in
      match lst with
      | [] ->
        error pos "'bodymask' cannot be empty"
      | lst ->
        let lst = List.map
            (function
              | Atom (pos, Type.Atom mask) -> mask
              | List (pos, _, _)
              | Atom (pos, _) -> error pos "'bodymask' expects strings list argument"
            ) lst |> List.rev
        in
        AST.Bodymask (flags, lst)
    )

  | List (pos, _, _)
  | Atom (pos, _) ->
    error pos "boolean argument expected"
  
and t_to_ast t =
  let open SexpLoc in
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
