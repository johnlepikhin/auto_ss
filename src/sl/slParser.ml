
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
  | SexpLoc.Atom (range, flag) -> pcre_flag_of_c range flag
  | SexpLoc.List (range, _) ->
    error range "PCRE flag expected"

let pcre_flags_of_ast = function
  | SexpLoc.List (range, flags) ->
    List.map pcre_flag_of_ast flags
  | SexpLoc.Atom (range, _) ->
    error range "PCRE flags list expected (or empty list: '()')"

let rec bool_to_ast t =
  let open SexpLoc in
  match t with
  | List (pos, (Atom (_, "and") :: lst)) -> (
      match lst with
      | [] ->
        error pos "'and' cannot be empty"
      | lst ->
        let lst = List.map bool_to_ast lst |> List.rev in
        AST.And lst
    )

  | List (pos, (Atom (_, "or") :: lst)) -> (
      match lst with
      | [] ->
        error pos "'or' cannot be empty"
      | lst ->
        let lst = List.map bool_to_ast lst |> List.rev in
        AST.Or lst
    )
    
  | List (pos, [Atom (_, "not"); expr]) ->
    let expr = bool_to_ast expr in
    AST.Not expr

  | Atom (pos, "true") -> AST.True

  | Atom (pos, "false") -> AST.False


  | List (pos, (Atom (_, "filemask") :: flags :: lst)) -> (
      let flags = pcre_flags_of_ast flags in
      match lst with
      | [] ->
        error pos "'filemask' cannot be empty"
      | lst ->
        let lst = List.map
            (function
              | Atom (pos, mask) -> mask
              | List (pos, _) -> error pos "'filemask' expects strings list argument"
            ) lst |> List.rev
        in
        AST.Filemask (flags, lst)
    )

  | List (pos, (Atom (_, "bodymask") :: flags :: lst)) -> (
      let flags = pcre_flags_of_ast flags in
      match lst with
      | [] ->
        error pos "'bodymask' cannot be empty"
      | lst ->
        let lst = List.map
            (function
              | Atom (pos, mask) -> mask
              | List (pos, _) -> error pos "'bodymask' expects strings list argument"
            ) lst |> List.rev
        in
        AST.Bodymask (flags, lst)
    )

  | List (pos, _)
  | Atom (pos, _) ->
    error pos "boolean argument expected"
  
and t_to_ast t =
  let open SexpLoc in
  match t with

  | List (pos, [Atom (_, "notify"); Atom (_, msg)]) ->
    AST.Notify msg

  | List (pos, [Atom (_, "if"); expr; app]) ->
    let expr = bool_to_ast expr in
    let app = t_to_ast app in
    AST.If (expr, app)

  | List (pos, [Atom (_, "set-context"); Atom (_, filename); app]) ->
    let app = t_to_ast app in
    AST.SetContext (filename, app)

  | List (pos, (Atom (_, "seq") :: lst)) -> (
      let lst = List.map t_to_ast lst |> List.rev in
      AST.Seq lst
    )
    

  | Atom (pos, name) ->
    error pos ("unexpected command: " ^ name)
  | List (pos, [List _]) ->
    error pos "list found where expression expected"
  | List (pos, _) ->
    error pos "undefined expression"
