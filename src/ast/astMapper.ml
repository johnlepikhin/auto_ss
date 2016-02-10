
open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

let exp_construct loc n v = Exp.construct ~loc (Location.mkloc (Lident n) loc) v

let loc_error ~loc msg =
  let err = Location.error ~loc msg in
  raise (Location.Error err)

let rec strings_of_args name = function
  | [] -> []

  | (_, ({ pexp_desc = Pexp_constant (Const_string (v, _));
           pexp_loc = loc
         })) :: tl ->
    v :: strings_of_args name tl
  | (_, {pexp_loc = loc}) :: _ ->
    let msg = name ^ " accepts list of strings: " ^ name ^ " \"string1\" \"string2\" ..." in
    loc_error ~loc msg

let cflags = [
  "i", `CASELESS, "caseless";
  "m", `MULTILINE, "multiline";
  "s", `DOTALL, "dotall";
  "x", `EXTENDED, "extended";
  "u", `UTF8, "utf-8";
]

let cflags_descr = List.map (fun (f, _, d) -> Printf.sprintf "%s = %s" f d) cflags |> String.concat ", "

let cflag_of_ident =
  let descr = Printf.sprintf "Pcre.cflag expected: %s" cflags_descr in
  function
    | { pexp_desc = Pexp_ident { txt = Lident flag }; pexp_loc = loc } -> (
        try
          let (_, f, _) = List.find (fun (f, _, _) -> f = flag) cflags in
          f
        with
        | _ -> loc_error ~loc descr
      )
  | { pexp_loc = loc } ->
    loc_error ~loc descr    

let cflags_of_list = function
  | (_, { pexp_desc = desc; pexp_loc = loc }) -> (
      match desc with
      | Pexp_construct (_, _) -> []
      | Pexp_apply (hd, tl) ->
        let tl = List.map (fun (_, v) -> v) tl in
        List.map cflag_of_ident (hd :: tl)
      | _ ->
        loc_error ~loc (Printf.sprintf "Empty list '()' or Pcre.cflags list expected: (i m ...). Available flags: %s" cflags_descr)
    )

let generator =
  let id = ref 0 in
  fun () ->
    incr id;
    !id

let my_mapper register_regexp =
  let expr_mapper mapper = function
    | {
      pexp_desc = Pexp_apply ({
          pexp_desc = Pexp_ident {txt = Lident fname}
        }, args);
      pexp_loc = loc;
    } when fname = "filemask" || fname = "bodymask"
      ->
      let flags, args =
        match args with
        | []
        | _ :: [] -> loc_error ~loc (Printf.sprintf "%s usage: %s (flags) \"regexp1\" \"regexp2\" ..." fname fname)
        | flags :: args ->
          let flags = cflags_of_list flags in
          flags, args
      in
      let args = strings_of_args fname args in
      let id = generator () in
      register_regexp ~id ~fname ~flags args;
      
      let id = Exp.constant ~loc (Const_int id) in
      let apply_name = Exp.ident ~loc (Location.mkloc (Lident ("match_" ^ fname)) loc) in
      Exp.apply ~loc apply_name ["", id]

    | x -> default_mapper.expr mapper x;
  in
  (*
  let structure_mapper mapper items =
    let items = List.map (fun v -> default_mapper.structure_item mapper v) items in

    let make_rex = function
      | RegisterRegexp { loc; fname; args; id } ->
        let fname = Exp.ident (Location.mkloc (Lident ("register_regexp_" ^ fname)) loc) in
        let args = List.map (fun v -> "", v) args in
        let args = ("id", id) :: args in
        let expr = Exp.apply fname args in
        Vb.mk (Pat.any ~loc ()) expr
    in
    let my_bindings =
      List.map make_rex !regexps
      |> Str.value Nonrecursive
    in

    my_bindings :: items
  in
*)
  { Ast_mapper.default_mapper with
    expr = expr_mapper;
    (*
    structure = structure_mapper;
*)
  }
