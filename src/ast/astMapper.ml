
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

let map_regexp ~register_regexp ~loc ~fname ~flags ~args () =
  let id = generator () in
  register_regexp ~id ~fname ~flags args;

  let id = Exp.constant ~loc (Const_int id) in
  let apply_name = Exp.ident ~loc (Location.mkloc (Lident ("match_" ^ fname)) loc) in
  Exp.apply ~loc apply_name ["", id]

let split_regexp_args ?flags ~loc ~fname = function
  | []
  | _ :: [] ->
    loc_error
      ~loc
      (Printf.sprintf "%s usage: %s (flags) \"regexp1\" \"regexp2\" ..." fname fname)
  | flags :: args ->
    let flags = cflags_of_list flags in
    flags, args

let composition_of_list ~loc ~fn = function
  | [] ->
    loc_error ~loc "at least one expression expected"
  | [hd] -> hd
  | hd :: tl ->
    List.fold_left (fun r elt ->
        let fn = Exp.ident ~loc (Location.mkloc (Lident fn) loc) in
        Exp.apply ~loc fn [
          "", r;
          "", elt;
        ]
      ) hd tl

let my_mapper register_regexp =
  let expr_mapper mapper = function
    | {
      pexp_desc = Pexp_apply ({
          pexp_desc = Pexp_ident {txt = Lident fname}
        }, args);
      pexp_loc = loc;
    } as x -> (
        match fname with
        | "filemask"
        | "bodymask" ->
          let flags, args = split_regexp_args ~loc ~fname args in
          let args = strings_of_args fname args in
          map_regexp ~register_regexp ~loc ~fname ~flags ~args ()
        | "charsetbodymask" -> (
          let args = strings_of_args fname args in
          match args with
          | src :: dst :: args ->
            let args = List.map (Iconv.convert ~src ~dst) args in
            map_regexp ~register_regexp ~loc ~fname:"bodymask" ~flags:[] ~args ()
          | _ ->
            loc_error ~loc (Printf.sprintf "%s usage: %s \"src-charset\" \"dst-charset\" \"regexp1\" \"regexp2\" ..." fname fname)
          )
        | "rusbodymask" ->
          let args = strings_of_args fname args in
          let exprlist =
            List.map (fun dst ->
                try
                  let args = List.map (Iconv.convert ~src:"UTF-8" ~dst) args in
                  map_regexp ~register_regexp ~loc ~fname:"bodymask" ~flags:[] ~args ()
                with
                | Failure _ ->
                  loc_error ~loc (Printf.sprintf "Failed to iconv to charset %s" dst)
              )
              ["utf-8"; "cp1251"; "koi8-r"; "cp866"]
          in
          composition_of_list ~loc ~fn:"||" exprlist
        | _ ->
          default_mapper.expr mapper x
      )

    | x -> default_mapper.expr mapper x;
  in
  { Ast_mapper.default_mapper with
    expr = expr_mapper;
  }
