
open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

let exp_construct loc n v = Exp.construct ~loc (Location.mkloc (Lident n) loc) v

let mklist loc lst =
  let rec aux = function
    | [] -> exit 1
    | hd :: [] ->
      exp_construct loc "::" (Some (Exp.tuple ~loc [hd; exp_construct loc "[]" None]))
    | hd :: tl ->
      exp_construct loc "::" (Some (Exp.tuple ~loc [hd; aux tl]))
  in
  match lst with
  | [] -> exp_construct loc "[]" None
  | lst -> aux lst

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
  | (_, ({ pexp_desc = desc; pexp_loc = loc } as expr)) -> (
      match desc with
      | Pexp_construct (_, _) -> []
      | Pexp_ident _ ->
        [cflag_of_ident expr]
      | Pexp_apply (hd, tl) ->
        let tl = List.map (fun (_, v) -> v) tl in
        List.map cflag_of_ident (hd :: tl)
      | _ ->
        loc_error ~loc (Printf.sprintf "Empty list '()' or Pcre.cflags list expected: (i m ...). Available flags: %s" cflags_descr)
    )

let expr_of_cflag ~loc = function
  | `CASELESS -> Exp.variant ~loc "CASELESS" None
  | `MULTILINE -> Exp.variant ~loc "MULTILINE" None
  | `DOTALL -> Exp.variant ~loc "DOTALL" None
  | `EXTENDED -> Exp.variant ~loc "EXTENDED" None
  | `UTF8 -> Exp.variant ~loc "UTF8" None
  | _ ->
    loc_error ~loc "Unsupported Pcre cflag"

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

let generator =
  let id = ref 0 in
  fun () ->
    incr id;
    !id

type descr = {
  regexps : string list;
  loc : Location.t;
  flags : Pcre.cflag list;
  name : string;
}

let my_mapper =
  let reg_regexps = ref [] in
  let register_regexp ~fname ~flags ~loc regexps =
    let id = generator () in
    let name = Printf.sprintf "__regexp_%s_%i" fname id in
    let regexp = {
      regexps;
      loc;
      flags;
      name;
    } in
    reg_regexps := regexp :: !reg_regexps;
    id, name
  in
  let map_regexp ~loc ~fname ~flags ~args () =
    let id, name = register_regexp ~fname ~flags ~loc args in
    let name = Exp.ident ~loc (Location.mkloc (Lident name) loc) in
    let context = Exp.ident ~loc (Location.mkloc (Lident "context") loc) in
    let fn_name = Longident.parse ("SSScript.External.match_" ^ fname) in
    let apply_name = Exp.ident ~loc (Location.mkloc fn_name loc) in
    id, Exp.apply ~loc apply_name ["", name; "", context]
  in
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
          let (_, expr) = map_regexp ~loc ~fname ~flags ~args () in
          expr
        | "charsetbodymask" -> (
          let args = strings_of_args fname args in
          match args with
          | src :: dst :: args ->
            let args = List.map (Iconv.convert ~src ~dst) args in
            let (_, expr) = map_regexp ~loc ~fname:"bodymask" ~flags:[] ~args () in
            expr
          | _ ->
            loc_error ~loc (Printf.sprintf "%s usage: %s \"src-charset\" \"dst-charset\" \"regexp1\" \"regexp2\" ..." fname fname)
          )
        | "rusbodymask" ->
          let args = strings_of_args fname args in
          let exprlist =
            let ids = ref [] in
            List.fold_left (fun rlst dst ->
                try
                  let args = List.map (Iconv.convert ~src:"UTF-8" ~dst) args in
                  let id, expr =
                    map_regexp ~loc ~fname:"bodymask" ~flags:[] ~args ()
                  in
                  if List.mem id !ids then
                    rlst
                  else (
                    ids := id :: !ids;
                    expr :: rlst
                  )
                with
                | Failure _ ->
                  loc_error ~loc (Printf.sprintf "Failed to iconv to charset %s" dst)
              )
              []
              ["utf-8"; "cp1251"; "koi8-r"; "cp866"]
          in
          composition_of_list ~loc ~fn:"||" exprlist
        | _ ->
          default_mapper.expr mapper x
      )
    | x -> default_mapper.expr mapper x
  in
  let structure_mapper mapper = function
    | x ->
      let tail = default_mapper.structure mapper x in
      let regexp_mapper r =
        let loc = r.loc in
        let fn = Exp.ident ~loc (Location.mkloc (Ldot ((Lident "Pcre"), "regexp_or")) loc) in
        let subrex_mapper sr =
          Ast_helper.Exp.constant ~loc (Const_string (sr, None))
        in
        let sub_regexps = List.map subrex_mapper r.regexps in
        let regexps = mklist loc sub_regexps in
        let flags =
          List.map (expr_of_cflag ~loc) r.flags
          |> mklist loc
        in
        let binding =
          Exp.apply ~loc fn ["flags", flags; "", regexps]
          |> Vb.mk ~loc (Pat.var (Location.mkloc r.name loc))
        in
        Str.value ~loc Nonrecursive [binding]
      in
      let vals = List.rev !reg_regexps |> List.map regexp_mapper in
      vals @ tail
  in
  { Ast_mapper.default_mapper with
    expr = expr_mapper;
    structure = structure_mapper;
  }
