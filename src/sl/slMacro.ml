
module M = Map.Make (struct type t = string let compare = compare end)
module ArgsM = Map.Make (struct type t = string let compare = compare end)

type macro = {
  body : SexpLoc.t;
  argsMap : int ArgsM.t;
  argsCount : int;
}

type context = macro M.t

let replaceIconv sl =
  let open SexpLoc in
  let open Sexplib in
  let rec aux = function
    | List (range, [
        Atom (_, Type.Atom "iconv");
        Atom (_, Type.Atom src);
        Atom (_, Type.Atom dst);
        body
      ], v2) -> (
        match aux body with
        | Atom (_, Type.Atom body) -> (
            try
              let newstring = Iconv.convert src dst body in
              Atom (range, Type.Atom newstring)
            with
            | e -> SlParser.error range (Printexc.to_string e)
          )
        | _ ->
          SlParser.error range "cannot expand third argument into string"
      )
    | List (v1, lst, v2) ->
      let lst = List.map aux lst in
      List (v1, lst, v2)
    | other -> other
  in
  aux sl

let expandMacroArgs body args argsMap =
  let open SexpLoc in
  let open Sexplib in
  let rec aux = function
    | (Atom (range, Type.Atom name)) as atom ->
      let newval =
        try
          ArgsM.find name argsMap
          |> List.nth args
        with
        | Not_found -> atom
      in
      newval
    | (Atom _) as atom -> atom
    | List (v1, lst, v2) ->
      let lst = List.map aux lst in
      List (v1, lst, v2)
  in
  aux body

let rec makeArgsMap map n = function
  | [] -> map
  | SexpLoc.Atom (range, Sexplib.Type.Atom argname) :: tl ->
    let map = ArgsM.add argname n map in
    makeArgsMap map (n+1) tl
  | SexpLoc.Atom (range, _) :: _
  | SexpLoc.List (range, _, _) :: _ ->
    SlParser.error range "arguments must be atoms (list of strings)"

let replace sl =
  let open SexpLoc in
  let open Sexplib in
  let rec aux context = function
    | [] -> []
    | (Atom (range, Type.Atom name)) as atom :: tl ->
      let newval =
        try
          let macro = M.find name context in
          if macro.argsCount > 0 then
            SlParser.error range (Printf.sprintf "macro %s expects arguments" name)
          else
            macro.body
        with
        | Not_found -> atom
      in
      newval :: aux context tl
    | (List (_, [
        Atom (_, Type.Atom "defmacro");
        Atom (_, Type.Atom name);
        List (_, args, _);
        body
      ], _)) :: tl ->
      let argsMap = makeArgsMap ArgsM.empty 0 args in
      let body =
        match aux context [body] with
        | [sl] -> sl
        | _ -> body
      in
      let macro = { body; argsMap; argsCount = List.length args } in
      let context = M.add name macro context in
      aux context tl

    | (List (range, ((Atom (_, Type.Atom name) :: args) as list), v2)) :: tl ->
      let newval =
        try
          let macro = M.find name context in
          if List.length args <> macro.argsCount then
            SlParser.error range (Printf.sprintf "macro %s expects %i arguments" name macro.argsCount)
          else
            expandMacroArgs macro.body args macro.argsMap
        with
        | Not_found ->
          let list = aux context list in
          List (range, list, v2)
      in
      newval :: aux context tl
    | List (v1, lst, v2) :: tl ->
      let lst = aux context lst in
      List (v1, lst, v2) :: aux context tl
    | (Atom _) as atom :: tl ->
      atom :: aux context tl
  in
  let sl = match aux M.empty [sl] with
    | [sl] -> sl
    | _ -> sl
  in
  replaceIconv sl

