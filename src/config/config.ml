
let readers : (module Config_sig.CONFIGREADER) list = [
  (module ConfigFile);
]

let virtualRange =
  let open SexpLoc in
  let open Sexplib in
  let pos = Sexp.Annotated.{
      line = 0;
      col = 0;
      offset = 0;
    } in
  {
    domain = Root;
    start_pos = pos;
    end_pos = pos;
  }

let concat lst =
  let open SexpLoc in
  let open Sexplib in
  let seqContent = function
    | List (_, (Atom (_, Type.Atom "seq") :: content), _) -> content
    | other -> [other]
  in
  List.map seqContent lst
  |> List.concat
  |> fun content ->
  List (virtualRange, Atom (virtualRange, Type.Atom "seq") :: content, Type.Atom "")

let get =
  let splitRex = Pcre.regexp ":" in
  fun lst ->
    let open Lwt in
    let rec aux elt =
      let values = Pcre.split ~rex:splitRex elt in
      match values with
      | [] -> Lwt.fail (failwith "Config reader requires at least reader name (slfile, etc.)")
      | name :: args ->
        try
          let m = List.find (fun (module Reader : Config_sig.CONFIGREADER) -> Reader.identifier = name) readers in
          let module Reader = (val m : Config_sig.CONFIGREADER) in
          Reader.get args
          >>= Lwt_list.map_s (fun (domain, config) -> String.trim config |> SlParser.of_string domain |> return)
        with
        | Not_found ->
          Lwt.fail (failwith (Printf.sprintf "Unknown config type: '%s'" name))
    in
    Lwt_list.map_s aux lst
    >>= fun lst ->
    List.concat lst
    |> fun lst ->
    let open SexpLoc in
    let open Sexplib in
    let tree =
      concat lst
      |> SlMacro.replace
      |> SlMacro.replaceIconv
      |> SlParser.t_to_ast
      |> ASTOptimized.of_ast
    in
    return tree
