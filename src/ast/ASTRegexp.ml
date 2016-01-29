

module M = Map.Make (struct type t = int let compare = compare end)
module RevM = Map.Make (struct type t = string * Pcre.cflag list let compare = compare end)

type regexp = {
  rex : string;
  flags : Pcre.cflag list;
  id : int;
}

type superex = {
  map : regexp M.t;
  revmap : int RevM.t;
  superex : Pcre.regexp;
  length : int;
}

type smap = {
  last_id : int;
  set : superex list;
}

type result = Superex.GroupsSet.t list

let rex_limit = 30000

let initial_superex = {
  map = M.empty;
  revmap = RevM.empty;
  superex = Pcre.regexp "";
  length = 0;
}

(* Empty superex set *)
let initial = {
  last_id = -1;
  set = [];
}

let rec find_id rex = function
  | [] -> raise Not_found
  | superex :: tl ->
    try
      RevM.find rex superex.revmap
    with
    | Not_found ->
      find_id rex tl

let add_to_superex superex regexp flags =
  {
    superex with
    map = M.add regexp.id regexp superex.map;
    revmap = RevM.add (regexp.rex, flags) regexp.id superex.revmap;
    length = superex.length + String.length regexp.rex;
  }

(* Add new regexp to set *)
let register ~rex smap =
  try
    let id = find_id rex smap.set in
    let (rex, flags) = rex in
    smap, { rex; flags; id }
  with
  | _ ->
    let last_id = smap.last_id + 1 in
    let (rex, flags) = rex in
    let regexp = {
      rex;
      flags;
      id = last_id;
    } in
    let set =
      match smap.set with
      | superex :: tl when superex.length + String.length rex < rex_limit ->
        add_to_superex superex regexp flags :: tl
      | superex :: tl ->
        add_to_superex initial_superex regexp flags :: superex :: tl
      | [] ->
        [add_to_superex initial_superex regexp flags]
    in
    { set; last_id }, regexp

(* Compile superex, make set applicable *)
let compile (smap : smap) =
  let set =
    List.map (fun s ->
        let superex =
          M.bindings s.map
          |> List.map (fun (key, regexp) -> (regexp.flags, regexp.rex))
          |> Superex.superex
        in
        { s with superex }
      ) smap.set
  in
  {
    smap with set
  }
      
(* Apply regexps set to string *)
let apply smap s : result =
  List.map (fun superex -> Superex.matched_regexps ~superex:superex.superex s) smap.set

(* Check if regexp (it's ID) matches *)
let matches regexp (result : result) =
  let rec aux = function
    | [] -> false
    | set :: tl ->
      if Superex.GroupsSet.mem regexp.id set then
        true
      else
        aux tl
  in
  aux result
