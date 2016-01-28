

module M = Map.Make (struct type t = int let compare = compare end)
module RevM = Map.Make (struct type t = string * Pcre.cflag list let compare = compare end)

type regexp = {
  rex : string;
  flags : Pcre.cflag list;
  id : int;
}

type smap = {
  last_id : int;
  map : regexp M.t;
  revmap : int RevM.t;
  superex : Pcre.regexp;
}

type result = Superex.GroupsSet.t

(* Empty superex set *)
let initial = {
  last_id = -1;
  map = M.empty;
  revmap = RevM.empty;
  superex = Pcre.regexp "";
}

(* Add new regexp to set *)
let register ~rex smap =
  try
    let id = RevM.find rex smap.revmap in
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
    let smap = {
      smap with
      last_id;
      map = M.add last_id regexp smap.map;
      revmap = RevM.add (rex, flags) last_id smap.revmap;
    } in
    smap, regexp

(* Compile superex, make set applicable *)
let compile smap =
  let superex =
    M.bindings smap.map
    |> List.map (fun (key, regexp) -> (regexp.flags, regexp.rex))
    |> Superex.superex
  in
  { smap with superex }

(* Apply regexps set to string *)
let apply smap s =
  Superex.matched_regexps ~superex:smap.superex s

(* Check if regexp (it's ID) matches *)
let matches regexp groups_set =
  Superex.GroupsSet.mem regexp.id groups_set
