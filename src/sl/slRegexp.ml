

module M = Map.Make (struct type t = int let compare = compare end)

type regexp = {
  rex : string;
  id : int;
}

type smap = {
  last_id : int;
  map : regexp M.t;
  superex : Pcre.regexp;
}

type result = Superex.GroupsSet.t

(* Empty superex set *)
let initial = {
  last_id = -1;
  map = M.empty;
  superex = Pcre.regexp "";
}

(* Add new regexp to set *)
let register ~rex smap =
  let last_id = smap.last_id + 1 in
  let regexp = {
    rex;
    id = last_id;
  } in
  let smap = {
    smap with
    last_id;
    map = M.add last_id regexp smap.map;
  } in
  smap, regexp

(* Compile superex, make set appliable *)
let compile smap =
  let superex =
    M.bindings smap.map
    |> List.map (fun (key, regexp) -> regexp.rex)
    |> Superex.superex
  in
  { smap with superex }

(* Apply regexps set to string *)
let apply smap s =
  Superex.matched_regexps ~superex:smap.superex s

(* Check if regexp (it's ID) matches *)
let matches regexp groups_set =
  Superex.GroupsSet.mem regexp.id groups_set
