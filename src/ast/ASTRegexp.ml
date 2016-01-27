

module M = Map.Make (struct type t = int let compare = compare end)
module RevM = Map.Make (struct type t = string let compare = compare end)

type regexp = {
  rex : string;
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
    smap, { rex; id }
  with
  | _ ->
    let last_id = smap.last_id + 1 in
    let regexp = {
      rex;
      id = last_id;
    } in
    let smap = {
      smap with
      last_id;
      map = M.add last_id regexp smap.map;
      revmap = RevM.add rex last_id smap.revmap;
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
