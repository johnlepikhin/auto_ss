

module M = Map.Make (struct type t = string * Pcre.cflag list let compare = compare end)
module ResM = Map.Make (struct type t = int let compare = compare end)

type regexp = {
  rex : string;
  flags : Pcre.cflag list;
  regexp : Pcre.regexp;
  id : int;
}

type smap = {
  last_id : int;
  map : regexp M.t;
}

type result = bool lazy_t ResM.t

let initial = {
  last_id = 0;
  map = M.empty;
}

let register ~rex smap =
  try
    smap, M.find rex smap.map
  with
  | Not_found ->
    let (rex_string, flags) = rex in
    let last_id = smap.last_id + 1 in
    let regexp = {
      rex = rex_string;
      flags;
      regexp = Pcre.regexp ~flags rex_string;
      id = last_id;
    } in
    let smap = {
      last_id;
      map = M.add rex regexp smap.map;
    } in
    smap, regexp

let compile smap =
  smap

let apply smap s : result =
  let rec aux result = function
    | [] -> result
    | regexp :: tl ->
      let getter () =
        Pcre.pmatch ~rex:regexp.regexp s
      in
      let result = ResM.add regexp.id (Lazy.from_fun getter) result in
      aux result tl
  in
  M.bindings smap.map
  |> List.map (fun (_, rex) -> rex)
  |> aux ResM.empty

(* Check if regexp (it's ID) matches *)
let matches regexp (result : result) =
  let result = ResM.find regexp.id result in
  Lazy.force result
