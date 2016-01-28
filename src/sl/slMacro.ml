
module M = Map.Make (struct type t = string let compare = compare end)

type context = SexpLoc.t M.t

let replace sl =
  let open SexpLoc in
  let open Sexplib in
  let rec aux context = function
    | [] -> []
    | (Atom (_, Type.Atom name)) as atom :: tl ->
      let newval =
        try
          M.find name context
        with
        | _ -> atom
      in
      newval :: aux context tl
    | (List (_, [
        Atom (_, Type.Atom "defmacro");
        Atom (_, Type.Atom name);
        List (_, args, _);
        body
      ], _)) :: tl ->
      let body =
        let body = aux context [body] in
        match body with
        | [sl] -> sl
        | _ -> sl
      in
      let context = M.add name body context in
      aux context tl
    | List (v1, lst, v2) :: tl ->
      let lst = aux context lst in
      List (v1, lst, v2) :: aux context tl
    | (Atom _) as atom :: tl ->
      atom :: aux context tl
  in
  match aux M.empty [sl] with
  | [sl] -> sl
  | _ -> sl
