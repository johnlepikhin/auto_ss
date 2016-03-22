
let limit = ref 100000

let usage = ArgPipeFormat.usageInOut ^ "\nCOMMAND LINE ARGUMENTS:\n"

let args = ArgPipeFormat.argsInOut @ [
    "-l", Arg.Set_int limit, "Cache size (default 100000)";
  ]

type stat = {
  mtime : float;
  size : int64;
}

type r = {
  mutable st : stat option;
  mutable last : int64;
}

module Key :
sig
  type t

  val of_string: string -> t

  val compare: t -> t -> int
end =
struct
  type t = string

  let of_string v =
    let open Cryptokit in
    let hash = Hash.sha1 () in
    hash_string hash v

  let compare = compare
end

let cache_preclean = Int64.of_int (!limit / 5)

module Storage =
struct
  let hash : (Key.t, r) Hashtbl.t = Hashtbl.create (!limit)

  let get key =
    Hashtbl.find hash key
    
  let set key v =
    Hashtbl.replace hash key v

  let cleanup () =
    let max_size = !limit - !limit / 5 in
    let size = Hashtbl.length hash in
    if size > max_size then (
      let min_last = Hashtbl.fold (fun k { last } prev -> min prev last) hash Int64.max_int in
      let delete_cnt = size - max_size in
      let max_last = Int64.(add min_last (of_int delete_cnt)) in
      Hashtbl.iter (fun k { last } -> if last < max_last then Hashtbl.remove hash k) hash;
      Gc.full_major ()
    )
end

let get_inc =
  let i = ref 0L in
  (* Cleanup cache every limit/5 cache updates *)
  let cache_cleanup_epoch = Int64.of_int (!limit / 5) in
  fun () ->
    i := Int64.succ !i;
    if Int64.rem !i cache_cleanup_epoch = 0L then
      Storage.cleanup ();
    !i

let check_if_updated current key =
  let stored =
    try
      Some (Storage.get key)
    with
    | _ -> None
  in
  match current, stored with
  (* file doesn't exist *)
  | None, _ ->
    false

  (* cache has no info on this file *)
  | Some _, None
  | Some _, Some { st = None } ->
    true

  (* compare file info with cache record *)
  | Some current, Some { st = Some stored } ->
    let open Unix.LargeFile in
    stored.mtime <> current.st_mtime || stored.size <> current.st_size

let stat_of_unix_opt = function
  | None -> None
  | Some st ->
    let open Unix.LargeFile in
    Some { mtime = st.st_mtime; size = st.st_size }

let () =
  Arg.parse args (fun _ -> ()) usage;
  let module IN_FORMAT = (val !ArgPipeFormat.input_format) in
  let module OUT_FORMAT = (val !ArgPipeFormat.output_format) in
  let module P = PipeUnix.Make (PipeFmtMain.Type) (IN_FORMAT) (OUT_FORMAT) in
  let pipe = P.init stdin stdout in
  P.iter_input (function
      | Pipe.Record record -> (
          let open Unix.LargeFile in
          let file = record.PipeFmtMain.Type.file in
          let key = Key.of_string file in
          let current =
            try
              Some (stat file)
            with
            | _ -> None
          in
          let updated = check_if_updated current key in
          Storage.set key { st = stat_of_unix_opt current; last = get_inc () };
          if updated then
            P.output pipe (Pipe.Record record);
        )
      | Pipe.Meta meta ->
        P.output pipe (Pipe.Meta meta)
    ) pipe
