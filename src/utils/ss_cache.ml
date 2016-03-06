
let limit = ref 100000

let usage = ArgPipeFormat.usageInOut ^ "\nCOMMAND LINE ARGUMENTS:\n"

let args = ArgPipeFormat.argsInOut @ [
    "-l", Arg.Set_int limit, "Cache size (default 100000)";
  ]

type r = {
  mutable st : Unix.LargeFile.stats option;
  mutable last : int64;
}

let () =
  Arg.parse args (fun _ -> ()) usage

let hash : (string, r) Hashtbl.t = Hashtbl.create (!limit*2)

let cleanup () =
  if Hashtbl.length hash > !limit then (
    let keys = ref [] in
    let cnt = ref 0 in
    Hashtbl.iter (fun k v -> incr cnt; keys := (v.last, k) :: !keys) hash;
    let keys = List.sort (fun (last1, _) (last2, _) -> compare last1 last2) !keys in
    let rec skip n = function
      | [] -> ()
      | (_, k) :: tl ->
        if n = 0 then ()
        else (
          Hashtbl.remove hash k;
          skip (n-1) tl
        )
    in
    skip (!cnt - !limit) keys
  )

let get_inc =
  let i = ref 0L in
  (* Cleanup cache every limit/5 cache updates *)
  let cache_overhead = Int64.of_int (!limit / 5) in
  fun () ->
    i := Int64.succ !i;
    if Int64.rem !i cache_overhead = 0L then
      cleanup ();
    !i

let check_if_updated current file =
  let stored =
    try
      Some (Hashtbl.find hash file)
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
    stored.st_mtime <> current.st_mtime || stored.st_size <> current.st_size

let () =
  Arg.parse args (fun _ -> ()) usage;
  let module IN_FORMAT = (val !ArgPipeFormat.input_format) in
  let module OUT_FORMAT = (val !ArgPipeFormat.output_format) in
  let module P = PipeUnix.Make (PipeFmtMain.Type) (IN_FORMAT) (OUT_FORMAT) in
  let pipe = P.init stdin stdout in
  P.iter_input (function
      | Pipe.Record record -> (
          let file = record.PipeFmtMain.Type.file in
          let current =
            try
              Some (Unix.LargeFile.stat file)
            with
            | _ -> None
          in
          let updated = check_if_updated current file in
          Hashtbl.replace hash file { st = current; last = get_inc () };
          if updated then
            P.output pipe (Pipe.Record record);
        )
      | Pipe.Meta meta ->
        P.output pipe (Pipe.Meta meta)
    ) pipe
