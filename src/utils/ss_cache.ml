
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

let () =
  Arg.parse args (fun _ -> ()) usage;
  let module IN_FORMAT = (val !ArgPipeFormat.input_format) in
  let module OUT_FORMAT = (val !ArgPipeFormat.output_format) in
  let module P = PipeUnix.Make (PipeFmtMain.Type) (IN_FORMAT) (OUT_FORMAT) in
  let pipe = P.init stdin stdout in
  P.iter_input (function
      | Pipe.Record record -> (
          let file = record.PipeFmtMain.Type.file in
          let stored =
            try
              Some (Hashtbl.find hash file)
            with
            | _ -> None
          in
          let current =
            try
              Some (Unix.LargeFile.stat file)
            with
            | _ -> None
          in
          let pass_required =
            match stored, current with
            (* new cache entry *)
            | None, Some _ ->
              true

            (* new cache entry for nonexistent file *)
            | None, None ->
              false

            (* file marked in cache as nonexistent while it actually exists *)
            | Some { st = None }, Some _ ->
              true

            (* file marked in cache as nonexistent and it actually doesn't exist *)
            | Some { st = None }, None
            (* cached file entry was deleted *)
            | Some { st = Some _ }, None ->
              false

            (* check if file changed *)
            | Some { st = Some stored }, Some current ->
              let open Unix.LargeFile in
              stored.st_mtime <> current.st_mtime || stored.st_size <> current.st_size
          in
          Hashtbl.replace hash file { st = current; last = get_inc () };
          if pass_required then
            P.output pipe (Pipe.Record record);
        )
      | Pipe.Meta meta ->
        P.output pipe (Pipe.Meta meta)
    ) pipe
