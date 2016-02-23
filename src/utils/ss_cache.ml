
let limit = ref 100000

let usage = ArgPipeFormat.usageInOut ^ "\nCOMMAND LINE ARGUMENTS:\n"

let args = ArgPipeFormat.argsInOut @ [
    "-l", Arg.Set_int limit, "Cache size (default 100000)";
  ]

type r = {
  mutable st : Unix.LargeFile.stats;
  mutable last : int64;
}

let () =
  Arg.parse args (fun _ -> ()) usage

let hash = Hashtbl.create (!limit*2)

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
  fun () ->
    i := Int64.succ !i;
    if Int64.rem !i 1000L = 0L then
      cleanup ();
    !i

let () =
  Arg.parse args (fun _ -> ()) usage;
  let module IN_FORMAT = (val !ArgPipeFormat.output_format) in
  let module OUT_FORMAT = (val !ArgPipeFormat.output_format) in
  let module P = PipeUnix.Make (PipeFmtMain.Type) (IN_FORMAT) (OUT_FORMAT) in
  P.iter_input (function
      | Pipe.Record record -> (
          let file = record.PipeFmtMain.Type.file in
          try
            let open Unix.LargeFile in
            let st = stat file in
            try
              let r = Hashtbl.find hash st.st_ino in
              if r.st.st_mtime <> st.st_mtime || r.st.st_size <> st.st_size then
                P.output stdout (Pipe.Record record);
              r.last <- get_inc ();
              r.st <- st;
            with
            | _ ->
              Hashtbl.add hash st.st_ino {
                st; last = get_inc ()
              };
              P.output stdout (Pipe.Record record);
          with
          | _ ->
            P.output stdout (Pipe.Record record)
        )
      | Pipe.Meta meta ->
        P.output stdout (Pipe.Meta meta)
    ) stdin
