
module type PARSER =
sig
  val parse: string -> (string * string) list -> PipeFmtMain.Type.record option
end

module Make (IO : Pipe.IO) (IN : Pipe.PIPE_FORMAT) (OUT : Pipe.PIPE_FORMAT) (Parser : PARSER) =
struct
  module PIN = PipeUnix.Make (PipeFmtLog.Type) (IN) (IN)
  module POUT = PipeUnix.Make (PipeFmtMain.Type) (OUT) (OUT)

  let iter r =
    let open PipeFmtLog.Type in
    try
      let ch = open_in r.file in
      try
        seek_in ch (Int64.to_int r.begin_pos);
        let end_pos = Int64.to_int r.end_pos in
        while pos_in ch < end_pos do
          let line = input_line ch in
          let record = Parser.parse line r.values in
          match record with
          | Some record ->
            POUT.output stdout (Pipe.Record record)
          | None ->
            ()
        done;
        close_in ch
      with
      | _ ->
        close_in ch
    with
    | _ -> ()

  let process () =
    PIN.iter_input
      (fun pipe ->
         match pipe with
         | Pipe.Record r ->
           iter r
         | Pipe.Meta m ->
           let o = Pipe.Meta m in
           POUT.output stdout o
      ) stdin
end
