

let input_format = ref (module PipeHuman.Make : Pipe.PIPE_FORMAT)
let output_format = ref (module PipeHuman.Make : Pipe.PIPE_FORMAT)

let list = [
  "shellescape", (module PipeShell.Make : Pipe.PIPE_FORMAT), "Bash-compatible escapes; fields separated by <TAB> and records by <NEWLINE>";
  "nullchar", (module PipeNullChar.Make : Pipe.PIPE_FORMAT), "0x00, <BACKSLASH> and 0x01 are escaped. Fields separated by 0x01 and records by 0x00.";
  "human", (module PipeHuman.Make : Pipe.PIPE_FORMAT), "<TAB>, <BACKSLASH> and <NEWLINE> are escaped. Fields separated by <TAB> and records by <NEWLINE>";
  "findprint0", (module PipeFindPrint0.Make : Pipe.PIPE_FORMAT), "No escaped charactes at all. Records are separated by 0x00 and assumed to have only one field";
]

let set_format v f =
  try
    let (_, m, _) = List.find (fun (n, _, _) -> n = f) list in
    v := m;
  with
  | _ ->
    Printf.eprintf "Unknown pipe format: %s\n" f;
    exit 1

let usage =
  let msg =
    List.map (fun (n, _, d) -> Printf.sprintf "  '%s' - %s" n d) list
    |> String.concat "\n"
  in
  Printf.sprintf "PIPE FORMAT is one of:\n%s. Default format is 'nullchar'.\n" msg

let usageIn = Printf.sprintf "\nOption -if accepts one of <PIPE FORMAT> values\n\n%s" usage
let usageOut = Printf.sprintf "\nOption -of accepts one of <PIPE FORMAT> values\n\n%s" usage
let usageInOut = Printf.sprintf "\nOptions -if and -of accepts one of <PIPE FORMAT> values\n\n%s" usage

let argsIn = Arg.[
    "-if", String (set_format input_format), "Pipe format for STDIN";
  ]
    
let argsOut = Arg.[
    "-of", String (set_format output_format), "Pipe format for STDOUT";
]

let argsInOut = argsIn @ argsOut

