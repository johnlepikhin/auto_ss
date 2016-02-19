type t =
  | Windows
  | UNIX

let v = match Sys.os_type with
  | "Unix" -> UNIX
  | "Win32"
  | "Cygwin" -> Windows
  | _ ->
    Printf.eprintf "Unknown OS type!\n";
    exit 1
