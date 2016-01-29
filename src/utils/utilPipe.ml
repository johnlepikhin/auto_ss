
type file = {
  file : string;
  alert : string;
  tail : string list;
}

type t =
  | File of file
  | Meta of string

exception InvalidInput

let of_pipe = function
  | Pipe.Sig.File { Pipe.Sig.file; Pipe.Sig.tail = alert :: tail } ->
    File { file; alert; tail }
  | Pipe.Sig.File { Pipe.Sig.file; Pipe.Sig.tail } ->
    File { file; alert = ""; tail }
  | Pipe.Sig.Meta s ->
    Meta s

let to_pipe = function
  | File { file; alert; tail } ->
    Pipe.Sig.(File { file; tail = alert :: tail })
  | Meta s ->
    Pipe.Sig.Meta s
