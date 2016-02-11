
type domain =
  | File of string
  | Argument

let string_of_domain = function
  | File s -> Printf.sprintf "file %s" s
  | Argument -> "command line argument"

module type CONFIGREADER =
sig
  val identifier: string

  val get: string list -> (domain * string) list Lwt.t
end

module M = Map.Make (struct type t = string let compare = compare end)
