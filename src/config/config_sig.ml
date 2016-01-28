
module type CONFIGREADER =
sig
  val identifier: string

  val get: string list -> (SexpLoc.domain * string) list Lwt.t
end

module M = Map.Make (struct type t = string let compare = compare end)
