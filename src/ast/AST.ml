module type Regexp =
sig
  type t
end

module type ContextInfo =
sig
  type t
end

module Make (R : Regexp) (C : ContextInfo) =
struct
  type t_context = C.t
  
  type t_bool =
    | And of t_bool list
    | Or of t_bool list
    | Not of t_bool
    | True
    | False
    | Filemask of R.t list
    | Bodymask of R.t list

  and t =
    | Notify of string
    | If of t_bool * t
    | SetContext of C.t * t
    | Seq of t list
end

module S =
  Make
    (struct type t = string end) (* source regexp *)
    (struct type t = string end) (* filename *)

include S
