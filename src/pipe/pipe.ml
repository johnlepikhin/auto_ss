
module Sig =
struct
  type file = {
    file : string;
    tail : string list;
  }

  type t =
    | File of file
    | Meta of string
end

module type PIPE =
sig
  val of_string: string -> Sig.t
  val to_string: Sig.t -> string
end

module type IO =
sig
  type 'a t
  type input_channel
  type output_channel

  val return : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  
  val read_line_opt : input_channel -> string option t
  val write_line : output_channel -> string -> unit t
end

module Make (IO : IO) (P : PIPE) =
struct
  open IO
  include Sig
  
  let iter_input fn input_channel =
    let rec loop () =
      read_line_opt input_channel
      >>= function
      | None -> return ()
      | Some s ->
        fn (P.of_string s)
        >>= fun () -> loop ()
    in
    loop ()

  let output output_channel r =
    IO.write_line output_channel @@ P.to_string r
end
