
module Sig =
struct
  type file = {
    file : string;
    tail : string list;
  }

  type pipe =
    | File of file
    | Meta of string
end

module type PIPE_FORMAT =
sig
  val of_string: string -> Sig.pipe
  val to_string: Sig.pipe -> string

  val record_separator: char
end

module type IO =
sig
  type 'a t
  type input_channel
  type output_channel

  val return : 'a -> 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  
  val read_record_opt : separator : char -> input_channel -> string option t
  val write_record : separator : char -> output_channel -> string -> unit t

  val return: 'a -> 'a t
end

module Make (IO : IO) (IN : PIPE_FORMAT) (OUT : PIPE_FORMAT) =
struct
  include IO
  include Sig

  let iter_input fn input_channel =
    let rec loop () =
      read_record_opt ~separator:IN.record_separator input_channel
      >>= function
      | None -> return ()
      | Some s ->
        fn (IN.of_string s)
        >>= fun () -> loop ()
    in
    loop ()

  let output output_channel r =
    IO.write_record ~separator:OUT.record_separator output_channel @@ OUT.to_string r
end
