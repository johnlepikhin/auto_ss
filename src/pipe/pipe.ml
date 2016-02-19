
type ('a, 'b) pipe =
  | Record of 'a
  | Meta of 'b

(*
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
*)

module type TYPE =
sig
  type record
  type meta

  val record_of_fields: string list -> record
  val meta_of_line: string -> meta
  val fields_of_record: record -> string list
  val line_of_meta: meta -> string
end

module type PIPE_FORMAT =
  functor (T : TYPE) ->
  sig
    val of_string: string -> (T.record, T.meta) pipe
    val to_string: (T.record, T.meta) pipe -> string

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

module Make (IO : IO) (T : TYPE) (IN : PIPE_FORMAT) (OUT : PIPE_FORMAT) =
struct
  include IO
  type record = T.record
  type meta = T.meta

  module INFmt = IN (T)
  module OUTFmt = OUT (T)

  let iter_input fn input_channel =
    let rec loop () =
      read_record_opt ~separator:INFmt.record_separator input_channel
      >>= function
      | None -> return ()
      | Some s ->
        fn (INFmt.of_string s)
        >>= fun () -> loop ()
    in
    loop ()

  let output output_channel r =
    IO.write_record ~separator:OUTFmt.record_separator output_channel @@ OUTFmt.to_string r
end
