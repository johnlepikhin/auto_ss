
module IO =
struct
  type 'a t = 'a
  type input_channel = in_channel
  type output_channel = out_channel
  
  let read_record_opt ~separator ic =
    let b = Buffer.create 1000 in
    let rec aux () =
      let c = input_char ic in
      if c = separator then
        Some (Buffer.contents b)
      else (
        Buffer.add_char b c;
        aux ()
      )
    in
    try
      aux ()
    with
    | _ -> None

  let write_record ~separator oc l =
    output_string oc l;
    output_char oc separator;
    flush oc

  let ( >>= ) r fn = fn r

  let return v = v

  let flush oc =
    flush oc
end

module Make = Pipe.Make (IO)
