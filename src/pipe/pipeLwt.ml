
module IO =
struct
  include Lwt
  include Lwt_io

  let read_record_opt ~separator ic =
    let b = Buffer.create 1000 in
    let rec aux () =
      read_char ic
      >>= fun c ->
      if c = separator then
        return (Some (Buffer.contents b))
      else (
        Buffer.add_char b c;
        aux ()
      )
    in
    catch aux (fun _ -> return None)

  let write_record ~separator oc l =
    write oc l >>= fun () -> write_char oc separator

end

module Make = Pipe.Make (IO)
