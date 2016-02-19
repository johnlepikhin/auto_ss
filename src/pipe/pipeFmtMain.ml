
module Type =
struct
  type record = {
    file : string;
    alert : string;
    remote_ip : string;
    username : string;
    tail : string list;
  }

  type meta = string

  let record_of_fields lst =
    let rec aux ~file ~alert ~remote_ip ~username ~tail = function
      | v :: tl ->
        if file = None then aux ~file:(Some v) ~alert ~remote_ip ~username ~tail tl
        else if alert = None then aux ~file ~alert:(Some v) ~remote_ip ~username ~tail tl
        else if remote_ip = None then aux ~file ~alert ~remote_ip:(Some v) ~username ~tail tl
        else if username = None then aux ~file ~alert ~remote_ip ~username:(Some v) ~tail tl
        else aux ~file ~alert ~remote_ip ~username ~tail:(v :: tail) tl
      | [] ->
        let opt v =
          match v with
          | None -> ""
          | Some v -> v
        in
        {
          file = opt file;
          alert = opt alert;
          remote_ip = opt remote_ip;
          username = opt username;
          tail = tail
        }
    in
    aux ~file:None ~alert:None ~remote_ip:None ~username:None ~tail:[] lst

  let meta_of_line s = s

  let fields_of_record r =
    r.file :: r.alert :: r.remote_ip :: r.tail

  let line_of_meta r =
    r

  let file_is_empty f =
    f.alert = "" && not (List.exists (( <> ) "") f.tail)
end

