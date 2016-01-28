
module M : Config_sig.CONFIGREADER =
struct
  let identifier = "sldir"

  let readdir dir =
    let open Unix in
    let dh = opendir dir in
    let rec read () =
      try
        let name = readdir dh in
        if String.length name > 0 && name.[0] <> '.' then
          let path = Filename.concat dir name in
          let st = stat path in
          if st.st_kind = S_REG then
            path :: read ()
          else
            read ()
        else
          read ()
      with
      | _ -> []
    in
    read ()
  
  let get params =
    List.map readdir params
    |> List.concat
    |> ConfigFile._get identifier
end

include M
