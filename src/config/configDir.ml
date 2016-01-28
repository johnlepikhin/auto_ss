
module M : Config_sig.CONFIGREADER =
struct
  let identifier = "sldir"

  let readdir dir =
    let open Unix in
    let dh = opendir dir in
    let rec read () =
      try
        let name = readdir dh in
        if name = "." || name = ".." then
          read ()
        else
          let path = Filename.concat dir name in
          let st = stat path in
          if st.st_kind = S_REG then
            path :: read ()
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
