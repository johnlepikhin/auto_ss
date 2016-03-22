
module M : SSConfig_sig.CONFIGREADER =
struct
  let identifier = "arg"

  let get params =
    let folder (is_value, rlst) elt =
      if elt = "-I" then
        (true, rlst)
      else
      if is_value then
        let rule = Printf.sprintf "
SSExternals.register (module (struct
  let check context =
    if (%s) then SSScript.External.notify context \"Inline rule matches\"
end))
" elt
        in
        (false, (SSConfig_sig.Argument, rule) :: rlst)
      else
        (false, rlst)
    in
    Array.fold_left folder (false, []) Sys.argv
    |> fun (_, r) -> Lwt.return r
end

include M
