
module M : SSConfig_sig.CONFIGREADER =
struct
  let identifier = "arg"

  let get params =
    let folder (is_value, rlst) elt =
      if elt = "--script" then
        (true, rlst)
      else
      if is_value then
        (false, (SSConfig_sig.Argument, elt) :: rlst)
      else
        (false, rlst)
    in
    Array.fold_left folder (false, []) Sys.argv
    |> fun (_, r) -> Lwt.return r
end

include M
