
module M : Config_sig.CONFIGREADER =
struct
  let identifier = "slarg"

  let get params =
    let folder (is_value, rlst) elt =
      if elt = "--slarg" then
        (true, rlst)
      else
      if is_value then
        (false, (SexpLoc.Argument, elt) :: rlst)
      else
        (false, rlst)
    in
    Array.fold_left folder (false, []) Sys.argv
    |> fun (_, r) -> Lwt.return r
end

include M
