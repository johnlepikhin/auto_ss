
open SSScript.External

module Member : SSExternals.MEMBER =
struct
  let check context =
    notify context "test"
end

let () =
  SSExternals.register (module Member)
