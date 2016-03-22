
module type MEMBER =
sig
  val check : SSScript.context_stack -> unit
end

type member = (module MEMBER)

let members : member list ref = ref []

let register member =
  members := !members @ [member]

let run filename =
  let context = SSScript.context filename in
  List.iter
    (fun m ->
       let module Member = (val m : MEMBER) in
       SSScript.External.set_context context;
       Member.check context
    )
    !members
