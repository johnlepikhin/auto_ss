
module IO =
struct
  include Lwt
  include Lwt_io
end

module Make = Pipe.Make (IO)
