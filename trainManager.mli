open Date
open Trip

module type TRAIN_MANAGER =
  sig
    type infos = (string * Date.date * string list * bool)
    val bad_infos : unit -> infos

    val parseCreateArgs : string list -> (string * Date.date * string list * bool)

    val createTrip : Trip.trip list -> string list -> Trip.trip list
    val deleteTrip : Trip.trip list -> string list -> Trip.trip list
    val printTrip  : Trip.trip list -> unit

  end

module TrainManager : TRAIN_MANAGER
