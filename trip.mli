open Train
open Link
open Date

module type TRIP =
  sig
    type trip

    val newTrip       : (string * Date.date * string list) -> trip list -> trip
    val newTripId     : trip list -> train_type -> int

    val getId         : trip -> int
    val getTrainType  : trip -> train_type

    val printTrip     : trip -> unit
  end

(* Functor sig *)
module type MAKETRIP = functor (Tr : TRAIN) (Lk : LINK) -> TRIP

module MakeTrip : MAKETRIP
module Trip : TRIP
