type speed = int
type stationName = string
type distance = int
type train_type = TGV | Eurostar | Thalys | Invalid
type train = Tcnst of (train_type * speed * stationName list)

module type TRAIN =
  sig
    val newTrain : train_type -> train
    val toString : train -> string
    val trainTypeToString : train_type -> string

    val trainTypeFromString : string -> train_type
    val getSpeed : train_type -> int

    val getTravelDuration : train_type -> distance -> int

  end

module Train : TRAIN
