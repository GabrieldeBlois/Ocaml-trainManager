type city = string

module type STATION =
  sig
    type station

    val newStation      : string -> station
    val newStationList  : string -> station list

    val toString    : station -> string

    val isValidCity : city -> bool
  end

module Station : STATION
