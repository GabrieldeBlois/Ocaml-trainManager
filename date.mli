module type DATE =
  sig
      type date
        val toString : date -> string

        val checkDate : string -> bool
        val checkTime : string -> bool

        val simple_add : date -> date -> date
        val simple_sub : date -> date -> date

        val normal_add : date -> date -> date
        val normal_sub : date -> date -> date

        val newDateFromString : string -> date
        val newTimeFromString : string -> date
        val createDateFromSec : int -> date

        val createDateFromDate :
            int -> (* second *)
            int -> (* minute *)
            int -> (* hour *)
            int -> (* day *)
            int -> (* month *)
            int -> (* year *)
            date

        val createDateFromString : string -> string -> date

        val isValid : date -> bool

        val addDay : date -> int -> date
        val addHour : date -> int -> date
        val addMinute : date -> int -> date

        val checkOverlap : (date * date) -> (date * date) -> bool
  end

  module Date : DATE
