open Unix
open Printf

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

module Date : DATE =
struct
    type date = float

    let checkDay = function
      | s when Str.string_match (Str.regexp "[0-2][0-9]-") s 0 -> true
      | s when Str.string_match (Str.regexp "[3][01]-") s 0 -> true
      | _ -> false
    ;;

    let checkMonth = function
      | s when Str.string_match (Str.regexp "[0][0-9]-") s 3 -> true
      | s when Str.string_match (Str.regexp "[1][012]-") s 3 -> true
      | _ -> false
    ;;

    let checkYear = function
      | s when Str.string_match (Str.regexp "[0-9][0-9][0-9][0-9]$") s 6 -> true
      | _ -> false
    ;;

    let checkHour = function
      | s when Str.string_match (Str.regexp "[0-1][0-9]:") s 0 -> true
      | s when Str.string_match (Str.regexp "[2][0-3]:") s 0 -> true
      | _ -> false
    ;;

    let checkMinute = function
      | s when Str.string_match (Str.regexp "[0-5][0-9]$") s 3 -> true
      | _ -> false
    ;;

    let checkTime str = (checkHour str) && (checkMinute str) ;;

    let checkDate str = (checkDay str) && (checkMonth str) && (checkYear str) ;;

    let createDateFromDate ss mm hh dd mm yy =
        fst (Unix.mktime
                 {
                    Unix.tm_sec = ss;
                    tm_min = mm;
                    tm_hour = hh;
                    tm_mday = dd;
                    tm_mon = mm - 1;
                    tm_year = yy - 1900;
                    tm_wday = 0;
                    tm_yday = 0;
                    tm_isdst = false
                })
        ;;

(*
 * simple add: add simply a date and a time.
 * do not add two dates together or you will be 2000 years in future :)
 **)
    let simple_add d1 d2 = d1 +. d2;;

(*
 * simple sub: sub simply a date and a time.
 * do not sub two dates together or you will be close to Jesus Christ :)
 **)
    let simple_sub d1 d2 = d1 -. d2;;

(*
*  adds two complete dates as dd//mm//yyyy//hh//mm/ss;
*)
    let normal_add d1 d2 =
        if d1 >= d2
        then let interval = d1 -. d2 in d1 +. interval
        else let interval = d2 -. d1 in d1 +. interval
    ;;

(*
* subs two complete dates as dd//mm//yyyy//hh//mm/ss;
*)
    let normal_sub d1 d2 =
        if d1 >= d2
        then let interval = d1 -. d2 in d1 -. interval
        else let interval = d2 -. d1 in d1 -. interval
    ;;

    let createDateFromSec = function
    | 0 -> -42.
    | other -> float_of_int other
    ;;

    let get_epoch_day_from_string = function
    | ""        -> -42.
    | str when (checkDate str) = false -> -42.
    | other     -> Scanf.sscanf other "%02d-%02d-%04d"
        (fun dd mm yyyy -> fst (Unix.mktime
                 {
                    Unix.tm_sec = 0;
                    tm_min = 0;
                    tm_hour = 0;
                    tm_mday = dd;
                    tm_mon = mm - 1;
                    tm_year = yyyy - 1900;
                    tm_wday = 0;
                    tm_yday = 0;
                    tm_isdst = false
                }))
    ;;

    let newDateFromString str =
        get_epoch_day_from_string str
    ;;

    let get_epoch_time_from_string = function
    | "" -> -42.
    | str when (checkTime str) = false -> -42.
    | other ->
        Scanf.sscanf other "%02d:%02d"
        (fun hh mm -> fst (Unix.mktime
                 {
                    Unix.tm_sec = 0;
                    tm_min = mm;
                    tm_hour = hh + 1;
                    tm_mday = 1;
                    tm_mon = 0;
                    tm_year = 70;
                    tm_wday = 0;
                    tm_yday = 0;
                    tm_isdst = false
                }))
    ;;

    let newTimeFromString str =
      get_epoch_time_from_string str
    ;;

    let addDay date day =
        let tmp = day * 24 * 60 * 60 in date +. (float_of_int tmp)
    ;;

    let addHour date hour =
        let tmp = hour * 60 * 60 in date +. (float_of_int tmp)
    ;;

    let addMinute date min =
        let tmp = min * 60 in date +. (float_of_int tmp)
    ;;

    let checkOverlap trip1 trip2 = match trip1, trip2 with
    | (d1, a1), (d2, a2) when d1 <= d2  -> if a1 > d2 then true else false
    | (d1, a1), (d2, a2) when d2 < d1   -> if a2 > d1 then true else false
    | _, _                              -> false
    ;;

    let toString time =
        let tm = localtime time in
            sprintf "%02d-%02d-%04d,%02d:%02d"
            tm.tm_mday
            (tm.tm_mon + 1)
            (tm.tm_year + 1900)
            tm.tm_hour
            tm.tm_min
    ;;

    let isValid = function
    | -42. | -84. -> false
    | _           -> true
    ;;

    let createDateFromString date time =
        get_epoch_time_from_string time +.
        get_epoch_day_from_string date
    ;;
end
