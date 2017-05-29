open Date
open Train
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

module TrainManager : TRAIN_MANAGER =
  struct

  type infos = (string * Date.date * string list * bool)

  let bad_infos () = ("", Date.newDateFromString "00-00-0000", ""::[], true);;

  (****************************************************************************)
  (* Parsing functions *)
  let split_on_char c str =
    let rec loop index len lis word = match (String.get str index) with
      | r   when (r = c) && index < len   -> loop (index + 1) len (lis@[word]) ""
      | _   when index < len              -> loop (index + 1) len lis (word ^ String.make 1 (String.get str index))
      | _   -> (lis@[word ^ String.make 1 (String.get str index)])
    in loop 0 ((String.length str) - 1) [] ""
  ;;

  let parseCreateArgs args =
    let rec loop (tt,date,lstation,err) c largs =
      match c, largs with
        | 0, hd::tl -> loop (hd, date, lstation, err) (c + 1) tl
        | 1, hd::tl -> loop (tt, Date.newDateFromString hd, lstation, err) (c + 1) tl
        | 2, hd::tl -> loop (tt, Date.simple_add date (Date.newTimeFromString hd), lstation, err) (c + 1) tl
        | 3, hd::tl -> loop (tt, date, split_on_char ',' hd, err) (c + 1) tl
        | 4, []     -> (tt,date,lstation,false)
        | _, []     -> (prerr_endline "Command error: too few arguments for create"; bad_infos ())
        | _, hd::tl -> (prerr_endline "Command error: too much arguments for create"; bad_infos ())
      in loop (bad_infos ()) 0 args
  ;;

  let parseDeleteArgs = function
    | hd::tl  -> hd
    | _       -> ""
  ;;

  (****************************************************************************)
  (* Command functions *)
  let createTrip tList args =
    match (parseCreateArgs args) with
    (* Error Checks *)
    | (_,_,_,err)  when err -> tList
    | (tt,_,_,_)   when (Train.trainTypeFromString tt) = Invalid -> (prerr_endline "Invalid train type"; tList)
    | (_,date,_,_) when (Date.isValid date) = false              -> (prerr_endline "Invalid date"; tList)
    | (_,_,sl,_)   when (List.length sl) < 2                     -> (prerr_endline "Invalid station list"; tList)
    (* Create TRIP *)
    | (tt,date,sl,_)  ->
      begin
        let newtp = Trip.newTrip (tt,date,sl) tList in
        (print_endline ("Trip created: " ^ tt ^ " " ^ (string_of_int (Trip.getId newtp)));
        newtp::tList)
      end
  ;;

  let deleteTrip tList args =
    let makeId tp =
      (Train.trainTypeToString (Trip.getTrainType tp) ^ string_of_int (Trip.getId tp))
    in

    let rec loop index ret = function
      | hd::tl when (makeId hd) = index ->
        begin
          if (List.length ret) = 0 then tl
          else ret@tl
        end
      | hd::tl  -> loop index (ret@[hd]) tl
      | []      -> ret
    in

    match (parseDeleteArgs args) with
      | ""  -> (prerr_endline "Delete command need one parameter"; tList)
      | a   -> loop a [] tList
  ;;

  let rec printTrip = function
    | hd::tl  -> Trip.printTrip hd; printTrip tl
    | []      -> ()
  ;;

  (****************************************************************************)

  end
