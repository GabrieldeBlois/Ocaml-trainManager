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

module MakeTrip : MAKETRIP = functor (Tr : TRAIN) (Lk : LINK) ->
  struct
    type trip = TripCnst of (int * train_type * Lk.step list)

    (****************************************************************************)
    (* Getters *)
    let getId tp        = match tp with | TripCnst(i,_,_)   -> i;;
    let getTrainType tp = match tp with | TripCnst(_,tt,_)  -> tt;;

    (****************************************************************************)
    (* Constructor *)
    let rec newTripId tList tt =
      let i = Random.int 999 in
      let getFirstPart = function
        | TGV       -> 1000
        | Eurostar  -> 2000
        | Thalys    -> 3000
        | Invalid   -> 9000
      in
      let rec isGoodId id = function
        | hd::tl when (getId hd) = id -> false
        | hd::tl  -> isGoodId id tl
        | []      -> true
      in
      match (isGoodId ((getFirstPart tt) + i) tList) with
        | true  -> ((getFirstPart tt) + i)
        | false -> newTripId tList tt
    ;;

    let newTrip (t,date,sl) tList =
      let tt = Tr.trainTypeFromString t in
      let rec newStepList ret date = function
        | hd::tl when tl = [] -> ret
        | hd::tl  ->
          begin
            let lk = (Lk.newLink hd (List.hd tl)) in
            let dt = Date.addMinute date ((Tr.getTravelDuration tt (Lk.getDist lk)) + 10) in
            newStepList (ret@[(Lk.newStep lk date tt)]) dt tl
          end
        | [] -> ret
      in TripCnst(newTripId tList tt, Tr.trainTypeFromString t, newStepList [] date sl)
    ;;

    (****************************************************************************)
    (* Print Functions *)
    let printTrip tp = match tp with | TripCnst(id,tt,sl) ->
      print_endline ((Tr.trainTypeToString tt) ^ " " ^ string_of_int id);
      print_string  ((Lk.getFrom (Lk.getLink (List.hd sl))) ^ " (,)");
      flush stdout;
      let rec loop = function
        | TripCnst(id,tt,hd::tl)  -> (Lk.printStep hd; loop (TripCnst(id,tt,tl)))
        | TripCnst(_,_,[])        -> print_endline " (,)"
      in loop tp
    ;;

  end

module Trip = MakeTrip (Train)(Link)
