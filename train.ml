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

module Train : TRAIN =
struct

  (****************************************************************************)
  (* Constructors *)
  let newTrain = function
    | TGV       ->  Tcnst(TGV, 230, ["Brest";"Le Havre";"Lille";"Paris";"Strasbourg";"Nancy";"Dijon";"Lyon";"Nice";"Marseille";
                    "Montpellier"; "Perpignan";"Bordeaux";"Nantes";"Avignon";"Rennes";"Biarritz";"Toulouse";"Le Mans"])
    | Eurostar  ->  Tcnst(Eurostar, 160, ["Paris";"London";"Brussels";"Lille"])
    | Thalys    ->  Tcnst(Thalys, 210, ["Paris";"Lille";"Liege";"Brussels";"Amsterdam";"Cologne";"Essen"])
    | Invalid   ->  Tcnst(Invalid, 0, [])
  ;;


  (****************************************************************************)
  (* ToString functions *)
  let trainTypeToString = function
    | TGV       ->  "TGV"
    | Eurostar  ->  "Eurostar"
    | Thalys    ->  "Thalys"
    | Invalid   ->  "Invalid"
  ;;
  let toString = function
    | Tcnst(t,_,_)  -> trainTypeToString t
  ;;

  (****************************************************************************)
  (* Getter functions *)
  let trainTypeFromString = function
    | "TGV"       -> TGV
    | "Eurostar"  -> Eurostar
    | "Thalys"    -> Thalys
    | _           -> Invalid
  ;;
  let getSpeed = function
    | TGV       -> 230
    | Eurostar  -> 160
    | Thalys    -> 210
    | Invalid   -> 0

  (****************************************************************************)
  (* Return the duration(in minutes) to travel dist(in km)*)
  let getTravelDuration tt dist = match tt with
    | Invalid   -> 0
    | _         -> (dist * 60) / (getSpeed tt)
end
