open Train
open Date

type city = string

module type LINK =
  sig
    type link
    type step
    type stepList

    val newLink       : city -> city -> link
    val newStep       : link -> Date.date -> train_type -> step

    val getFrom       : link -> city
    val getTo         : link -> city
    val getDist       : link -> distance

    val getLink       : step -> link

    val toString      : link -> string
    val printLinkList : link list -> unit
    val printStep     : step -> unit

    val newLinkList   : city -> link list

    val isLink        : city -> city -> bool
    val allCities     : unit -> city list

    val makeTrip      : city -> city -> Date.date -> train_type-> stepList
    val trainAllowed : link -> train_type -> bool

  end

exception New_Link_With_Invalid_Pair_Of_Cities

module Link : LINK =
struct
  type link = Lcnst of (city * city * distance)
  type step = Scnst of (link * Date.date * Date.date)
  type stepList = SLcnst of (step list) | NotFound

  (*type node = Ncnst of (city * link list)*)

  (*type graph = node list*)

  (*let rec addLinkToCity city link = function
  | [] -> []
  | Ncnst (tmpCity, x)::next when tmpCity = city -> Ncnst (city, link::x)::next
  | Ncnst (tmpCity, x)::next -> Ncnst (tmpCity, x)::addLinkToCity city link next
  ;;*)

  (****************************************************************************)
  (* Getter functions *)
  let getFrom l = match l with | Lcnst(c,_,_) -> c;;
  let getTo   l = match l with | Lcnst(_,c,_) -> c;;
  let getDist l = match l with | Lcnst(_,_,d) -> d;;

  let getLink s = match s with | Scnst(s,_,_) -> s;;

  (****************************************************************************)
  (* Constructors *)
  let newLink c1 c2 = match c1, c2 with
    | "Brest",    "Rennes"    | "Rennes",   "Brest"     -> Lcnst(c1, c2, 248)
    | "Rennes",   "Le Mans"   | "Le Mans",  "Rennes"    -> Lcnst(c1, c2, 163)
    | "Le Mans",  "Nantes"    | "Nantes",   "Le Mans"   -> Lcnst(c1, c2, 183)
    | "Le Mans",  "Paris"     | "Paris",    "Le Mans"   -> Lcnst(c1, c2, 201)
    | "Paris",    "Bordeaux"  | "Bordeaux", "Paris"     -> Lcnst(c1, c2, 568)
    | "Lille",    "Brussels"  | "Brussels", "Lille"     -> Lcnst(c1, c2, 106)
    | "Lille",    "London"    | "London",   "Lille"     -> Lcnst(c1, c2, 269)
    | "Paris",    "Lille"     | "Lille",    "Paris"     -> Lcnst(c1, c2, 225)
    | "Brussels", "Liege"     | "Liege",    "Brussels"  -> Lcnst(c1, c2, 104)
    | "Paris",    "Le Havre"  | "Le Havre", "Paris"     -> Lcnst(c1, c2, 230)
    | "Liege",    "Cologne"   | "Cologne",  "Liege"     -> Lcnst(c1, c2, 118)
    | "Cologne",  "Essen"     | "Essen",    "Cologne"   -> Lcnst(c1, c2, 81)
    | "Paris",    "Nancy"     | "Nancy",    "Paris"     -> Lcnst(c1, c2, 327)
    | "Dijon",    "Nancy"     | "Nancy",    "Dijon"     -> Lcnst(c1, c2, 226)
    | "Nancy",    "Strasbourg"    | "Strasbourg", "Nancy"     -> Lcnst(c1, c2, 149)
    | "Paris",    "Strasbourg"    | "Strasbourg", "Paris"     -> Lcnst(c1, c2, 449)
    | "Brussels", "Amsterdam"     | "Amsterdam",  "Brussels"  -> Lcnst(c1, c2, 211)
    | "Dijon",    "Strasbourg"    | "Strasbourg", "Dijon"     -> Lcnst(c1, c2, 309)
    | "Lyon",    "Marseille"    | "Marseille", "Lyon"     -> Lcnst(c1, c2, 325)
    | "Lyon",    "Paris"    | "Paris", "Lyon"     -> Lcnst(c1, c2, 427)
    | "Lyon",    "Dijon"    | "Dijon", "Lyon"     -> Lcnst(c1, c2, 192)
    | "Montpellier",    "Marseille"    | "Marseille", "Montpellier"     -> Lcnst(c1, c2, 176)
    | "Montpellier",    "Toulouse"    | "Toulouse", "Montpellier"     -> Lcnst(c1, c2, 248)
    | "Toulouse",    "Bordeaux"    | "Bordeaux", "Toulouse"     -> Lcnst(c1, c2, 256)
    | _,_ -> raise New_Link_With_Invalid_Pair_Of_Cities
  ;;

  let newStep link departDate trainType =
    Scnst (link, departDate,
          Date.addMinute departDate (Train.getTravelDuration trainType (getDist link)))
  ;;

  (****************************************************************************)
  (* ToString/Print Functions *)
  let toString = function
    | Lcnst(from,dest,d) -> (from ^ " -> ") ^ ((dest ^ "(") ^ ((string_of_int d) ^ " km)"))
  ;;

  let printLinkList l =
    let rec loop = function
      | hd::tl  ->  (print_endline (toString hd); loop tl)
      | []      -> ()
    in loop l
  ;;

  let printStep s = match s with | Scnst(link,departure,arrive) ->
    print_endline (" (" ^ (Date.toString departure) ^ ")");
    print_string  ((getTo link) ^ " (" ^ (Date.toString arrive) ^ ")");
    flush stdout
  ;;

  (****************************************************************************)
  (* Utils Functions *)
  let isLink c1 c2 =
    try ignore (newLink c1 c2); true
    with _ -> false
  ;;

  let allCities () =
    "Brest"::"Rennes"::"Le Mans"::"Nantes"::"Bordeaux"::"Paris"::
    "Le Havre"::"Lille"::"London"::"Brussels"::"Amsterdam"::
    "Liege"::"Cologne"::"Essen"::"Nancy"::"Dijon"::"Strasbourg"::[]
  ;;

  (****************************************************************************)
  (* List Constructor Function *)
  let newLinkList city =
    let rec loop ret = function
      | hd::tl when (isLink city hd)          -> loop ((newLink city hd)::ret) tl
      | hd::tl when (isLink city hd == false) -> loop ret tl
      | []  | _ -> ret
    in loop [] (allCities ())
  ;;

  let trainAllowed link trainType = match trainType with
    | Invalid -> false
    | aTrain -> match link with Lcnst (c1, c2, dist) ->
      let predicate train citySearched = match train with
        | Tcnst (_, _, cityList) ->
          let rec checkCityList = function
            | [] -> false
            | statName::_ when citySearched = statName -> true
            | _::next -> checkCityList next
          in checkCityList cityList
      in predicate (Train.newTrain aTrain) c2

let rec makeTrip dcity acity ddate trainType =
  let rec allLinkCheck = function
  | [] -> NotFound
  | Lcnst (c1, c2, dist)::next when (trainAllowed (Lcnst (c1, c2, dist)) trainType) = false -> NotFound
  | Lcnst (c1, c2, dist)::next when c2 = acity  -> SLcnst ((Scnst (Lcnst (c1, c2, dist), ddate, Date.addMinute ddate (Train.getTravelDuration trainType dist)))::[])
  | Lcnst (c1, c2, dist)::next                  ->
    match (makeTrip c2 acity (Date.addMinute ddate (Train.getTravelDuration trainType dist)) trainType) with
    | SLcnst (value) ->
    SLcnst ((Scnst (Lcnst (c1, c2, dist), ddate, Date.addMinute ddate (Train.getTravelDuration trainType dist)))::value)
    | NotFound -> allLinkCheck next
  in allLinkCheck (newLinkList dcity)

  (****************************************************************************)

end
