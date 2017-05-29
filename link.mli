open Train
open Date

type city = string

module type LINK =
  sig
    type link
    type step
    type stepList

    (*  Create a new link module,                       *)
    (*  The first parameter is the refered city         *)
    (*  The second is the city linked to this reference *)
    val newLink       : city -> city -> link
    val newStep       : link -> Date.date -> train_type -> step

    val getFrom       : link -> city
    val getTo         : link -> city
    val getDist       : link -> distance

    val getLink       : step -> link

    val toString      : link -> string
    val printLinkList : link list -> unit
    val printStep     : step -> unit

    (*  Create a new list of link                           *)
    (*  = All cities linked to the city passed as parameter *)
    (*  Example : newLinkList "Rennes" return               *)
    (*  [("Rennes","Brest", 248); ("Rennes","Le Mans", 163)]                  *)
    val newLinkList   : city -> link list

    val isLink        : city -> city -> bool
    val allCities     : unit -> city list
    val makeTrip      : city -> city -> Date.date -> train_type-> stepList
    val trainAllowed  : link -> train_type -> bool

  end

module Link : LINK
