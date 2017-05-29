open Link

type city = string

exception Unknown_City

module type STATION =
  sig
    type station

    val newStation      : city -> station
    val newStationList  : string -> station list

    val toString    : station -> string

    val isValidCity : city -> bool
  end

module Station : STATION =
struct
  type station = Scnst of (city * Link.link list)

  (****************************************************************************)
  (* Constructors *)
  let newStation city = Scnst(city, Link.newLinkList city);;

  let isValidCity city =
    let cmp s = (s = city) in
    List.exists cmp (Link.allCities ())
  ;;

  let newStationList lis =
    let split_on_char c str =
      let rec loop index len lis word = match (String.get str index) with
        | ' ' when index < len  -> loop (index + 1) len (lis@[word]) ""
        | _   when index < len  -> loop (index + 1) len lis (word ^ String.make 1 (String.get str index))
        | _   -> (lis@[word ^ String.make 1 (String.get str index)])
      in loop 0 ((String.length str) - 1) [] ""
    in

    let rec loop sList = function
      | hd::tl when (isValidCity hd == true)  -> loop ((newStation hd)::sList) tl
      | hd::tl when (isValidCity hd == false) -> raise Unknown_City
      | [] | _::_ -> sList
    in loop [] (split_on_char ',' lis)
  ;;

  (****************************************************************************)
  (* ToString functions *)
  let toString station = "TO DO";;

  (****************************************************************************)

end
