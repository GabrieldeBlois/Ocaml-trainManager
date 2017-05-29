open TrainManager

exception Create_Too_Much_Arguments
exception Create_Too_Few_Arguments

let main () =
  let split_on_char c str =
    let rec loop index len lis word = match (String.get str index) with
      | ' ' when index < len  -> loop (index + 1) len (lis@[word]) ""
      | _   when index < len  -> loop (index + 1) len lis (word ^ String.make 1 (String.get str index))
      | _   -> (lis@[word ^ String.make 1 (String.get str index)])
    in loop 0 ((String.length str) - 1) [] ""
  in

  let parseCmd line =
    let ret = function
      | hd::tl  when String.compare hd "" != 0 ->  hd
      | _::_ | []   -> ""
    in ret (split_on_char ' ' line)
  in

  let parseArgs line =
    let ret = function
      | hd::tl ->  tl
      | []     ->  []
    in ret (split_on_char ' ' line)
  in

  let rec shell tripList =
    let line = read_line () in
    match (parseCmd line) with
      | "create"  ->  shell (TrainManager.createTrip tripList (parseArgs line))
      | "delete"  ->  shell (TrainManager.deleteTrip tripList (parseArgs line))
      | "list"    ->  (TrainManager.printTrip tripList; shell tripList)
      | "quit"    ->  ()
      | _         ->  (prerr_endline "Invalid Command"; shell tripList)
  in shell []
;;

let _ = main ()
