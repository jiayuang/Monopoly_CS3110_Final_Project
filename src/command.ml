type command =
  | Roll
  | Grid of int
  | Player of string
  | Map
  | Quit
  | Buy
  | Sell of int
  | Upgrade
  | Continue

exception Empty

exception Malformed

let check_empty str =
  if str = "" || String.trim str = "" then raise Empty else str

let rec remove_empty_string str_lst =
  List.filter (fun x -> x <> "") str_lst

let process str =
  str |> check_empty |> String.trim
  |> String.split_on_char ' '
  |> remove_empty_string

let parse str =
  match process str with
  | exception Empty -> raise Empty
  | [] -> raise Empty
  | [ h ] ->
      if h = "roll" then Roll
      else if h = "quit" then Quit
      else if h = "map" then Map
      else if h = "buy" then Buy
      else if h = "upgrade" then Upgrade
      else if h = "continue" then Continue
      else raise Malformed
  | [ x1; x2 ] ->
      if x1 = "grid" then
        try Grid (int_of_string x2) with
        | Failure s -> failwith "not an int"
      else if x1 = "player" then Player x2
      else if x1 = "sell" then
        try Sell (int_of_string x2) with
        | Failure s -> failwith "Cannot sell this land"
      else raise Malformed
  | h :: t -> raise Malformed