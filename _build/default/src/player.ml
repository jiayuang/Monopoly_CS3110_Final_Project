open Grid
open Game_map
(* open Property *)

type player = {
  player_name : string;
  balance : float;
  properties : g list;
  curr_grid : g;
  stay : int;
}

let create_player name balance properties curr_grid =
  { player_name = name; balance; properties; curr_grid; stay = 0 }

let create_player_with_stay name balance properties curr_grid stay =
  { player_name = name; balance; properties; curr_grid; stay }

let get_player_name player = player.player_name

let get_player_balance player = player.balance

let get_player_properties player = player.properties

let get_player_curr_pos player = player.curr_grid

let get_player_curr_index player = get_grid_index player.curr_grid

let update_balance player new_balance : player =
  { player with balance = new_balance }

let update_balance_properties player new_balance new_property =
  let new_properties = new_property :: player.properties in
  { player with balance = new_balance; properties = new_properties }

let rec update_property properties acc new_property old_property =
  match properties with
  | [] -> failwith "Not found corresponding old property"
  | h :: t ->
      if get_grid_name h = get_grid_name new_property then
        acc @ [ new_property ] @ t
      else update_property t (h :: acc) new_property old_property

let rec remove_property properties acc removed_property =
  match properties with
  | [] -> failwith "no property left"
  | h :: t ->
      if h = removed_property then acc @ t
      else remove_property t (h :: acc) removed_property

let positive_mod length index = index mod length

let update_pos game_map player steps =
  player |> get_player_curr_pos |> get_grid_index |> ( + ) steps
  |> positive_mod (get_map_size game_map)
  |> get_grid_by_index game_map
  |> create_player player.player_name player.balance player.properties

let buy_land player map grid_index =
  let grid = get_grid_by_index map grid_index in
  match get_grid_type grid with
  | Vacant_land p ->
      if get_player_balance player >= p then
        let new_grid =
          set_grid_type grid
            (Owned_land
               {
                 land_price = p;
                 ownership = get_player_name player;
                 toll = 20. *. p;
                 level = 0;
               })
        in
        ( update_grid_type_in_map map
            (get_grid_type new_grid)
            [] grid_index,
          update_balance_properties player
            (get_player_balance player -. p)
            new_grid )
      else failwith "Not enough balance"
  | _ -> failwith "Cannot buy non-vacant land"

let upgrade_land player map grid_index =
  let grid = get_grid_by_index map grid_index in
  match get_grid_type grid with
  | Owned_land ol when get_grid_ownership grid = get_player_name player
    ->
      let price = get_grid_price grid in
      let curr_level = get_grid_level grid in
      if curr_level = 5 then failwith "Reached highest level"
      else
        let new_grid =
          set_grid_type grid
            (Owned_land
               {
                 ol with
                 land_price = price *. 1.2;
                 toll = price *. 1.2 *. 20.;
                 level = get_grid_level grid + 1;
               })
        in
        let new_map =
          update_grid_type_in_map map
            (get_grid_type new_grid)
            [] grid_index
        in
        let updated_properties =
          update_property
            (get_player_properties player)
            [] new_grid grid
        in
        ( new_map,
          {
            player with
            balance = get_player_balance player -. (price *. 0.2);
            properties = updated_properties;
          } )
  | _ -> failwith "Cannot upgrade property on this land"

let sell_land player map grid_index coefficient =
  let grid = get_grid_by_index map grid_index in
  match get_grid_type grid with
  | Owned_land ol when get_grid_ownership grid = get_player_name player
    ->
      let old_price = get_grid_price grid in
      let price = coefficient *. old_price in
      let new_map =
        update_grid_type_in_map map (Vacant_land old_price) []
          grid_index
      in
      let new_player =
        {
          player with
          balance = get_player_balance player +. price;
          properties =
            remove_property (get_player_properties player) [] grid;
        }
      in
      (new_map, new_player)
  | _ -> failwith "This land is not owned"

(** [lose_land player map grid_index] evalutes to the updated map and
    players who loses property at [grid_index]*)
let lose_land player map grid_index =
  let grid = get_grid_by_index map grid_index in
  match get_grid_type grid with
  | Owned_land ol when get_grid_ownership grid = get_player_name player
    ->
      let price = get_grid_price grid in
      let new_map =
        update_grid_type_in_map map (Vacant_land price) [] grid_index
      in
      let new_player =
        {
          player with
          properties =
            remove_property (get_player_properties player) [] grid;
        }
      in
      (new_map, new_player)
  | _ -> failwith "exception: grid_index should be owned by you"

let get_stay player = player.stay

let update_stay (player : player) (new_time : int) =
  { player with stay = new_time }

(** [hospital_or_jail_stay player map] is the [player] *)
let hospital_or_jail_stay player map =
  Random.self_init ();
  let hospital =
    try List.find (fun x -> get_grid_type x = Hospital) map with
    | Not_found -> failwith "hospital problem"
  in
  let jail =
    try List.find (fun x -> get_grid_type x = Jail) map with
    | Not_found -> failwith "jail problem"
  in
  if Random.int 2 = 0 then (
    Random.self_init ();
    let rounds = 1 + Random.int 3 in
    print_endline
      ("You will stay in Hospital for " ^ string_of_int rounds
     ^ "rounds.");
    { player with curr_grid = hospital; stay = rounds })
  else (
    Random.self_init ();
    let rounds = 1 + Random.int 3 in
    print_endline
      ("You will stay in Jail for " ^ string_of_int rounds ^ " rounds.");
    { player with curr_grid = jail; stay = rounds })

(** [modify_land player map] is the [player] who either has one random
    property upgraded or lose one random property and the updated map*)
let modify_land player map =
  let properties = get_player_properties player in
  if List.length properties = 0 then (
    print_endline
      "Player does not have any property, so no property is \
       confiscated.";
    (map, player))
  else (
    Random.self_init ();
    let rand_idx = Random.int (List.length properties) in
    let prop_idx = get_grid_index (List.nth properties rand_idx) in
    let up_or_lose = Random.int 2 in
    if up_or_lose = 0 then (
      print_endline
        ("The card upgrades your land at " ^ string_of_int prop_idx
       ^ ".\n");
      upgrade_land player map prop_idx)
    else (
      print_endline
        ("The card confiscates your land at grid index "
       ^ string_of_int prop_idx ^ ".\n");
      lose_land player map prop_idx))

let rec bankruptcy_toll payer_index recipient_index players map toll =
  let payer = players.(payer_index) in
  let recipient = players.(recipient_index) in
  let payer_balance = get_player_balance payer in
  let recipient_balance = get_player_balance recipient in
  let payer_name = get_player_name payer in
  let recipient_name = get_player_name recipient in
  if payer_balance >= toll then (
    print_endline
      (payer_name ^ " paid the toll of " ^ string_of_float toll ^ " to "
     ^ recipient_name ^ "\n --- " ^ payer_name
     ^ "'s remaining balance: "
      ^ string_of_float (payer_balance -. toll));
    print_endline
      (recipient_name ^ " received the toll of " ^ string_of_float toll
     ^ "\n ---" ^ recipient_name ^ "'s remaining balance: "
      ^ string_of_float (recipient_balance +. toll));
    players.(payer_index) <- update_balance payer (payer_balance -. toll);
    players.(recipient_index) <-
      update_balance recipient (recipient_balance +. toll);
    (map, players))
  else
    match get_player_properties payer with
    | h :: t ->
        print_endline
          (get_player_name payer ^ "'s land at index "
          ^ string_of_int (get_grid_index h)
          ^ " is traded to make up the amount to pay the toll to "
          ^ get_player_name recipient);
        let tuple = sell_land payer map (get_grid_index h) 0.5 in
        players.(payer_index) <- snd tuple;
        bankruptcy_toll payer_index recipient_index players (fst tuple)
          toll
    | [] ->
        print_endline
          ("Oops, " ^ payer_name
         ^ " don't have enough balance to pay the toll. " ^ payer_name
         ^ " went bankrupt!");
        players.(payer_index) <- update_stay payer (-1);
        players.(payer_index) <- update_balance players.(payer_index) 0.;
        print_endline
          (payer_name ^ " stay:"
          ^ string_of_int (get_stay players.(payer_index)));
        players.(recipient_index) <-
          update_balance recipient (payer_balance +. recipient_balance);
        (map, players)

let rec bankruptcy_lose payer_index players map amount =
  let payer = players.(payer_index) in
  let cur_balance = get_player_balance payer in
  let payer_name = get_player_name payer in
  if cur_balance >= amount then (
    print_endline
      ("you lose " ^ string_of_float amount ^ "$" ^ "\n ---"
     ^ payer_name ^ " remaining balance: "
      ^ string_of_float (cur_balance -. amount));
    players.(payer_index) <- update_balance payer (cur_balance -. amount);
    (map, players))
  else
    match get_player_properties payer with
    | h :: t ->
        print_endline
          (get_player_name payer ^ "'s land at index "
          ^ string_of_int (get_grid_index h)
          ^ " is traded to make up the amount to pay the money");
        let tuple = sell_land payer map (get_grid_index h) 0.5 in
        players.(payer_index) <- snd tuple;
        bankruptcy_lose payer_index players (fst tuple) amount
    | [] ->
        print_endline
          ("Oops, " ^ payer_name
         ^ " don't have enough balance to pay the toll. " ^ payer_name
         ^ " went bankrupt!");
        players.(payer_index) <- update_stay payer (-1);
        (map, players)

let modify_money map payer_index players =
  let player = players.(payer_index) in
  let old_balance = get_player_balance player in
  let delta =
    Random.float (0.5 *. old_balance) +. (0.25 *. old_balance)
  in
  Random.self_init ();
  if Random.int 2 = 1 then (
    players.(payer_index) <- update_balance player (delta +. old_balance);
    print_endline
      ("Congrats! " ^ get_player_name player ^ " won a lottery of $"
     ^ string_of_float delta ^ "!" ^ " Current balance: "
      ^ string_of_float (delta +. old_balance));
    players)
  else (
    print_endline ("Oops, you lost $" ^ string_of_float delta);
    snd (bankruptcy_lose payer_index players map delta))

let draw_card player_index players map =
  print_endline "Drawing a card:";
  let player = players.(player_index) in
  Random.self_init ();
  let card_idx = Random.int 3 in
  if card_idx = 0 then (
    print_endline "You drew a balance-changing card";
    (map, modify_money map player_index players))
  else if card_idx = 1 then (
    print_endline
      "You drew a card that pauses you for a number of rounds";
    players.(player_index) <- hospital_or_jail_stay player map;
    (map, players))
  else (
    print_endline "You drew a card that modifies your land properties";
    let updated_players = modify_land player map in
    players.(player_index) <- snd updated_players;
    (fst updated_players, players))
