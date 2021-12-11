open Game
open Grid
open Game_map
open Player
open Command

let unrandomized_map =
  [
    set_grid "Carpenter Hall" 0 (Vacant_land 780.);
    set_grid "Willard Staright Hall" 1 (Vacant_land 1400.);
    set_grid "Olin Hall" 2 (Vacant_land 1200.);
    set_grid "Rhodes Hall" 3 (Vacant_land 1100.);
    set_grid "Gates Hall" 4 (Vacant_land 2500.);
    set_grid "Bard Hall" 5 (Vacant_land 1120.);
    set_grid "Upson Hall" 6 (Vacant_land 2000.);
    set_grid "Klarman Hall" 7 (Vacant_land 600.);
    set_grid "Goldman Smith Hall" 8 (Vacant_land 800.);
    set_grid "Phillips Hall" 9 (Vacant_land 350.);
    set_grid "Uris Hall" 10 Jail;
    set_grid "Kimball Hall" 11 (Vacant_land 760.);
    set_grid "Stocking Hall" 12 (Vacant_land 1600.);
    set_grid "Thurton Hall" 13 (Vacant_land 720.);
    set_grid "Warren Hall" 14 (Vacant_land 620.);
    set_grid "Draw Cards" 15 Draw_Card;
    set_grid "Hollister Hall" 16 (Vacant_land 530.);
    set_grid "Law School" 17 (Vacant_land 380.);
    set_grid "Olin Library" 18 (Vacant_land 650.);
    set_grid "Balch Hall" 19 (Vacant_land 420.);
    set_grid "Anabel Taylor Hall" 20 (Vacant_land 440.);
    set_grid "Sibley Hall" 21 (Vacant_land 350.);
    set_grid "Milstein Hall" 22 (Vacant_land 500.);
    set_grid "Cornell Store" 23 Hospital;
    set_grid "Flora Rose House" 24 (Vacant_land 650.);
    set_grid "Day Hall" 25 (Vacant_land 900.);
    set_grid "Bethe House" 26 Draw_Card;
    set_grid "Draw Cards" 27 Draw_Card;
    set_grid "Draw Cards" 28 Draw_Card;
    set_grid "Draw Cards" 29 Draw_Card;
    set_grid "Draw Cards" 30 Draw_Card;
    set_grid "Draw Cards" 31 Draw_Card;
    set_grid "Draw Cards" 32 Draw_Card;
    set_grid "Draw Cards" 33 Draw_Card;
    set_grid "Savage Hall" 34 (Vacant_land 520.);
    set_grid "Draw Cards" 35 Draw_Card;
    set_grid "Draw Cards" 36 Draw_Card;
    set_grid "Becker House" 37 (Vacant_land 800.);
    set_grid "Duffield Hall" 38 (Vacant_land 920.);
    set_grid "Statler Hall" 39 (Vacant_land 730.);
  ]

(**[our_compare (n1, g1) (n2, g2)] is the comparison of two grids based
   on grid index*)
let our_compare (n1, g1) (n2, g2) = Stdlib.compare n1 n2

(**[get_player_by_name name players] is the player whose name is [name]*)
let get_player_by_name (name : string) (players : player array) : player
    =
  if get_player_name players.(0) = name then players.(0)
  else if get_player_name players.(1) = name then players.(1)
  else if get_player_name players.(2) = name then players.(2)
  else failwith "no such name"

(**[print_map map] prints information about [map]*)
let rec print_map map =
  match map with
  | [] -> print_endline ""
  | h :: t ->
      print_endline (string_of_grid h);
      print_map t

(**[print_players_property properties] prints the basic information
   about [properties] *)
let rec print_players_property (properties : g list) =
  match properties with
  | [] -> ""
  | h :: t ->
      "  The name of the property is: " ^ get_grid_name h
      ^ "\n   The position of the property is "
      ^ string_of_int (get_grid_index h)
      ^ "\n   The value of the property is "
      ^ string_of_float (get_grid_price h)
      ^ "\n   The toll of the property is "
      ^ string_of_float (get_grid_toll h)
      ^ "\n   The level of the property is "
      ^ string_of_int (get_grid_level h)
      ^ "\n\n"
      ^ print_players_property t

(**[print_player p] prints the basic information about player [p] *)
let print_player p =
  print_endline
    ("The name of the player is " ^ get_player_name p
   ^ "\nThe position of the current grid is "
    ^ string_of_int (get_grid_index (get_player_curr_pos p))
    ^ "\nThe money balance of the player is "
    ^ string_of_float (get_player_balance p)
    ^ "\n The properties of the player are: \n "
    ^ print_players_property (get_player_properties p))

(**[find player players] is the index of [player] in [players]*)
let find (player : player) (players : player array) =
  if players.(0) = player then 0
  else if players.(1) = player then 1
  else if players.(2) = player then 2
  else failwith "couldn't find the index by player"

let print_start () : unit =
  print_endline
    "Please input one of the following commands: \n\
    \ \"roll\" to roll dice, \n\
    \ \"quit\" to quit the game, \n\
    \ \"map\" to see the map, \n\
    \ \"player [player name]\" to check information about [player name],\n\
    \ \"grid [grid index]\" to check information about grid [grid \
     index]\"";
  print_string "> "

let print_roll curr_pos : unit =
  print_endline
    ("current grid: " ^ curr_pos
   ^ "\n\
     \ You can input one of the following commands:\n\
     \ \"buy\" to buy the land you are standing on, \n\
     \ \"sell [grid index]\" to sell your land at [grid index],");
  (ANSITerminal.print_string [ ANSITerminal.red ])
    " (ATTENTION: if your balance can't afford the toll, your land \
     properties will be sold at 50% of the original price; so consider \
     selling if you need more money.) \n\
    \ ";
  print_endline
    "\"upgrade\" to upgrade the land you are standing on if you are \
     the owner, \n\
    \ \"continue\" to end your turn, ";
  print_string "> "

(**[player_loop map players] process one turn for player and evaluates
   to new information of [map] and [players]*)
let player_loop map (players : player array) : map * player array =
  let rec handler map players =
    print_start ();
    match parse (read_line ()) with
    | exception _ ->
        print_endline "Invalid command";
        handler map players
    | Roll ->
        Random.self_init ();
        let steps = Random.int 6 + 1 in
        let updated_player = update_pos map players.(0) steps in
        players.(0) <- updated_player;
        let curr_pos =
          string_of_grid (get_player_curr_pos updated_player)
        in
        let curr_pos_index = get_player_curr_index updated_player in
        print_endline
          ("steps: " ^ string_of_int steps ^ ". Camel is at grid "
          ^ string_of_int curr_pos_index);
        let rec roll_handler map (players : player array) =
          let curr_grid = get_grid_by_index map curr_pos_index in
          match get_grid_type curr_grid with
          | Draw_Card ->
              let tuple = draw_card 0 players map in
              let new_map = fst tuple in
              let updated_players = snd tuple in
              (new_map, updated_players)
          | Owned_land p ->
              let toll = get_grid_toll curr_grid in
              let owner_name = get_grid_ownership curr_grid in
              let owner = get_player_by_name owner_name players in
              if owner_name <> "Camel" then
                let recipient_idx = find owner players in
                let output =
                  bankruptcy_toll 0 recipient_idx players map toll
                in
                (fst output, snd output)
              else (map, players)
          | _ -> (
              print_roll curr_pos;
              match parse (read_line ()) with
              | Buy -> (
                  try
                    let tuple =
                      buy_land updated_player map curr_pos_index
                    in
                    let new_map = fst tuple in
                    let new_player = snd tuple in
                    Printf.printf
                      "Transaction succeeded. Current Balance: %f\n"
                      (get_player_balance new_player);
                    players.(0) <- new_player;
                    (new_map, players)
                  with
                  | Failure s ->
                      print_endline s;
                      roll_handler map players)
              | Sell g -> (
                  try
                    let tuple = sell_land updated_player map g 1. in
                    let new_map = fst tuple in
                    let new_player = snd tuple in
                    Printf.printf
                      "Transaction succeeded. Current Balance: %f\n"
                      (get_player_balance new_player);
                    players.(0) <- new_player;
                    (new_map, players)
                  with
                  | Failure s ->
                      print_endline s;
                      roll_handler map players)
              | Upgrade -> (
                  try
                    let tuple =
                      upgrade_land updated_player map curr_pos_index
                    in
                    let new_map = fst tuple in
                    let new_player = snd tuple in
                    Printf.printf
                      "Transaction succeeded. Current Balance: %f\n"
                      (get_player_balance new_player);
                    players.(0) <- new_player;
                    (new_map, players)
                  with
                  | Failure s ->
                      print_endline s;
                      roll_handler map players)
              | Continue -> (map, players)
              | exception _ ->
                  print_endline "Invalid Command";
                  handler map players
              | _ ->
                  print_endline "Invalid Command";
                  handler map players)
        in
        roll_handler map players
    | Grid t ->
        if t < List.length map && t >= 0 then (
          t |> get_grid_by_index map |> string_of_grid |> print_endline;
          handler map players)
        else (
          print_endline "Invalid grid index";
          handler map players)
    | Map ->
        print_map map;
        handler map players
    | Quit ->
        print_endline "See you next time!";
        exit 0
    | Player t ->
        if Array.mem t (Array.map get_player_name players) then (
          print_player (get_player_by_name t players);
          handler map players)
        else (
          print_endline "Player doesn't exist!";
          handler map players)
    | _ ->
        print_endline "Invalid command";
        handler map players
  in
  handler map players

(**[ai_loop_1 map players] process one turn for ai_1 and evaluates to
   new information of [map] and [players]*)
let ai_loop_1 map (players : player array) : map * player array =
  print_endline "AI_1's turn: \n";
  Random.self_init ();
  let steps = Random.int 6 + 1 in
  let updated_AI = update_pos map players.(1) steps in
  players.(1) <- updated_AI;
  let curr_grid = get_player_curr_pos updated_AI in
  let curr_index = get_grid_index curr_grid in
  print_endline
    ("steps: " ^ string_of_int steps ^ ". AI_1 is at grid "
    ^ (curr_grid |> get_grid_index |> string_of_int));
  match get_grid_type curr_grid with
  | Vacant_land p ->
      let balance = get_player_balance players.(1) in
      if balance >= p then (
        let bought =
          buy_land players.(1) map (get_grid_index curr_grid)
        in
        players.(1) <- snd bought;
        print_endline
          ("AI_1 bought " ^ get_grid_name curr_grid ^ " at index "
          ^ string_of_int curr_index
          ^ "\n");
        (fst bought, players))
      else (
        players.(1) <- updated_AI;
        (map, players))
  | Owned_land l ->
      let owner =
        get_player_by_name (get_grid_ownership curr_grid) players
      in
      if owner = players.(1) then
        let balance = get_player_balance players.(1) in
        let price = get_owned_land_price l in
        let curr_level = get_grid_level curr_grid in
        if balance >= price && curr_level < 5 then (
          print_endline
            ("AI_1 upgraded its property at index "
            ^ string_of_int curr_index
            ^ " to level "
            ^ string_of_int (get_grid_level curr_grid + 1)
            ^ "\n");
          let output =
            upgrade_land players.(1) map (get_grid_index curr_grid)
          in
          players.(1) <- snd output;
          (fst output, players))
        else (
          players.(1) <- updated_AI;
          (map, players))
      else
        let toll = get_grid_toll curr_grid in
        let recipient_idx = find owner players in
        bankruptcy_toll 1 recipient_idx players map toll
  | Draw_Card ->
      let tuple = draw_card 1 players map in
      let new_map = fst tuple in
      let updated_players = snd tuple in
      (new_map, updated_players)
  | _ ->
      players.(1) <- updated_AI;
      (map, players)

(**[ai_loop_2 map players] process one turn for ai_2 and update
   information about [map] and [players]*)
let ai_loop_2 map (players : player array) : map * player array =
  print_endline "AI_2's turn: \n";
  Random.self_init ();
  let steps = Random.int 6 + 1 in
  let updated_AI = update_pos map players.(2) steps in
  players.(2) <- updated_AI;
  let curr_grid = get_player_curr_pos updated_AI in
  let curr_index = get_grid_index curr_grid in
  print_endline
    ("steps: " ^ string_of_int steps ^ ". AI_2 is at grid "
    ^ (curr_grid |> get_grid_index |> string_of_int));
  match get_grid_type curr_grid with
  | Vacant_land p ->
      let balance = get_player_balance players.(2) in
      Random.self_init ();
      let rand = Random.int 2 in
      if balance >= p && rand = 1 then (
        let bought =
          buy_land players.(2) map (get_grid_index curr_grid)
        in
        players.(2) <- snd bought;
        print_endline
          ("AI_2 bought " ^ get_grid_name curr_grid ^ " at index "
          ^ string_of_int curr_index
          ^ "\n");
        (fst bought, players))
      else (
        players.(2) <- updated_AI;
        (map, players))
  | Owned_land l ->
      let owner =
        get_player_by_name (get_grid_ownership curr_grid) players
      in
      if owner = players.(2) then (
        let balance = get_player_balance players.(2) in
        let price = get_owned_land_price l in
        let curr_level = get_grid_level curr_grid in
        Random.self_init ();
        let rand = Random.int 2 in
        if balance >= price && curr_level < 5 && rand = 1 then (
          print_endline
            ("AI_2 upgraded its property at index "
            ^ string_of_int curr_index
            ^ " to level "
            ^ string_of_int (get_grid_level curr_grid + 1)
            ^ "\n");
          let output =
            upgrade_land players.(2) map (get_grid_index curr_grid)
          in
          players.(2) <- snd output;
          (fst output, players))
        else (
          players.(2) <- updated_AI;
          (map, players)))
      else
        let toll = get_grid_toll curr_grid in
        let recipient_idx = find owner players in
        bankruptcy_toll 2 recipient_idx players map toll
  | Draw_Card ->
      let tuple = draw_card 2 players map in
      let new_map = fst tuple in
      let updated_players = snd tuple in
      (new_map, updated_players)
  | _ ->
      players.(2) <- updated_AI;
      (map, players)

(** [game_loop map players num_players player_index] runs the game for
    player who has [player_index] in [players] and update [map],
    [players], and [num_players] as needed.[player_index] increases by 1
    (and returns to 0 when reaches players' length) to iterate all
    players. [num_players] records the remaining players in the game, if
    [num_players] = 1 then the person who left wins*)

let rec game_loop map players num_players player_index =
  print_endline
    ("Number of players in the current game is "
    ^ string_of_int num_players);
  let curr_player = players.(player_index) in
  if num_players = 1 && get_stay curr_player <> -1 then
    print_endline (get_player_name curr_player ^ " won!")
  else
    let updated_map = ref map in
    let curr_stay = get_stay curr_player in
    if curr_stay = -1 then
      game_loop !updated_map players num_players
        ((player_index + 1) mod Array.length players)
    else if curr_stay > 0 then (
      print_endline
        ("------------------------"
        ^ get_player_name curr_player
        ^ " mustn't move. Pause for " ^ string_of_int curr_stay
        ^ " rounds.");
      players.(player_index) <- update_stay curr_player (curr_stay - 1);

      game_loop !updated_map players num_players
        ((player_index + 1) mod Array.length players))
    else if player_index = 0 then (
      (ANSITerminal.print_string [ ANSITerminal.yellow ])
        "------------------------Your turn\n";
      let result = player_loop map players in
      let updated_players = snd result in
      updated_map := fst result;
      if get_stay updated_players.(0) = -1 then
        game_loop !updated_map updated_players (num_players - 1)
          ((player_index + 1) mod Array.length updated_players)
      else
        game_loop !updated_map updated_players num_players
          ((player_index + 1) mod Array.length updated_players))
    else if player_index = 1 then (
      (ANSITerminal.print_string [ ANSITerminal.cyan ])
        "------------------------AI_1's turn \n";
      let result = ai_loop_1 map players in
      let updated_players = snd result in
      updated_map := fst result;
      if get_stay updated_players.(1) = -1 then
        game_loop !updated_map updated_players (num_players - 1)
          ((player_index + 1) mod Array.length updated_players)
      else
        game_loop !updated_map updated_players num_players
          ((player_index + 1) mod Array.length updated_players))
    else (
      (ANSITerminal.print_string [ ANSITerminal.magenta ])
        "------------------------AI_2's turn \n";
      let result = ai_loop_2 map players in
      let updated_players = snd result in
      updated_map := fst result;
      if get_stay updated_players.(2) = -1 then
        game_loop !updated_map updated_players (num_players - 1)
          ((player_index + 1) mod Array.length updated_players)
      else
        game_loop !updated_map updated_players num_players
          ((player_index + 1) mod Array.length updated_players))

(**[change_index m counter] is a map with indexes in each grid arranges
   increasingly from 0 to 39*)
let rec change_index m counter =
  match m with
  | [] -> []
  | h :: t ->
      set_grid (get_grid_name h) counter (get_grid_type h)
      :: change_index t (counter + 1)

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to the Monopoly game.\n";
  Random.self_init ();
  (*The following raw_map function is referenced from this stackoverlow
    post
    https://stackoverflow.com/questions/15095541/how-to-shuffle-list-in-on-in-ocaml*)
  let raw_map =
    let nd = List.map (fun c -> (Random.bits (), c)) unrandomized_map in
    let sond = List.sort our_compare nd in
    List.map snd sond
  in
  let init_map = change_index raw_map 0 in

  let init_player_array =
    [|
      create_player "Camel" 3000. [] (get_grid_by_index init_map 0);
      create_player "AI_1" 3000. [] (get_grid_by_index init_map 10);
      create_player "AI_2" 3000. [] (get_grid_by_index init_map 20);
    |]
  in
  game_loop init_map init_player_array 3 0

(* Execute the game engine. *)
let () = main ()
