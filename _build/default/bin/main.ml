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

(** [buy_sell_helper]'s function argument takes up 8 lines, which should
    essentially be 1 line.*)
let buy_sell_helper
    roll_handler
    updated_player
    players
    curr_pos_index
    map
    idx
    g =
  try
    let tuple =
      if idx = 0 then buy_land updated_player map curr_pos_index
      else sell_land updated_player map g 1.
    in
    let new_map = fst tuple in
    let new_player = snd tuple in
    Printf.printf "Transaction succeeded. Current Balance: %f\n"
      (get_player_balance new_player);
    players.(0) <- new_player;
    (new_map, players)
  with
  | Failure s ->
      print_endline s;
      roll_handler map players

(*[buy_sell_upgrade] has to exceed line limits because it must pattern
  match to six possible user prompts and OcamlFormat separates print
  statements and expressions into multiple lines*)

(** [buy_sell_upgrade]'s function argument takes up 7 lines, which
    should essentially be 1 line. Also, 173-175, 181-182 and 176-178
    should each be 1 line. *)
let buy_sell_upgrade
    roll_handler
    handler
    (updated_player : player)
    (players : player array)
    (curr_pos_index : int)
    (map : map) : map * player array =
  match parse (read_line ()) with
  | Buy ->
      buy_sell_helper roll_handler updated_player players curr_pos_index
        map 0 0
  | Sell g ->
      buy_sell_helper roll_handler updated_player players curr_pos_index
        map 1 g
  | Upgrade -> (
      try
        let map_player =
          upgrade_land updated_player map curr_pos_index
        in
        Printf.printf "Transaction succeeded. Current Balance: %f\n"
          (get_player_balance (snd map_player));
        players.(0) <- snd map_player;
        (fst map_player, players)
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
      handler map players

(* the [player_loop] functions has to exceed the limited number of lines
   because of the inevitable OcamlFormat that pushes arguments and
   expressions into separte lines. We has a total of 6 cases to pattern
   math, and creating helper funtions for each case will destroy
   readability*)

(**[player_loop map players] process one turn for player and evaluates
   to new information of [map] and [players] *)
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
              let map_players = draw_card 0 players map in
              (fst map_players, snd map_players)
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
          | _ ->
              print_roll curr_pos;
              buy_sell_upgrade roll_handler handler updated_player
                players curr_pos_index map
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

let print_ai_roll steps curr_grid ai_idx =
  print_endline
    ("steps: " ^ string_of_int steps ^ ". AI_" ^ ai_idx ^ " is at grid "
    ^ (curr_grid |> get_grid_index |> string_of_int))

let print_ai_buy curr_grid curr_index ai_idx =
  print_endline
    ("AI_" ^ ai_idx ^ " bought " ^ get_grid_name curr_grid
   ^ " at index "
    ^ string_of_int curr_index
    ^ "\n")

let print_ai_upgrade curr_index curr_grid ai_idx =
  print_endline
    ("AI_" ^ ai_idx ^ " upgraded its property at index "
    ^ string_of_int curr_index
    ^ " to level "
    ^ string_of_int (get_grid_level curr_grid + 1)
    ^ "\n")

(*OcamlFormat separates let and if expressions into multiple lines and
  [ai_loop_1], thus surpassing the line limits for [ai_loop_1] function.
  And we can't factor out subparts of the function because the random
  number generator can't propagate into helper functions*)

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
  print_ai_roll steps curr_grid "1";
  match get_grid_type curr_grid with
  | Vacant_land p ->
      let balance = get_player_balance players.(1) in
      if balance >= p then (
        let bought =
          buy_land players.(1) map (get_grid_index curr_grid)
        in
        players.(1) <- snd bought;
        print_ai_buy curr_grid curr_index "1";
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
          print_ai_upgrade curr_index curr_grid "1";
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
      (fst tuple, snd tuple)
  | _ ->
      players.(1) <- updated_AI;
      (map, players)

(*OcamlFormat separates let expressions, function arguments, and if else
  statements into multiple lines. It has to handle four possible types
  of grid. We can't factor out subparts of the function because the
  random number generator wouldn't propagate into helper functions.All
  these cause [ai_loop_2] surpassing the line limits *)

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
  print_ai_roll steps curr_grid "2";
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
        print_ai_buy curr_grid curr_index "2";
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
          print_ai_upgrade curr_index curr_grid "2";
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
      (fst tuple, snd tuple)
  | _ ->
      players.(2) <- updated_AI;
      (map, players)

let print_num_players (num_players : int) =
  print_endline
    ("Number of players in the current game is "
    ^ string_of_int num_players)

(** [game_loop map players num_players player_index] runs the game for
    player who has [player_index] in [players] and update [map],
    [players], and [num_players] as needed.[player_index] increases by 1
    (and returns to 0 when reaches players' length) to iterate all
    players. [num_players] records the remaining players in the game, if
    [num_players] = 1 then the person who left wins*)

(*[game_loop] has to exceed limited length because this is the single
  most important game engine that handles the logic of the game and
  different stages of the game including how the game proceeds with some
  players go bankrupted.*)
let rec game_loop map players num_players player_index =
  print_num_players num_players;
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
      game_loop_helper updated_players num_players updated_map
        player_index 0)
    else if player_index = 1 then (
      (ANSITerminal.print_string [ ANSITerminal.cyan ])
        "------------------------AI_1's turn \n";
      let result = ai_loop_1 map players in
      let updated_players = snd result in
      updated_map := fst result;
      game_loop_helper updated_players num_players updated_map
        player_index 1)
    else (
      (ANSITerminal.print_string [ ANSITerminal.magenta ])
        "------------------------AI_2's turn \n";
      let result = ai_loop_2 map players in
      let updated_players = snd result in
      updated_map := fst result;
      game_loop_helper updated_players num_players updated_map
        player_index 2)

and game_loop_helper
    updated_players
    num_players
    updated_map
    player_index
    p_index =
  if get_stay updated_players.(p_index) = -1 then
    game_loop !updated_map updated_players (num_players - 1)
      ((player_index + 1) mod Array.length updated_players)
  else
    game_loop !updated_map updated_players num_players
      ((player_index + 1) mod Array.length updated_players)

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
