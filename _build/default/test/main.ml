open OUnit2
open Game
open Grid
open Game_map
open Player

(* Test Plan: Most of the functions in grid.ml, game_map.ml and
   player.ml were automatically tested by OUnit. Some functions in those
   files were used to print messages in the terminal, and some execute
   based on a random number generated within the function. Those kind of
   functions were manually tested. We did not used OUnit to test the
   functions in bin/main.ml because they are either print functions or
   involve too much randomness when executing. Test cases were all
   developed following black box testting. All of the functions in
   command.ml were used in /bin/main.ml, so they were also indirectly
   tested when we did manual testing for \bin\main.ml

   To prove correctness, the OUnit test suites cover all the helper
   functions exposed in .mli files. Functions that run the game and
   involve more direct interactions with the players can be tested with
   manually putting in all the possible commands exhaustively*)

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

let rec print_map map =
  match map with
  | [] -> print_endline ""
  | h :: t ->
      print_endline (string_of_grid h);
      print_map t

(**[print_players_property properties] prints the basic information
   about [properties] *)
let rec print_player_property (properties : g list) =
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
      ^ "\n\n" ^ print_player_property t

let print_player p =
  print_endline
    ("The name of the player is " ^ get_player_name p
   ^ "\nThe position of the current grid is "
    ^ string_of_int (get_grid_index (get_player_curr_pos p))
    ^ "\nThe money balance of the player is "
    ^ string_of_float (get_player_balance p)
    ^ "\n The properties of the player are: \n "
    ^ print_player_property (get_player_properties p))

let print_players (players : player array) =
  for i = 0 to Array.length players - 1 do
    print_player players.(i)
  done

let get_grid_index_test
    (name : string)
    (input_grid : g)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output (get_grid_index input_grid)

let get_grid_name_test
    (name : string)
    (input_grid : g)
    (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output (get_grid_name input_grid)

let get_grid_type_test
    (name : string)
    (input_grid : g)
    (expected_output : grid_type) : test =
  name >:: fun _ ->
  assert_equal expected_output (get_grid_type input_grid)

let get_grid_toll_test
    (name : string)
    (input_grid : g)
    (expected_output : float) : test =
  name >:: fun _ ->
  assert_equal expected_output (get_grid_toll input_grid)

let get_grid_ownership_test
    (name : string)
    (input_grid : g)
    (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output (get_grid_ownership input_grid)

let get_grid_level_test
    (name : string)
    (input_grid : g)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output (get_grid_level input_grid)

let get_grid_price_test
    (name : string)
    (input_grid : g)
    (expected_output : float) : test =
  name >:: fun _ ->
  assert_equal expected_output (get_grid_price input_grid)

let set_grid_type_test
    (name : string)
    (input_grid : g)
    (input_type : grid_type)
    (expected_output : g) : test =
  name >:: fun _ ->
  assert_equal expected_output (set_grid_type input_grid input_type)

let get_owned_land_price_test
    (name : string)
    (input_land : owned_land)
    (expected_output : float) : test =
  name >:: fun _ ->
  assert_equal expected_output (get_owned_land_price input_land)

let string_of_grid_test
    (name : string)
    (input_grid : g)
    (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output (string_of_grid input_grid)

let grid_0 = set_grid "Carpenter Hall" 0 (Vacant_land 780.)

let grid_0' = set_grid "Carpenter Hall" 0 Jail

let grid_0'' =
  set_grid "Carpenter Hall" 0
    (Owned_land
       {
         land_price = 1200.;
         ownership = "Adam";
         toll = 120.;
         level = 0;
       })

let grid_1 = set_grid "Gates Hall" 1 (Vacant_land 2000.)

let grid_1' = set_grid "Gates Hall" 1 Hospital

let grid_2 = set_grid "Uris Hall" 2 (Vacant_land 3000.)

let grid_3 = set_grid "Rhodes Hall" 3 (Vacant_land 400.)

let grid_4 = set_grid "Olin Hall" 4 Draw_Card

let grid_4' = set_grid "Olin Hall" 4 (Vacant_land 1000.)

let grid_4'' =
  set_grid "Olin Hall" 4
    (Owned_land
       {
         land_price = 1200.;
         ownership = "Adam";
         toll = 120.;
         level = 0;
       })

let grid_0_owned =
  set_grid "Carpenter Hall" 0
    (Owned_land
       {
         land_price = 780.;
         ownership = "Goose";
         toll = 156.;
         level = 0;
       })

let grid_1_owned =
  set_grid "Gates Hall" 1
    (Owned_land
       {
         land_price = 2000.;
         ownership = "Goose";
         toll = 400.;
         level = 0;
       })

let grid_2_owned =
  set_grid "Uris Hall" 2
    (Owned_land
       {
         land_price = 3000.;
         ownership = "Goose";
         toll = 600.;
         level = 5;
       })

let grid_3_owned =
  set_grid "Rhodes Hall" 7
    (Owned_land
       { land_price = 4000.; ownership = "Cat"; toll = 800.; level = 3 })

let grid_1 = set_grid "Gates Hall" 1 (Vacant_land 2000.)

let grid_2 = set_grid "Uris Hall" 2 (Vacant_land 3000.)

let map_1_owned' = [ grid_0; grid_1; grid_2_owned ]

let map_1_owned'' = [ grid_0; grid_1; grid_2 ]

let map_1_owned = [ grid_0; grid_1_owned; grid_2_owned ]

let map_1_owned''' = [ grid_0_owned; grid_1_owned; grid_2_owned ]

let map_1_owned_dup = [ grid_0; grid_1_owned; grid_2_owned ]

let owned_land_1 =
  { land_price = 1000.; ownership = "Dog"; toll = 200.; level = 5 }

let owned_land_2 =
  { land_price = 300.; ownership = "Giraffe"; toll = 60.; level = 2 }

let grid_tests =
  [
    get_grid_index_test "grid 1's index is 0" grid_0 0;
    get_grid_name_test "grid 1's name is Carpenter Hall" grid_0
      "Carpenter Hall";
    get_grid_name_test "grid 2's name is Uris Hall" grid_2 "Uris Hall";
    get_grid_type_test "grid 1's grid type is Vacant_land 780." grid_0
      (Vacant_land 780.);
    get_grid_type_test "grid 1's grid type is Draw Card" grid_4
      Draw_Card;
    get_grid_type_test "grid 1''s grid type is Hospital" grid_1'
      Hospital;
    get_grid_toll_test "grid_1_owned's toll is 400." grid_1_owned 400.;
    get_grid_toll_test "grid_2_owned's toll is 600." grid_2_owned 600.;
    get_grid_ownership_test "grid_1_owned's owner is \"Goose\""
      grid_1_owned "Goose";
    get_grid_ownership_test "grid_2_owned's owner is \"Cat\""
      grid_3_owned "Cat";
    get_grid_level_test "grid_1_owned's level is 0" grid_1_owned 0;
    get_grid_level_test "grid_2_owned's level is 5" grid_2_owned 5;
    get_grid_level_test "grid_3_owned's level is 3" grid_3_owned 3;
    get_grid_price_test "grid_1_owned's price is 2000." grid_1_owned
      2000.;
    get_grid_price_test "grid_1_owned's price is 2000." grid_1_owned
      2000.;
    set_grid_type_test "grid_1's type is changed to Hospital" grid_1
      Hospital grid_1';
    set_grid_type_test "grid_4's type is chanegd to Vacant_land 1000."
      grid_4 (Vacant_land 1000.) grid_4';
    get_owned_land_price_test "owned_land_1's price is 1000."
      owned_land_1 1000.;
    get_owned_land_price_test "owned_land_2's price is 1000."
      owned_land_2 300.;
  ]

let map_1 = [ grid_0; grid_1; grid_2; grid_3 ]

let map_1' = [ grid_0; grid_1'; grid_2; grid_3 ]

let map_1'' = [ grid_0'; grid_1; grid_2; grid_3 ]

let map_2 = [ grid_0'; grid_1; grid_2; grid_3; grid_4 ]

let map_2' = [ grid_0'; grid_1; grid_2; grid_3; grid_4' ]

let map_2'' = [ grid_0'; grid_1; grid_2; grid_3; grid_4'' ]

let map_empty = []

let get_map_size_test
    (name : string)
    (input_map : map)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output (get_map_size input_map)

let update_grid_type_in_map_test
    (name : string)
    (input_map : map)
    (input_ntype : grid_type)
    (input_acc : map)
    (input_index : int)
    (expected_output : map) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (update_grid_type_in_map input_map input_ntype input_acc input_index)

let update_grid_type_in_empty_map_test
    (name : string)
    (input_map : map)
    (input_ntype : grid_type)
    (input_acc : map)
    (input_index : int) : test =
  name >:: fun _ ->
  assert_raises (Failure "Empty map") (fun () ->
      update_grid_type_in_map input_map input_ntype input_acc
        input_index)

let get_grid_by_index_test
    (name : string)
    (input_map : map)
    (input_index : int)
    (expected_output : g) : test =
  name >:: fun _ ->
  assert_equal expected_output (get_grid_by_index input_map input_index)

let get_grid_by_index_ob_test
    (name : string)
    (input_map : map)
    (input_index : int) : test =
  name >:: fun _ ->
  assert_raises
    (Failure "The index you input exceeds the size of the game map")
    (fun () -> get_grid_by_index input_map input_index)

let get_grid_by_index_neg_test
    (name : string)
    (input_map : map)
    (input_index : int) : test =
  name >:: fun _ ->
  assert_raises (Failure "The index you input is a negative number")
    (fun () -> get_grid_by_index input_map input_index)

let game_map_tests =
  [
    get_map_size_test "The size of map_1 is 4" map_1 4;
    get_map_size_test "The size of an empty map is 0" map_empty 0;
    update_grid_type_in_map_test
      "Update the type grid_1 of map_1 to Hospital" map_1 Hospital [] 1
      map_1';
    update_grid_type_in_map_test
      "Update the type of grid_0 of map_1 to Jail" map_1 Jail [] 0
      map_1'';
    update_grid_type_in_map_test
      "Update the type of grid_4 of map_2 to Vacant_land with a price \
       of 1000."
      map_2 (Vacant_land 1000.) [] 4 map_2';
    update_grid_type_in_map_test
      "Update the type of grid4' of map2 to Owned_land" map_2
      (Owned_land
         {
           land_price = 1200.;
           ownership = "Adam";
           toll = 120.;
           level = 0;
         })
      [] 4 map_2'';
    update_grid_type_in_empty_map_test
      "Cannot update a grid in an empty map" map_empty Hospital [] 0;
    get_grid_by_index_test "The grid in map_1 with index 0 is grid_0"
      map_1 0 grid_0;
    get_grid_by_index_test "The grid in map_2 with index 4 is grid_4"
      map_2 4 grid_4;
    get_grid_by_index_ob_test
      "Cannot get the grid with index 5 in a map of size 4" map_1 5;
    get_grid_by_index_ob_test
      "Cannot get the grid with index 4 in a map of size 4" map_1 4;
    get_grid_by_index_neg_test "Cannot get a grid with negative index"
      map_2 (-1);
  ]

let update_balance_properties_test
    (name : string)
    (input_player : player)
    (input_balance : float)
    (input_property : g)
    (expected_output : player) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (update_balance_properties input_player input_balance input_property)

(* let modify_money_test (name : string) (input_map : map)
   (input_player_index : int) (input_players : player array)
   (expected_output : player array): test = name >:: fun _ ->
   assert_equal expected_output (modify_money input_map
   input_player_index input_players) *)

let player_one =
  create_player "Goose" 5000. [ grid_1_owned; grid_2_owned ] grid_0

let player_1_1000_minus =
  create_player "Goose" 4000. [ grid_1_owned; grid_2_owned ] grid_0

let player_1_4998_minus =
  create_player "Goose" 2. [ grid_1_owned; grid_2_owned ] grid_0

let player_1_one_prop_off =
  create_player "Goose" 999. [ grid_2_owned ] grid_0

let player_one_1prop_more =
  create_player "Goose" 4220.
    [ grid_0_owned; grid_1_owned; grid_2_owned ]
    grid_0

let player_1_two_props_off = create_player "Goose" 499. [] grid_0

let player_two = create_player "JoCalf" 4000. [] grid_0

let player_arr_two = [| player_one; player_two |]

let player_arr_one = [| player_one |]

let player_arr_one_dup1 = [| player_one |]

let player_arr_one_dup2 = [| player_one |]

let player_arr_one_dup3 = [| player_one |]

let player_arr_one_1prop_off = [| player_1_one_prop_off |]

let player_arr_one_2props_off = [| player_1_two_props_off |]

let player_arr_one' = [| player_1_1000_minus |]

let player_arr_one'' = [| player_1_4998_minus |]

let create_player_test
    (name : string)
    (input_name : string)
    (input_balance : float)
    (input_property : g list)
    (input_curr_grid : g)
    (expected_output : player) =
  name >:: fun _ ->
  assert_equal expected_output
    (create_player input_name input_balance input_property
       input_curr_grid)

let get_player_name_test
    (name : string)
    (input_player : player)
    (expected_output : string) =
  name >:: fun _ ->
  assert_equal expected_output (get_player_name input_player)

let get_player_balance_test
    (name : string)
    (input_player : player)
    (expected_output : float) =
  name >:: fun _ ->
  assert_equal expected_output (get_player_balance input_player)

let get_player_properties_test
    (name : string)
    (input_player : player)
    (expected_output : g list) =
  name >:: fun _ ->
  assert_equal expected_output (get_player_properties input_player)

let get_player_curr_pos_test
    (name : string)
    (input_player : player)
    (expected_output : g) =
  name >:: fun _ ->
  assert_equal expected_output (get_player_curr_pos input_player)

let pos_mod_test
    (name : string)
    (input_int1 : int)
    (input_int2 : int)
    (expected_output : int) =
  name >:: fun _ ->
  assert_equal expected_output (positive_mod input_int1 input_int2)

let update_pos_test
    (name : string)
    (input_map : map)
    (input_player : player)
    (input_idx : int)
    (expected_output : player) =
  name >:: fun _ ->
  assert_equal expected_output
    (update_pos input_map input_player input_idx)

let buy_land_test
    (name : string)
    (player : player)
    (map : map)
    (grid_index : int)
    (expected_output : map * player) =
  name >:: fun _ ->
  assert_equal expected_output (buy_land player map grid_index)

let sell_land_test
    (name : string)
    (player : player)
    (map : map)
    (grid_index : int)
    (coef : float)
    (expected_output : map * player) =
  name >:: fun _ ->
  assert_equal expected_output (sell_land player map grid_index coef)

let upgrade_land_test
    (name : string)
    (player : player)
    (map : map)
    (grid_index : int)
    (expected_output : map * player) =
  name >:: fun _ ->
  assert_equal expected_output (upgrade_land player map grid_index)

let get_player_curr_index_test
    (name : string)
    (input_player : player)
    (expected_output : int) =
  name >:: fun _ ->
  assert_equal expected_output (get_player_curr_index input_player)

let update_balance_test
    (name : string)
    (input_player : player)
    input_balance
    (expected_output : player) =
  name >:: fun _ ->
  assert_equal expected_output
    (update_balance input_player input_balance)

let bankruptcy_lose_test
    (name : string)
    (pay_index : int)
    (players : player array)
    (map : map)
    (amount : float)
    (updated_players : player array) =
  name >:: fun _ ->
  let r = bankruptcy_lose 0 players map amount in
  print_players (snd r);
  (* print_map (fst r); *)
  assert_equal r (map, updated_players)

let bankruptcy_lose_test_updated_map
    (name : string)
    (pay_index : int)
    (players : player array)
    (map : map)
    (amount : float)
    (updated_map : map)
    (updated_players : player array) =
  name >:: fun _ ->
  let r = bankruptcy_lose 0 players map amount in
  print_players (snd r);
  (* print_map (fst r); *)
  assert_equal r (updated_map, updated_players)

let bankruptcy_toll_test
    (name : string)
    (pay_index : int)
    (recipient_index : int)
    (players : player array)
    (map : map)
    (amount : float)
    (expected_players : player array) =
  name >:: fun _ ->
  let r =
    bankruptcy_toll pay_index recipient_index players map amount
  in
  (* print_players (snd r); *)
  (* print_map (fst r); *)
  assert_equal r (map, expected_players)

let bankruptcy_toll_test_lose_prop
    (name : string)
    (pay_index : int)
    (recipient_index : int)
    (players : player array)
    (map : map)
    (amount : float)
    (expected_value : map * player array) =
  name >:: fun _ ->
  let r =
    bankruptcy_toll pay_index recipient_index players map amount
  in
  (* print_map (fst r); *)
  (* print_players (snd r); *)
  assert_equal r expected_value

let get_stay_test
    (name : string)
    (input_player : player)
    (expected_output : int) =
  name >:: fun _ -> assert_equal expected_output (get_stay input_player)

let update_stay_test
    (name : string)
    (input_player : player)
    (input_stay : int)
    (expected_output : player) =
  name >:: fun _ ->
  assert_equal expected_output (update_stay input_player input_stay)

let upgrade_land_test
    (name : string)
    (input_player : player)
    (input_map : map)
    (input_index : int)
    (expected_output : map * player) =
  name >:: fun _ ->
  assert_equal expected_output
    (upgrade_land input_player input_map input_index)

let grid_cat =
  set_grid "Olin Hall" 4
    (Owned_land
       { land_price = 1200.; ownership = "Cat"; toll = 120.; level = 0 })

let grid_cat_level_1 =
  set_grid "Olin Hall" 4
    (Owned_land
       { land_price = 1440.; ownership = "Cat"; toll = 288.; level = 1 })

let grid_cat_level_2 =
  set_grid "Olin Hall" 4
    (Owned_land
       {
         land_price = 1728.;
         ownership = "Cat";
         toll = 345.6;
         level = 2;
       })

let grid_no_cat = set_grid "Olin Hall" 4 (Vacant_land 1200.)

let map_2'' = [ grid_0; grid_1; grid_2; grid_3; grid_cat ]

let map_2_upgraded =
  [ grid_0; grid_1; grid_2; grid_3; grid_cat_level_1 ]

let map_2_upgraded_2 =
  [ grid_0; grid_1; grid_2; grid_3; grid_cat_level_2 ]

let map_2_without_prop = [ grid_0; grid_1; grid_2; grid_3; grid_no_cat ]

let player_have_property = create_player "Cat" 5000. [] grid_0

let player_1 = create_player "Puppy" 5000. [] grid_0

let player_1_no_mon = create_player "Puppy" 0. [] grid_0

let player_1' = create_player "Puppy" 4000. [] grid_0

let player_1'' = create_player "Puppy" 8000. [] grid_0

let player_1''' = create_player "Puppy" 5000. [ grid_1_owned ] grid_0

let player_1'''' = create_player "Puppy" 8100. [] grid_0

let player_2 = create_player "Cat" 2500. [ grid_cat ] grid_1

let player_2' = create_player "Cat" 3500. [ grid_cat ] grid_1

let player_2'' = create_player "Cat" 7500. [ grid_cat ] grid_1

let player_2_without_prop = create_player "Cat" 100. [] grid_1

let player_2_upgrade_land =
  create_player "Cat" 2260. [ grid_cat_level_1 ] grid_1

let player_2_upgrade_land_2 =
  create_player "Cat" 1972. [ grid_cat_level_2 ] grid_1

let player_2_in_jail =
  create_player_with_stay "Cat" 2500. [ grid_cat ] grid_1 3

let player_2_bankrupt =
  create_player_with_stay "Cat" 2500. [ grid_cat ] grid_1 (-1)

let player_2_bankrupt_true =
  create_player_with_stay "Cat" 0. [] grid_1 (-1)

let player_3 =
  create_player "Goose" 5000. [ grid_0_owned; grid_1_owned ] grid_1

let player_3_sold0 = create_player "Goose" 5780. [ grid_1_owned ] grid_1

let player_3_sold1 = create_player "Goose" 7000. [ grid_0_owned ] grid_1

let player_3_at2 =
  create_player "Goose" 5000. [ grid_0_owned; grid_1_owned ] grid_2

let map_3 = [ grid_0_owned; grid_1_owned; grid_2 ]

let map_3_sold0 = [ grid_0; grid_1_owned; grid_2 ]

let map_3_sold1 = [ grid_0_owned; grid_1; grid_2 ]

let player_arr_2 = [| player_1; player_2 |]

let player_arr_2_dup = [| player_1; player_2 |]

let player_arr_2_dup_2 = [| player_1; player_2 |]

let player_arr_2_dup_3 = [| player_1; player_2 |]

let player_arr_2' = [| player_1'; player_2' |]

let player_arr_2'_similar = [| player_1_no_mon; player_2'' |]

let player_arr_2'' = [| player_1''; player_2_without_prop |]

let player_arr_2''' = [| player_1''''; player_2_bankrupt_true |]

let player_tests =
  [
    get_stay_test "normal player's stay" player_2 0;
    get_stay_test "bankrupt player's stay" player_2_bankrupt (-1);
    get_stay_test "jail player's stay" player_2_in_jail 3;
    update_stay_test "update stay from 0 to 3" player_2 3
      player_2_in_jail;
    update_stay_test "update stay from 3 to 0" player_2_in_jail 0
      player_2;
    update_stay_test "update stay from 0 to -1" player_2 (-1)
      player_2_bankrupt;
    update_balance_properties_test
      "Update player_1 balance to 5000. and properties to grid_0"
      player_1 5000. grid_1_owned player_1''';
    bankruptcy_lose_test "[player_one] loses 1000$" 0 player_arr_one
      map_1 1000. player_arr_one';
    bankruptcy_lose_test "[player_one] loses 4998$" 0
      player_arr_one_dup1 map_1 4998. player_arr_one'';
    bankruptcy_lose_test_updated_map
      "[player_one] loses 5001$, triggers property trade" 0
      player_arr_one_dup2 map_1_owned 5001. map_1_owned'
      player_arr_one_1prop_off;
    bankruptcy_lose_test_updated_map
      "[player_one] loses 7001$, triggers property trade" 0
      player_arr_one_dup3 map_1_owned_dup 7001. map_1_owned''
      player_arr_one_2props_off;
    buy_land_test "[player_one] spends 780$ to buy [grid_0]" player_one
      map_1_owned 0
      (map_1_owned''', player_one_1prop_more);
    sell_land_test "[player_3] sold grid_0_owned" player_3 map_3 0 1.
      (map_3_sold0, player_3_sold0);
    sell_land_test "[player_3] sold grid_1_owned" player_3 map_3 1 1.
      (map_3_sold1, player_3_sold1);
    (* sell_land_test "[player_one] sells the owned [grid_1]" player_one
       map_1_owned 0 (map_1_owned''', player_one_1prop_less); *)
    bankruptcy_toll_test "[player_1] pays toll of 1000 to [player_2]" 0
      1 player_arr_2 map_1 1000. player_arr_2';
    bankruptcy_toll_test "[player_1] pays toll of 5000 to [player_2]" 0
      1 player_arr_2_dup map_2'' 5000. player_arr_2'_similar;
    bankruptcy_toll_test_lose_prop
      "[player_2] pays toll of 3000 to [player_1]" 1 0
      player_arr_2_dup_2 map_2'' 3000.
      (map_2_without_prop, player_arr_2'');
    bankruptcy_toll_test_lose_prop
      "[player_2] pays toll of 5500 to [player_1]" 1 0
      player_arr_2_dup_3 map_2'' 5500.
      (map_2_without_prop, player_arr_2''');
    upgrade_land_test "upgrade [player_2]'s grid_cat to level 1"
      player_2 map_2'' 4
      (map_2_upgraded, player_2_upgrade_land);
    upgrade_land_test "upgrade [player_2]'s grid_cat to level 2"
      player_2_upgrade_land map_2_upgraded 4
      (map_2_upgraded_2, player_2_upgrade_land_2);
    get_player_curr_index_test "[player_1]'s current grid index is 0"
      player_1 0;
    get_player_curr_index_test "[player_3]'s current grid index is 1"
      player_3 1;
    update_pos_test "[player_3]'s position is upgraded to grid_2" map_2
      player_3 1 player_3_at2;
    update_balance_test "[player_2]'s balance is changed to 3500"
      player_2 3500. player_2';
    get_player_curr_pos_test "[player_one]'s current position is 0"
      player_one grid_0;
    get_player_properties_test
      "[player_one] has [grid_1] and [grid_2] as property " player_one
      [ grid_1_owned; grid_2_owned ];
    get_player_balance_test "[player_one] has balance of 5000$"
      player_one 5000.;
    get_player_name_test "The name is [player_one] is Goose" player_one
      "Goose";
  ]

let suite =
  "test suite for the Final Project"
  >::: List.flatten [ grid_tests; game_map_tests; player_tests ]

let _ = run_test_tt_main suite
