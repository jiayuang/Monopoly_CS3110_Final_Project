open Grid
open Game_map

type player
(** The abstract type of values representing player*)

val create_player : string -> float -> g list -> g -> player
(**[create_player name balance properties curr_grid] is a new player
   with player_name = [name], balance = [balance], properties =
   [properties], curr_grid = [curr_grid], stay = 0*)

val create_player_with_stay :
  string -> float -> g list -> g -> int -> player
(**[create_player name balance properties curr_grid stay] is a new
   player with player_name = [name], balance = [balance], properties =
   [properties], curr_grid = [curr_grid], stay = [stay]*)

val get_player_name : player -> string
(**[get_player_name player] is the name of [player]*)

val get_player_balance : player -> float
(**[get_player_balance player] is the balance of [player]*)

val get_player_properties : player -> g list
(**[get_player_properties player] is the properties of [player]*)

val get_player_curr_pos : player -> g
(**[get_player_curr_pos player] is the current grid of [player]*)

val update_balance : player -> float -> player
(**[update_balance player new_balance] evaluates to a new player whose
   balance is changed to [new_balance]*)

val update_balance_properties : player -> float -> g -> player
(** [update_balance_properties player new_balance new_property]
    evaluates to a new player with the properties: add new_property in
    to player's properties by [new_property] and replace balance by
    [new_balance]*)

val positive_mod : int -> int -> int
(** [positive_mod index length] is index mod length with wrap-around*)

val update_pos : map -> player -> int -> player
(** [update_pos game_map player steps] evaluates to [player] whose index
    in [game_map] is increased by [steps]*)

val buy_land : player -> map -> int -> map * player
(** [buy_land player map grid_index] evaluates to the updated map and
    players who buy land at [grid_index]. It fails when [player] does
    not have enough balance or the land is not vacant*)

val sell_land : player -> map -> int -> float -> map * player
(** [sell_land player map grid_index] evalutes to the updated map and
    players who sell property at [grid_index] at a coefficient of
    [coefficient]*)

val upgrade_land : player -> map -> int -> map * player
(** [upgrade_land player map grid_index] upgrade the grid whose index is
    grid_index if the grid is a property of player and the player has
    enough balance to upgrade; and return the update map and player as a
    tuple *)

val get_player_curr_index : player -> int
(** [get_player_curr_index player] is the current position (grid index)
    of [player]*)

val get_stay : player -> int
(** [get_stay player] is the [stay] of [player]*)

val update_stay : player -> int -> player
(** [update_stay player new_time] is [player] with stay changed to
    [new_time]*)

val bankruptcy_toll :
  int -> int -> player array -> map -> float -> map * player array
(** [bankruptcy_toll payer_index recipient_index map toll] process the
    player at [payer_index] paying the player at [recipient_index]
    [toll]. It evaluates to a tuple of an updated map and a player array
    with updated balance for each the payer and recipient. If the payer
    does not have enough balance, his/her properties is sold to
    compensate If all of the payer's properties is sold and still canot
    pay, the payer goes bankrupt*)

val bankruptcy_lose :
  int -> player array -> map -> float -> map * player array
(** [bankruptcy_toll payer_index map amount] process the player at
    [payer_index] losing [amount]. It evaluates to a tuple of an updated
    map and a player array with updated payer balance. If the payer does
    not have enough balance, his/her properties is sold to compensate.
    If all of the payer's properties is sold and still canot pay, the
    payer goes bankrupt.*)

val modify_money : map -> int -> player array -> player array
(** [modify_money map player] randomly adds or minus a certain amount of
    money from [player]. The player might go bankrupt.*)

val draw_card : int -> player array -> map -> map * player array
(** [draw_card player_index players map] draws a random card for player
    who has [player_index] in [players] and evalutes to updated map and
    players*)
