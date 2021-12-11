type owned_land = {
  land_price : float;
  ownership : string;
  toll : float;
  level : int;
}
(**the abstract data type of owned_land*)

type grid_type =
  | Vacant_land of float
  | Owned_land of owned_land
  | Hospital
  | Jail
  | Draw_Card

(** the abstract data type for grid_type *)

type grid_name = string
(**type synonym for grid_name*)

type grid_index = int
(**type synonym for grid_index*)

type g
(** The abstract type of values representing grid*)

val set_grid : grid_name -> grid_index -> grid_type -> g
(** [set_grid grid_name grid_index grid_type] initiates a new grid*)

val get_grid_index : g -> grid_index
(**[get_grid_index gr] is the index of [gr]*)

val get_grid_name : g -> grid_name
(**[get_grid_name gr] is the name of [gr]*)

val get_grid_type : g -> grid_type
(**[get_grid_type gr] is the grid type of [gr]*)

val get_grid_toll : g -> float
(**[get_grid_toll gr] is the toll of [gr]*)

val get_grid_ownership : g -> string
(**[get_grid_ownership g] returns the name of the player who owns this
   grid of type [Owned_land]; if the grid has a different type, failure
   raised*)

val get_grid_level : g -> int
(**[get_grid_level g] is the level of [g] if [g] is a owned land,
   otherwise, it fails*)

val get_grid_price : g -> float
(**[get_grid_price gr] is the grid price of [gr]*)

val set_grid_type : g -> grid_type -> g
(**[set_grid_type h new_grid_type] is [h] with grid_type changed to
   [new_grid_type]*)

val get_owned_land_price : owned_land -> float
(**[get_owned_land_price l] is the land price of [l]*)

val string_of_grid : g -> string
(**[string_of_grid g] print information about [g]*)