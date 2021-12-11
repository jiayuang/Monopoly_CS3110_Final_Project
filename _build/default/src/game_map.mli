open Grid

type map = g list
(** The type of values representing map*)

val get_map_size : map -> int
(**[get_map_size map] is the size of [map]*)

val update_grid_type_in_map : map -> grid_type -> map -> int -> map
(**[update_grid_type_in_map game_map new_grid_type acc edited_index] is
   the updated map with type of the grid at [edited_index] in [game_map]
   updated to [new_grid_type]*)

val get_grid_by_index : map -> int -> g
(**[get_grid_by_index game_map index] is the grid at [index] *)