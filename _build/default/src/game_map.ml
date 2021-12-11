open Grid

type map = g list

let get_map_size map = List.length map

let rec update_grid_type_in_map game_map new_grid_type acc edited_index
    =
  match game_map with
  | [] -> failwith "Empty map"
  | h :: t ->
      if get_grid_index h = edited_index then
        acc @ [ set_grid_type h new_grid_type ] @ t
      else
        update_grid_type_in_map t new_grid_type
          (acc @ [ h ])
          edited_index

let get_grid_by_index game_map index =
  try List.nth game_map index with
  | Failure s ->
      failwith "The index you input exceeds the size of the game map"
  | Invalid_argument s ->
      failwith "The index you input is a negative number"
