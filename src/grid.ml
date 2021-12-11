type owned_land = {
  land_price : float;
  ownership : string;
  toll : float;
  level : int;
}

type grid_type =
  | Vacant_land of float
  | Owned_land of owned_land
  | Hospital
  | Jail
  | Draw_Card

type grid_name = string

type grid_index = int

type g = {
  grid_name : grid_name;
  grid_index : grid_index;
  grid_type : grid_type;
}

let set_grid
    (grid_name : grid_name)
    (grid_index : grid_index)
    (grid_type : grid_type) =
  { grid_name; grid_index; grid_type }

let get_grid_toll gr =
  match gr.grid_type with
  | Owned_land ol -> ol.toll
  | _ -> failwith "Does not have a toll"

let get_grid_index gr = gr.grid_index

let get_grid_name gr = gr.grid_name

let get_grid_type gr = gr.grid_type

let get_owned_land_price l = l.land_price

let get_grid_price gr =
  match gr.grid_type with
  | Owned_land ol -> ol.land_price
  | Vacant_land p -> p
  | _ -> failwith "Does not have a price"

let get_grid_ownership g =
  match g.grid_type with
  | Owned_land ol -> ol.ownership
  | _ -> failwith "Does not have an owner"

let get_grid_level g =
  match g.grid_type with
  | Owned_land ol -> ol.level
  | _ ->
      failwith
        "This is not an owned land: doesn't have a upgraded level"

let string_of_grid g =
  "The index of the grid is "
  ^ string_of_int g.grid_index
  ^ "\n" ^ "This grid is a(an) "
  ^
  match g.grid_type with
  | Vacant_land p -> "vacant land with price " ^ string_of_float p
  | Owned_land { land_price; ownership; toll } ->
      "owned land with price "
      ^ string_of_float land_price
      ^ " owned by " ^ ownership ^ " and with toll at "
      ^ string_of_float toll ^ " dollars " ^ "at a level of "
      ^ string_of_int (get_grid_level g)
  | Hospital -> "hospital."
  | Jail -> "jail."
  | Draw_Card ->
      "place where players can draw the card to get involved in a \
       random event"

let set_grid_type h new_grid_type = { h with grid_type = new_grid_type }