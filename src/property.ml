type property_type =
  | Go
  | CommunityChest
  | Tax
  | Chance
  | Jail
  | FreeParking
  | GoToJail
  | Brown
  | LightBlue
  | Pink
  | Orange
  | Red
  | Yellow
  | Green
  | DarkBlue
  | RailRoad
  | Utilities

type property_level =
  | Unbuyable
  | Other
  | Zero
  | One 
  | Two 
  | Three 
  | Four 
  | Hotel

type t =
  {
    name: string;
    t_type: property_type;
    price: int;
    rent: (property_level * int) list;
    house_cost: int;
    mutable owner: string;
    mutable level: property_level;
    mutable mortgaged: bool
  }

let is_buyable_type = function
  | Brown 
  | LightBlue 
  | Pink 
  | Orange 
  | Red 
  | Yellow 
  | Green 
  | DarkBlue
  | RailRoad 
  | Utilities -> true
  | _ -> false

let is_property pp = is_buyable_type pp.t_type

let can_be_upgraded pp = 
  match pp.level with
  | Unbuyable
  | Other 
  | Hotel -> false
  | _ -> true

let can_be_upgraded_type = function
  | Brown
  | LightBlue
  | Pink
  | Orange
  | Red
  | Yellow
  | Green
  | DarkBlue -> true
  | _ -> false

let string_to_property_type = function
  | "go" -> Go
  | "community chest" -> CommunityChest
  | "tax" -> Tax
  | "chance" -> Chance
  | "jail" -> Jail
  | "free parking" -> FreeParking
  | "go to jail" -> GoToJail
  | "brown" -> Brown
  | "light blue" -> LightBlue
  | "pink" -> Pink
  | "orange" -> Orange
  | "red" -> Red
  | "yellow" -> Yellow
  | "green" -> Green
  | "dark blue" -> DarkBlue
  | "railroad" -> RailRoad
  | "utilities" -> Utilities
  | _ -> failwith "Invalid Argument"

let is_owned pp = pp.owner <> ""

let is_tax pp = 
  match pp.t_type with
  | Tax -> true
  | _ -> false

let is_utilities pp =
  match pp.t_type with
  | Utilities -> true
  | _ -> false

let is_railroad pp =
  match pp.t_type with
  | RailRoad -> true
  | _ -> false

let is_jail pp =
  match pp.t_type with
  | Jail -> true
  | _ -> false

let is_go_to_jail pp =
  match pp.t_type with
  | GoToJail -> true
  | _ -> false

let is_chance pp =
  match pp.t_type with
  | Chance -> true
  | _ -> false

let is_community_chest pp =
  match pp.t_type with
  | CommunityChest -> true
  | _ -> false

let create_t name' t_type' price' rent' house_cost' =
  {
    name = name';
    t_type = t_type';
    price = price';
    rent = rent';
    house_cost = house_cost';
    owner = "";
    level = 
        (if is_buyable_type t_type' then
          if can_be_upgraded_type t_type' then Zero
          else Other
        else Unbuyable);
    mortgaged = false
  }

let create_rent_list (arr: int array) = function
  | Brown
  | LightBlue
  | Pink
  | Orange
  | Red
  | Yellow
  | Green
  | DarkBlue ->
    [
      (Zero, arr.(0));
      (One, arr.(1));
      (Two, arr.(2));
      (Three, arr.(3));
      (Four, arr.(4));
      (Hotel, arr.(5));
    ]
  | RailRoad
  | Utilities ->
    [(Other, arr.(0))]
  | _ -> [(Unbuyable, arr.(0))]

let create_property name t_type_string price rent house_cost = 
  let t_type = string_to_property_type t_type_string in
  create_t name t_type price (create_rent_list rent t_type) house_cost

let calculate_rent pp = List.assoc pp.level pp.rent

let upgrade_property pp =
  match pp.level with
  | Zero -> pp.level <- One
  | One -> pp.level <- Two
  | Two -> pp.level <- Three
  | Three -> pp.level <- Four
  | Four -> pp.level <- Hotel
  | Unbuyable
  | Other
  | Hotel -> ()

let downgrade_property pp =
  match pp.level with
  | One -> pp.level <- Zero
  | Two -> pp.level <- One
  | Three -> pp.level <- Two
  | Four -> pp.level <- Three
  | Hotel -> pp.level <- Four
  | Unbuyable
  | Other
  | Zero -> ()

let num_for_monopoly pp =
  match pp.t_type with
  | Brown
  | DarkBlue -> 2
  | LightBlue
  | Pink
  | Orange
  | Red
  | Yellow
  | Green -> 3
  | _ -> -1

let num_houses pp =
  match pp.level with
  | Unbuyable
  | Other
  | Zero -> 0
  | One -> 1
  | Two -> 2
  | Three -> 3
  | Four -> 4
  | Hotel -> 5

let reset_level pp =
  match pp.level with
  | One
  | Two
  | Three
  | Four
  | Hotel -> pp.level <- Zero
  | Unbuyable
  | Other
  | Zero -> ()

let release_property pp =
  pp.owner <- "";
  pp.mortgaged <- false

(* HELPER *)
let get_name pp = pp.name

let get_type pp = pp.t_type

let get_price pp = pp.price

let get_house_cost pp = pp.house_cost

let get_owner pp = pp.owner

let set_owner pp name = pp.owner <- name

let get_mortgaged_price pp = pp.price / 2

let get_value pp = if pp.mortgaged then 0 else get_mortgaged_price pp

let is_mortgaged pp = pp.mortgaged

let set_mortgaged pp = pp.mortgaged <- true

let unset_mortgaged pp = pp.mortgaged <- false
