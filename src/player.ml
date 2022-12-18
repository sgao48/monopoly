open Property
open Spectrum

type t =
  {
    name: string;
    is_real: bool;
    mutable money: int;
    mutable position: Property.t;
    mutable properties: Property.t list;
    mutable monopolies: Property.property_type list;
    mutable doubles: int;
    mutable jailed_turns: int;
    mutable jail_free_card: string list;
  }

let create_player is_real' name' position' = 
  {
    name = name';
    is_real = is_real';
    money = 1500;
    position = position';
    properties = [];
    monopolies = [];
    doubles = 0;
    jailed_turns = 0;
    jail_free_card = [];
  }

let roll_dice () =
  Random.self_init ();
  (Random.int 6 + 1, Random.int 6 + 1)

let is_doubles dice =
  if fst dice = snd dice then true
  else false

let sum_dices dice =
  fst dice + snd dice

let reset_doubles player = player.doubles <- 0

let is_in_jail player = player.jailed_turns > 0

let move_to_jail player = player.jailed_turns <- 3

let out_of_jail player = player.jailed_turns <- 0

let next_jailed_turn player = 
  player.jailed_turns <- player.jailed_turns - 1

let move_by_position player next_position =
  player.position <- next_position

let update_money player money =
  player.money <- player.money + money

let add_monopoly player pp_type =
  player.monopolies <- pp_type :: player.monopolies

let count_owned_properties player property =
  player.properties
  |> List.filter (fun x -> get_type x = get_type property)
  |> List.length

let check_monopoly player property =
  let num_required = num_for_monopoly property in 
  let num_owned = count_owned_properties player property in 
  if num_owned + 1 = num_required then 
    add_monopoly player (get_type property)
  else ()

let add_property player property = 
  (if can_be_upgraded_type (get_type property) then
    check_monopoly player property);
  player.properties <- property :: player.properties

let remove_property player property =
  player.properties <- List.filter (fun x -> x <> property) player.properties

let buy_property player property = 
  let price = get_price property in 
  update_money player (-price);
  add_property player property;
  set_owner property player.name

let num_of_railroad player = 
  player.properties
  |> List.filter (fun x -> is_railroad x)
  |> List.length

let railroad_ratio player =
  int_of_float (2. ** float_of_int (num_of_railroad player - 1))

[@@@coverage off]
let num_of_utilities player =
  player.properties
  |> List.filter (fun x -> is_utilities x)
  |> List.length

let utilities_rent player = 
  let num = num_of_utilities player in 
  let dices = roll_dice () in
  Simple.printf "The result of the dice roll is %d and %d. This is for paying for the rent of utilities.\n" (fst dices) (snd dices);
  let points = sum_dices dices in 
  if num = 1 then 4 * points
  else 10 * points

[@@@coverage on]
let is_monopolied player property =
  player.monopolies
  |> List.exists (fun x -> x = (get_type property))

let rent_to_pay property owner =
  if is_utilities property then utilities_rent owner
  else
    let rent = calculate_rent property in 
    if is_railroad property then 
      rent * (railroad_ratio owner)
    else if is_monopolied owner property && num_houses property = 0 then rent * 2
    else rent

let is_built_evenly_helper house flag houses =
  let min = List.hd houses in 
  let max = houses |> List.rev |> List.hd in 
  if max - min <= 1 then
    house = if flag then min else max
  else false

let is_built_evenly player property flag =
  player.properties
  |> List.filter (fun x -> get_type x = get_type property)
  |> List.map num_houses
  |> List.sort Int.compare
  |> is_built_evenly_helper (num_houses property) flag

let is_buildable player property = 
  can_be_upgraded_type @@ get_type property
  && can_be_upgraded property
  && is_monopolied player property
  && is_built_evenly player property true
  && player.money >= get_house_cost property

let buidable_properties player =
  player.properties
  |> List.filter (fun x -> is_buildable player x)

let rec total_assets_helper properties ans =
  match properties with
  | [] -> ans
  | hd :: tl ->
    total_assets_helper tl (ans + get_value hd + (num_houses hd * get_house_cost hd / 2))

let total_assets player =
  total_assets_helper player.properties 0

let no_house_on_monopoly player property =
  let monopoly =
    player.properties
    |> List.filter (fun x -> get_type x = get_type property)
    |> List.map num_houses
  in 
  match monopoly with
  | [] -> true
  | l -> not @@ List.exists (fun x -> x > 0) l

let is_owner player property =
  List.mem property player.properties

let is_mortgageable player property =
  is_owner player property
  && not @@ is_mortgaged property
  && (num_houses property = 0)

let mortgageable_properties player =
  player.properties
  |> List.filter (fun x -> is_mortgageable player x)

let unmortgageable_properties player =
  player.properties
  |> List.filter (fun x -> is_mortgaged x)

let return_to_bank player property = 
  remove_property player property;
  release_property property;
  reset_level property

let rec return_all_properties_helper player properties =
  match properties with
  | [] -> ()
  | hd :: tl ->
    return_to_bank player hd;
    return_all_properties_helper player tl

let return_all_properties player =
  return_all_properties_helper player player.properties

let swap_owner giver receiver property =
  remove_property giver property;
  set_owner property receiver.name;
  add_property receiver property

let rec give_all_properties_helper giver receiver properties =
  match properties with
  | [] -> ()
  | hd :: tl -> 
    swap_owner giver receiver hd;
    give_all_properties_helper giver receiver tl

let give_all_properties giver receiver =
  give_all_properties_helper giver receiver giver.properties

let is_destroyable player property =
  is_owner player property
  && num_houses property > 0
  && is_built_evenly player property false

let destroyable_properties player =
  player.properties
  |> List.filter (fun x -> is_destroyable player x)

(* HELPER *)
let get_jail_free_card player = player.jail_free_card

let num_jail_free_card player = List.length player.jail_free_card

let add_jail_free_card player deck =
  player.jail_free_card <- deck :: player.jail_free_card

let remove_jail_free_card player =
  match player.jail_free_card with
  | [] -> ()
  | _ :: tl -> player.jail_free_card <- tl

let get_player_name player = player.name

let get_position player = player.position

let get_doubles player = player.doubles

let add_doubles player = player.doubles <- player.doubles + 1

let get_money player = player.money

let get_properties player = player.properties

let get_monopolies player = player.monopolies

let get_jailed_turns player = player.jailed_turns

let get_is_real player = player.is_real