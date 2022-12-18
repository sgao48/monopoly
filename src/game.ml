open Property
open Player
open Spectrum

type players = Player.t list

type gameboard = Property.t list

type t =
  {
    board: gameboard;
    mutable player_list: players;
    mutable current_player: string;
    mutable chance_deck: int list;
    mutable community_chest_deck: int list
  }

let create_gameboard (l : Property.t list) : gameboard = l

let rec shuffle_deck_helper num ans =
  if num = 0 then ans
  else
    shuffle_deck_helper (num - 1) ((num - 1) :: ans)

let shuffle_deck () =
  Random.self_init ();
  let num_cards = 16 in 
  let deck = shuffle_deck_helper num_cards [] in 
  let deck_with_weights = List.map (fun x -> (Random.bits (), x)) deck in 
  let shuffled_deck = List.sort compare deck_with_weights in 
  List.map (fun x -> snd x) shuffled_deck

let get_deck game deck_type =
  match deck_type with
  | "chance" -> game.chance_deck
  | _ -> game.community_chest_deck

let set_deck game deck deck_type =
  match deck_type with
  | "chance" -> game.chance_deck <- deck
  | _ -> game.community_chest_deck <- deck

let create_game board' =
  {
    board = board';
    player_list = [];
    current_player = "";
    chance_deck = shuffle_deck ();
    community_chest_deck = shuffle_deck ()
  }

(* HELPER *)
let move_front_to_back l =
  match l with
  | [] -> []
  | hd :: tl -> tl @ [hd]

let move_to_next_player game =
  game.player_list <- move_front_to_back game.player_list;
  game.current_player <- get_player_name @@ List.hd game.player_list

let get_player_list game = game.player_list

let add_player game player =
  game.player_list <- player :: game.player_list

let get_property_by_name game property_name =
  List.find (fun x -> get_name x = property_name) game.board

let start_position game = List.hd game.board

[@@@coverage off]
let choose_first_player game =
  Random.self_init ();
  let id = Random.int 4 in 
  let i = ref (id - 1) in
  while !i >= 0 do 
    game.player_list <- move_front_to_back game.player_list;
    i := !i - 1;
  done;
  game.current_player <- get_player_name @@ List.hd game.player_list

let players_setup game num_ai =
  for i = 1 to num_ai do 
    let ai = create_player false ("AI " ^ string_of_int i) (start_position game) in 
    add_player game ai;
  done;
  for i = 1 to (4 - num_ai) do 
    let player = create_player true ("Player " ^ string_of_int i) (start_position game) in 
    add_player game player;
  done;
  choose_first_player game

[@@@coverage on]
let num_players game = List.length game.player_list

let get_player_by_name game player_name =
  List.find (fun x -> get_player_name x = player_name) game.player_list

let get_property_owner game property =
  get_player_by_name game (get_owner property)

let get_jail game = 
  List.find (fun x -> is_jail x) game.board

let get_current_player game =
  List.find (fun x -> get_player_name x = game.current_player) game.player_list

(* MAIN *)
let only_one_player game = List.length game.player_list = 1

let in_game game player =
  List.mem player game.player_list

let rec get_property_index_helper properties property id =
  match properties with
  | [] -> raise Not_found
  | hd :: tl ->
    if hd = property then id
    else get_property_index_helper tl property (id + 1)

let get_property_index game property =
  get_property_index_helper game.board property 0
  
let rec get_property_at_index_helper properties id =
  match properties with
  | [] -> raise Not_found
  | hd :: tl ->
    if id = 0 then hd
    else get_property_at_index_helper tl (id - 1)

let get_property_at_index game id =
  get_property_at_index_helper game.board id
  
[@@@coverage off]
let pay_rent game player property rent = 
  Simple.printf "You have to pay $%d for the rent. ↩" rent;
  let _ = read_line () in ();
  update_money player (-rent);
  (if is_owned property then update_money (get_property_owner game property) rent);
  Simple.printf "You have paid the rent. Your current money is $%d. ↩" (get_money player);
  let _ = read_line () in ()

[@@@coverage on]
let remove_player game player =
  let player_name = get_player_name player in 
  if player_name = game.current_player then move_to_next_player game;
  game.player_list <- List.filter (fun x -> get_player_name x <> player_name) game.player_list

[@@@coverage off]
let forfeit game player =
  return_all_properties player;
  remove_player game player;
  Simple.printf "You gave all your assets back to the bank. ↩";
  let _ = read_line () in ()
  
let bankruptcy game player property =
  if is_owned property then
    (
      let owner = get_property_owner game property in 
      update_money owner (get_money player);
      give_all_properties player owner;
      remove_player game player;
      Simple.printf "You gave all your assets to %s. ↩" (get_player_name owner);
      let _ = read_line () in ()
    )
  else forfeit game player

[@@@coverage on]
let move_by_steps game player steps collect =
  let prev = get_position player in 
  let prev_id = get_property_index game prev in 
  let next_id = (prev_id + steps) mod 40 in 
  let next = get_property_at_index game next_id in 
  move_by_position player next;
  Simple.printf "Your current position is %s.\n" (get_name @@ get_position player);
  (if next_id < prev_id && collect then
    (update_money player 200;
    print_endline "Congratulations! You passed over Pass Go and collected $200 from bank."))

let arrested game player =
  move_to_jail player;
  move_by_position player (get_jail game)
