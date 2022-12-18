(* Game: current game status in Monopoly game *)

(* Representation of the player list type *)
type players

(* Representation of the gameboard type *)
type gameboard

(* Abstract representation of the game type *)
type t

(* [create_gameboard pp_list] creates a gameboard with the specific property list *)
val create_gameboard: Property.t list -> gameboard

(* [create_game gameboard] creats a game *)
val create_game: gameboard -> t

(* 
   HELPER 
*)

(* [shuffle_deck] shuffles the chance and community chest decks *)
val shuffle_deck: unit -> int list

(* [get_deck game deck] gets the specific [deck] *)
val get_deck: t -> string -> int list

(* [set_deck game list deck] sets the specific [deck] as [list] *)
val set_deck: t -> int list -> string -> unit

(* [move_front_to_back list] moves the head element of [list] to the tail *)
val move_front_to_back: 'a list -> 'a list

(* [move_to_next_player game] moves to the next player *)
val move_to_next_player: t -> unit

(* [get_player_list game] gets the player list *)
val get_player_list: t -> Player.t list

(* [add_player game player] adds [player] to the game *)
val add_player: t -> Player.t -> unit

(* [get_property_by_name game name] gets the specific property by [name] *)
val get_property_by_name: t -> string -> Property.t

(* [start_positioin game] gets the start position of the gameboard *)
val start_position: t -> Property.t

(* [players_setup game num_ai] setups the game with [num_ai] AI players *)
val players_setup: t -> int -> unit

(* [num_players game] gets the number of players *)
val num_players: t -> int

(* [get_player_by_name game name] gets the specific player by [name] *)
val get_player_by_name: t -> string -> Player.t

(* [get_property_owner game pp] gets the owner of [pp] *)
val get_property_owner: t -> Property.t -> Player.t

(* [get_jail game] returns the jail space *)
val get_jail: t -> Property.t

(* [get_current_player game] gets the current player *)
val get_current_player: t -> Player.t

(* 
   MAIN 
*)

(* [only_one_player game] checks if there is only one player left *)
val only_one_player: t -> bool

(* [in_game game player] checks if [player] is in game *)
val in_game: t -> Player.t -> bool

(* [get_property_index game pp] gets the index of [pp] *)
val get_property_index: t -> Property.t -> int

(* [get_property_at_index game index] gets the property at [index] *)
val get_property_at_index: t -> int -> Property.t

(* [pay_rent game player pp money] pays the rent for [pp]. [money] is the number of rent. *)
val pay_rent: t -> Player.t -> Property.t -> int -> unit

(* [remove_player game player] removes [player] from the game *)
val remove_player: t -> Player.t -> unit

(* [forfeit game player] removes a player from the game when [player] owes money to the bank *)
val forfeit: t -> Player.t -> unit

(* [bankruptcy game player pp] makes the player in bankruptcy status and remove it from the game *)
val bankruptcy: t -> Player.t -> Property.t -> unit

(* [move_by_steps game player steps flag] moves the player by [steps]. [flag] is for checking if the player
   is allowed to collect $200 from passing through Pass Go. *)
val move_by_steps: t -> Player.t -> int -> bool -> unit

(* [arrested game player] arrests a player *)
val arrested: t -> Player.t -> unit