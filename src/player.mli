(* Player: a player in Monopoly game *)

(* Abstract representation of the player type *)
type t

(* 
   MAIN 
*)

(* [create_player is_real name start] creates a player *)
val create_player: bool -> string -> Property.t -> t

(* [roll_dice] rolls two dices *)
val roll_dice: unit -> (int * int)

(* [is_doubles dice] checks if [dice] is doubles *)
val is_doubles: (int * int) -> bool

(* [sum_dices dice] sums up the points of [dice] *)
val sum_dices: (int * int) -> int

(* [reset_doubles player] resets the player's number of doubles as 0 *)
val reset_doubles: t -> unit

(* [is_in_jail player] checks if the player is in jail *)
val is_in_jail: t -> bool

(* [move_to_jail player] moves a player to the jail *)
val move_to_jail: t -> unit

(* [out_of_jail player] moves a player out of the jail *)
val out_of_jail: t -> unit

(* [next_jailed_turn player] starts the next jailed turn of the player *)
val next_jailed_turn: t -> unit

(* [move_by_position player pp] moves a player by the specific property *)
val move_by_position: t -> Property.t -> unit

(* [update_money player money] updates the player's money by [money] *)
val update_money: t -> int -> unit

(* [add_monopoly player type] adds [type] to the player's monopoly list *)
val add_monopoly: t -> Property.property_type -> unit

(* [count_owned_properties player pp] counts the number of owned properties with the same color as [pp] *)
val count_owned_properties: t -> Property.t -> int

(* [check_monopoly player pp] checks if the player satisfies the monopoly condition after owning [pp] *)
val check_monopoly: t-> Property.t -> unit

(* [is_monopolied player pp] checks if [pp] color group is monopolied by the player *)
val is_monopolied: t -> Property.t -> bool

(* [add_property player pp] adds [pp] to the player's property list *)
val add_property: t -> Property.t -> unit

(* [remove_property player pp] removes [pp] from the player's property list *)
val remove_property: t -> Property.t -> unit

(* [buy_property player pp] buys [pp] for the player *)
val buy_property: t -> Property.t -> unit

(* [rent_to_pay pp owner] calculates the rent needed to pay *)
val rent_to_pay: Property.t -> t -> int 

(* [is_built_evenly player pp flag] checks if [pp] color group owned by the player is built evenly. 
   [flag] is for checking if it is in building or destroying process. *)
val is_built_evenly: t -> Property.t -> bool -> bool

(* [buildable_properties player] returns all buildable properties owned by the player *)
val buidable_properties: t -> Property.t list

(* [total_assets player] returns the total assets of the player *)
val total_assets: t -> int

(* [no_house_on_monopoly player pp] checks if there is no house on the monopolied color group [pp] *)
val no_house_on_monopoly: t -> Property.t -> bool

(* [is_owner player pp] checks if the player is the owner of [pp] *)
val is_owner: t -> Property.t -> bool

(* [is_mortgageable player pp] checks if [pp] is mortgageable *)
val is_mortgageable: t -> Property.t -> bool

(* [mortgageable_properties player] returns all mortgageable properties owned by the player *)
val mortgageable_properties: t -> Property.t list

(* [unmortgageable_properties player] returns all unmortgageable properties owned by the player *)
val unmortgageable_properties: t -> Property.t list

(* [return_to_bank player pp] returns [pp] owned by the player back to the bank *)
val return_to_bank: t -> Property.t -> unit

(* [return_all_properties player] returns all properties owned by the player back to the bank *)
val return_all_properties: t -> unit

(* [swap_owner giver receiver pp] gives [pp] owned by [giver] to [receiver] *)
val swap_owner: t -> t -> Property.t -> unit

(* [give_all_properties giver receiver] gives all properties owned by [giver] to [receiver] *)
val give_all_properties: t -> t -> unit

(* [destroyable_properties player] returns all destroyable properties owned by the player *)
val destroyable_properties: t -> Property.t list

(* 
   HELPER 
*)

(* [get_jail_free_card player] gets all jail free cards owned by the player *)
val get_jail_free_card: t -> string list

(* [num_jail_free_card player] gets the number of jail free cards owned by the player *)
val num_jail_free_card: t -> int

(* [add_jail_free_card player name] adds a jail free card to the player's deck *)
val add_jail_free_card: t -> string -> unit

(* [reomve_jail_free_card player] removes a jail free card from the player's deck *)
val remove_jail_free_card: t -> unit

(* [get_player_name player] gets the name of the player *)
val get_player_name: t -> string

(* [get_position player] gets the position of the player *)
val get_position: t -> Property.t

(* [get_doubles player] gets the number of doubles achieved by the player *)
val get_doubles: t -> int

(* [add_doubles player] adds the number of doubles achieved by the player by 1 *)
val add_doubles: t -> unit

(* [get_money player] gets the money of the player *)
val get_money: t -> int

(* [get_properties player] gets the property list owned by the player *)
val get_properties: t -> Property.t list

(* [get_monopolies player] gets the monopoly list owned by the player *)
val get_monopolies: t -> Property.property_type list

(* [get_jailed_turns player] gets the left jailed turns of the player *)
val get_jailed_turns: t -> int

(* [get_is_real player] checks if the player is a real human player *)
val get_is_real: t -> bool
