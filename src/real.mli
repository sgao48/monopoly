(* Real: real player in Monopoly game *)

(* [handle_on_property game player pp] handles the situation when the real player lands on a property type *)
val handle_on_property: Game.t -> Player.t -> Property.t -> bool

(* [handle_auction player pp price] handles the situation when the real player participates in the auction *)
val handle_auction: Player.t -> int -> int

(* [handle_in_jail game player] handles the situation when the real player is in the jail *)
val handle_in_jail: Game.t -> Player.t -> int

(* [handle_actions game player] handles the situation when the real player is choosing actions freely *)
val handle_actions: Game.t -> Player.t -> unit

(* [pay_money_check game player pp owed] checks if the real player is able to pay the money *)
val pay_money_check: Game.t -> Player.t -> Property.t -> int -> unit