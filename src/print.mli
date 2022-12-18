(* Print: complex I/O in Monopoly game *)

(* [input] lets the player input something *)
val input: unit -> string

(* [press_any_key] lets the player press any key *)
val press_any_key: unit -> unit

(* [clear] clears the current screen *)
val clear: unit -> unit

(* [print_board game] prints the gameboard on the screen *)
val print_board: Game.t -> unit

(* [print_opening] prints the opening information on the screen *)
val print_opening: unit -> unit

(* [print_ending] prints the ending information on the screen *)
val print_ending: unit -> unit

(* [print_player_info] prints the current player's information on the screen *)
val print_player_info: Player.t -> unit

(* [print_hr] prints a horizontal line on the screen *)
val print_hr: unit -> unit