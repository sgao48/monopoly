(* Card: card information in Monopoly game *)

(* Abstract representation of the card type *)
type t

(* [chance_cards] returns the chance card deck *)
val chance_cards: t list

(* [community_chest_cards] returns the community chest card deck *)
val community_chest_cards: t list

(* [get_card deck index] gets the [index] card from the specific deck *)
val get_card: string -> int -> t

(* [get_card_text card] gets the text of a card *)
val get_card_text: t -> string

(* [get_card_effect card] gets the effect list of a card *)
val get_card_effect: t -> Game.t -> (Player.t option * int * Player.t option) list