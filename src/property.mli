(* Property: a property space in Monopoly game *)

(* Representation of the property type *)
type property_type

(* Representation of the property status *)
type property_level

(* Abstract representation of the property type *)
type t

(* 
   MAIN 
*)

(* [is_property pp] checks if a space is property *)
val is_property: t -> bool

(* [is_owned pp] checks if a property is owned *)
val is_owned: t -> bool

(* [is_tax pp] checks if a property's type is tax type *)
val is_tax: t -> bool

(* [is_utilities pp] checks if a property's type is utility type *)
val is_utilities: t -> bool

(* [is_railroad pp] checks if a property's type is railroad type *)
val is_railroad: t -> bool

(* [is_jail pp] checks if a property is jail *)
val is_jail: t -> bool

(* [is_go_to_jail pp] checks if a property is go to jail *)
val is_go_to_jail: t -> bool

(* [is_chance pp] checks if a property's type is chance type *)
val is_chance: t -> bool

(* [is_community_chest pp] checks if a property's type is community chest type *)
val is_community_chest: t -> bool

(* [can_be_upgraded pp] checks if a property can be upgraded/built house *)
val can_be_upgraded: t -> bool

(* [can_be_upgrade_type pp] checks if a property belongs to a color property *)
val can_be_upgraded_type: property_type -> bool

(* [create_property name type price rent house_cost] creates a property with specific attributes *)
val create_property: string -> string -> int -> int array -> int -> t

(* [calculate_rent pp] calculates the rent of a property *)
val calculate_rent: t -> int

(* [upgrade_property pp] upgrades a property *)
val upgrade_property: t -> unit

(* [downgrade_property pp] downgrades a property *)
val downgrade_property: t -> unit

(* [num_for_monopoly pp] calculates how many properties are needed to get monopoly *)
val num_for_monopoly: t -> int

(* [num_houses pp] returns the number of houses on the specific property *)
val num_houses: t -> int

(* [reset_level pp] resets the property's level as 0 *)
val reset_level: t -> unit

(* [release_property pp] releases the property and resets all its attributes *)
val release_property: t -> unit

(* 
   HELPER 
*)

(* [get_name pp] gets the name of the property *)
val get_name: t -> string

(* [get_type pp] gets the type of the property *)
val get_type: t -> property_type

(* [get_price pp] gets the price of the property *)
val get_price: t -> int 

(* [get_house_cost pp] gets the house cost of the property *)
val get_house_cost: t -> int 

(* [get_owner pp] gets the owner name of the property *)
val get_owner: t -> string

(* [set_owner pp name] sets the owner name of the property as [name] *)
val set_owner: t -> string -> unit

(* [get_mortgaged_price pp] gets the mortgaged price of the property *)
val get_mortgaged_price: t -> int

(* [get_value pp] gets the whole value of the property *)
val get_value: t -> int

(* [is_mortgaged pp] checks if the property is mortgaged *)
val is_mortgaged: t -> bool

(* [set_mortgaged pp] sets a property as mortgaged status *)
val set_mortgaged: t -> unit

(* [unset_mortgaged pp] sets a property as unmortgaged status *)
val unset_mortgaged: t -> unit