open Spectrum
open Property
open Player
open Game

let input () =
  print_string "\n> ";
  read_line ()

let press_any_key () =
  let _ = read_line () in ()

let clear () =
  let _ = Sys.command("clear") in ()

let print_hr () =
  ANSITerminal.print_string [ANSITerminal.yellow] (String.make 126 '>' ^ "\n")

let print_opening () =
  clear ();
  Simple.printf
  "\n\n 
  @{<#ff0000>███╗   ███╗ ██████╗ ███╗   ██╗ ██████╗ ██████╗  ██████╗ ██╗  ██╗   ██╗██╗@}
  @{<#ffa500>████╗ ████║██╔═══██╗████╗  ██║██╔═══██╗██╔══██╗██╔═══██╗██║  ╚██╗ ██╔╝██║@}
  @{<#ffff00>██╔████╔██║██║   ██║██╔██╗ ██║██║   ██║██████╔╝██║   ██║██║   ╚████╔╝ ██║@}
  @{<#008000>██║╚██╔╝██║██║   ██║██║╚██╗██║██║   ██║██╔═══╝ ██║   ██║██║    ╚██╔╝  ╚═╝@}
  @{<#00bfff>██║ ╚═╝ ██║╚██████╔╝██║ ╚████║╚██████╔╝██║     ╚██████╔╝███████╗██║   ██╗@}
  @{<#d2691e>╚═╝     ╚═╝ ╚═════╝ ╚═╝  ╚═══╝ ╚═════╝ ╚═╝      ╚═════╝ ╚══════╝╚═╝   ╚═╝@}\n\n"

let print_ending () =
  clear ();
  Simple.printf
  "\n\n
  @{<#ff0000> ██████╗  █████╗ ███╗   ███╗███████╗     ██████╗ ██╗   ██╗███████╗██████╗@} 
  @{<#ffa500>██╔════╝ ██╔══██╗████╗ ████║██╔════╝    ██╔═══██╗██║   ██║██╔════╝██╔══██╗@}
  @{<#ffff00>██║  ███╗███████║██╔████╔██║█████╗      ██║   ██║██║   ██║█████╗  ██████╔╝@}
  @{<#008000>██║   ██║██╔══██║██║╚██╔╝██║██╔══╝      ██║   ██║╚██╗ ██╔╝██╔══╝  ██╔══██╗@}
  @{<#00bfff>╚██████╔╝██║  ██║██║ ╚═╝ ██║███████╗    ╚██████╔╝ ╚████╔╝ ███████╗██║  ██║@}
  @{<#d2691e> ╚═════╝ ╚═╝  ╚═╝╚═╝     ╚═╝╚══════╝     ╚═════╝   ╚═══╝  ╚══════╝╚═╝  ╚═╝@}\n\n"

let player_info_string player =
  let name = get_player_name player in 
  let money = string_of_int @@ get_money player in 
  let position = get_name @@ get_position player in 
  let properties = string_of_int @@ List.length @@ get_properties player in 
  let monopolies = string_of_int @@ List.length @@ get_monopolies player in 
  let turns = string_of_int @@ get_jailed_turns player in 
  let cards = Core.List.to_string ~f:(fun x -> x) (get_jail_free_card player) in 
  " ---------------------------------------------- \n"
  ^
  "|  Player Name: " ^ name ^ String.make (31 - String.length name) ' ' ^ "|\n"
  ^
  "|  Money: $" ^ money ^ String.make (36 - String.length money) ' ' ^ "|\n"
  ^
  "|  Position: " ^ position ^ String.make (34 - String.length position) ' ' ^ "|\n"
  ^
  "|  Property Amount: " ^ properties ^ String.make (27 - String.length properties) ' ' ^ "|\n"
  ^
  "|  Monopoly Amount: " ^ monopolies ^ String.make (27 - String.length monopolies) ' ' ^ "|\n"
  ^
  "|  Left Jailed Turns: " ^ turns ^ String.make (25 - String.length turns) ' ' ^ "|\n"
  ^
  "|  Jail Free Cards: " ^ cards ^ String.make (27 - String.length cards) ' ' ^ "|\n"
  ^
  " ---------------------------------------------- \n"
 
(* 48 *)
let print_player_info player =
  ANSITerminal.print_string [] (player_info_string player)

let print_board game =
  clear ();
  let med_ave_owner = get_owner @@ get_property_by_name game "Med. Avenue" in 
  let baltic_ave_owner = get_owner @@ get_property_by_name game "Baltic Avenue" in 
  let oriental_ave_owner = get_owner @@ get_property_by_name game "Oriental Avenue" in 
  let vermont_ave_owner = get_owner @@ get_property_by_name game "Vermont Avenue" in 
  let conn_ave_owner = get_owner @@ get_property_by_name game "Conn. Avenue" in 
  let st_chas_place_owner = get_owner @@ get_property_by_name game "St. Chas. Place" in 
  let states_ave_owner = get_owner @@ get_property_by_name game "States Avenue" in 
  let vir_ave_owner = get_owner @@ get_property_by_name game "Virginia Avenue" in 
  let st_james_place_owner = get_owner @@ get_property_by_name game "St. James Place" in 
  let tenn_ave_owner = get_owner @@ get_property_by_name game "Tennessee Avenue" in 
  let ny_ave_owner = get_owner @@ get_property_by_name game "New York Avenue" in 
  let kent_ave_owner = get_owner @@ get_property_by_name game "Kentucky Avenue" in 
  let indiana_ave_owner = get_owner @@ get_property_by_name game "Indiana Avenue" in 
  let illinois_ave_owner = get_owner @@ get_property_by_name game "Illinois Avenue" in 
  let atl_ave_owner = get_owner @@ get_property_by_name game "Atlantic Avenue" in 
  let vent_ave_owner = get_owner @@ get_property_by_name game "Ventnor Avenue" in 
  let marvin_gardens_owner = get_owner @@ get_property_by_name game "Marvin Gardens" in 
  let pac_ave_owner = get_owner @@ get_property_by_name game "Pacific Avenue" in 
  let n_carol_ave_owner = get_owner @@ get_property_by_name game "N. Carol. Avenue" in 
  let penn_ave_owner = get_owner @@ get_property_by_name game "Penn. Avenue" in 
  let park_place_owner = get_owner @@ get_property_by_name game "Park Place" in 
  let boardwalk_owner = get_owner @@ get_property_by_name game "Boardwalk" in 
  let reading_rr_owner = get_owner @@ get_property_by_name game "Reading Railroad" in 
  let penn_rr_owner = get_owner @@ get_property_by_name game "Penn. Railroad" in 
  let bo_rr_owner = get_owner @@ get_property_by_name game "B. & O. Railroad" in 
  let short_line_owner = get_owner @@ get_property_by_name game "Short Line" in 
  let elec_comp_owner = get_owner @@ get_property_by_name game "Electric Company" in 
  let water_works_owner = get_owner @@ get_property_by_name game "Water Works" in 

  let med_ave = 
    let pp = get_property_by_name game "Med. Avenue" in 
    if not @@ is_owned pp then "" else if is_mortgaged pp then " (M) " else " (" ^ (string_of_int @@ num_houses pp) ^ ") " in 
  let baltic_ave = 
    let pp = get_property_by_name game "Baltic Avenue" in
    if not @@ is_owned pp then "" else if is_mortgaged pp then " (M) " else " (" ^ (string_of_int @@ num_houses pp) ^ ") " in 
  let oriental_ave = 
    let pp =  get_property_by_name game "Oriental Avenue" in 
    if not @@ is_owned pp then "" else if is_mortgaged pp then " (M) " else " (" ^ (string_of_int @@ num_houses pp) ^ ") " in 
  let vermont_ave = 
    let pp = get_property_by_name game "Vermont Avenue" in 
    if not @@ is_owned pp then "" else if is_mortgaged pp then " (M) " else " (" ^ (string_of_int @@ num_houses pp) ^ ") " in 
  let conn_ave = 
    let pp = get_property_by_name game "Conn. Avenue" in 
    if not @@ is_owned pp then "" else if is_mortgaged pp then " (M) " else " (" ^ (string_of_int @@ num_houses pp) ^ ") " in 
  let st_chas_place = 
    let pp = get_property_by_name game "St. Chas. Place" in 
    if not @@ is_owned pp then "" else if is_mortgaged pp then " (M) " else " (" ^ (string_of_int @@ num_houses pp) ^ ") " in 
  let states_ave = 
    let pp = get_property_by_name game "States Avenue" in 
    if not @@ is_owned pp then "" else if is_mortgaged pp then " (M) " else " (" ^ (string_of_int @@ num_houses pp) ^ ") " in 
  let vir_ave = 
    let pp = get_property_by_name game "Virginia Avenue" in 
    if not @@ is_owned pp then "" else if is_mortgaged pp then " (M) " else " (" ^ (string_of_int @@ num_houses pp) ^ ") " in 
  let st_james_place = 
    let pp = get_property_by_name game "St. James Place" in 
    if not @@ is_owned pp then "" else if is_mortgaged pp then " (M) " else " (" ^ (string_of_int @@ num_houses pp) ^ ") " in 
  let tenn_ave = 
    let pp = get_property_by_name game "Tennessee Avenue" in 
    if not @@ is_owned pp then "" else if is_mortgaged pp then " (M) " else " (" ^ (string_of_int @@ num_houses pp) ^ ") " in 
  let ny_ave = 
    let pp = get_property_by_name game "New York Avenue" in 
    if not @@ is_owned pp then "" else if is_mortgaged pp then " (M) " else " (" ^ (string_of_int @@ num_houses pp) ^ ") " in 
  let kent_ave = 
    let pp = get_property_by_name game "Kentucky Avenue" in 
    if not @@ is_owned pp then "" else if is_mortgaged pp then " (M) " else " (" ^ (string_of_int @@ num_houses pp) ^ ") " in 
  let indiana_ave = 
    let pp = get_property_by_name game "Indiana Avenue" in 
    if not @@ is_owned pp then "" else if is_mortgaged pp then " (M) " else " (" ^ (string_of_int @@ num_houses pp) ^ ") " in 
  let illinois_ave = 
    let pp = get_property_by_name game "Illinois Avenue" in 
    if not @@ is_owned pp then "" else if is_mortgaged pp then " (M) " else " (" ^ (string_of_int @@ num_houses pp) ^ ") " in 
  let atl_ave = 
    let pp = get_property_by_name game "Atlantic Avenue" in 
    if not @@ is_owned pp then "" else if is_mortgaged pp then " (M) " else " (" ^ (string_of_int @@ num_houses pp) ^ ") " in 
  let vent_ave = 
    let pp = get_property_by_name game "Ventnor Avenue" in 
    if not @@ is_owned pp then "" else if is_mortgaged pp then " (M) " else " (" ^ (string_of_int @@ num_houses pp) ^ ") " in 
  let marvin_gardens = 
    let pp = get_property_by_name game "Marvin Gardens" in 
    if not @@ is_owned pp then "" else if is_mortgaged pp then " (M) " else " (" ^ (string_of_int @@ num_houses pp) ^ ") " in 
  let pac_ave = 
    let pp = get_property_by_name game "Pacific Avenue" in 
    if not @@ is_owned pp then "" else if is_mortgaged pp then " (M) " else " (" ^ (string_of_int @@ num_houses pp) ^ ") " in 
  let n_carol_ave = 
    let pp = get_property_by_name game "N. Carol. Avenue" in 
    if not @@ is_owned pp then "" else if is_mortgaged pp then " (M) " else " (" ^ (string_of_int @@ num_houses pp) ^ ") " in 
  let penn_ave = 
    let pp = get_property_by_name game "Penn. Avenue" in 
    if not @@ is_owned pp then "" else if is_mortgaged pp then " (M) " else " (" ^ (string_of_int @@ num_houses pp) ^ ") " in 
  let park_place = 
    let pp = get_property_by_name game "Park Place" in 
    if not @@ is_owned pp then "" else if is_mortgaged pp then " (M) " else " (" ^ (string_of_int @@ num_houses pp) ^ ") " in 
  let boardwalk = 
    let pp = get_property_by_name game "Boardwalk" in 
    if not @@ is_owned pp then "" else if is_mortgaged pp then " (M) " else " (" ^ (string_of_int @@ num_houses pp) ^ ") " in 
  let reading_rr = 
    let pp = get_property_by_name game "Reading Railroad" in 
    if is_mortgaged pp then " (M) " else "" in 
  let penn_rr = 
    let pp = get_property_by_name game "Penn. Railroad" in 
    if is_mortgaged pp then " (M) " else "" in 
  let bo_rr = 
    let pp = get_property_by_name game "B. & O. Railroad" in 
    if is_mortgaged pp then " (M) " else "" in 
  let short_line = 
    let pp = get_property_by_name game "Short Line" in 
    if is_mortgaged pp then " (M) " else "" in 
  let elec_comp = 
    let pp = get_property_by_name game "Electric Company" in 
    if is_mortgaged pp then " (M) " else "" in 
  let water_works = 
    let pp = get_property_by_name game "Water Works" in 
    if is_mortgaged pp then " (M) " else "" in 

  Simple.printf 
  "
 --------------------------------------------------------------------------------------------------------------------------------------------------------------------
|            |   $220   |          |   $220   |   $240   |   $200   |   $260   |   $260   |   $150   |   $280   |            | Color                                 |
|    Free    | Kentucky |  Chance  |  Indiana | Illinois |  B.& O.  | Atlantic |  Ventnor |   Water  |  Marvin  |    Go To   |=======================================|
|   Parking  |  Avenue  |    II    |  Avenue  |  Avenue  | Railroad |  Avenue  |  Avenue  |   Works  |  Gardens |    Jail    | Med. Avenue: %s
|            |@{<#ff0000>██████████@}|          |@{<#ff0000>██████████@}|@{<#ff0000>██████████@}|          |@{<#ffff00>██████████@}|@{<#ffff00>██████████@}|          |@{<#ffff00>██████████@}|            | Baltic Avenue: %s
|------------ -------------------------------------------------------------------------------------------------- ------------|---------------------------------------|
| New York @{<#ffa500>██@}|                                                                                                  |@{<#008000>██@}  Pacific | Oriental Avenue: %s
|  Avenue  @{<#ffa500>██@}|                                                                                                  |@{<#008000>██@}  Avenue  | Vermont Avenue: %s
|   $200   @{<#ffa500>██@}|                                                                                                  |@{<#008000>██@}   $300   | Conn. Avenue: %s
|------------|                                                                                                  |------------|---------------------------------------|
|Tennessee @{<#ffa500>██@}|                                                                                                  |@{<#008000>██@} N. Carol.| St. Chas. Place: %s
|  Avenue  @{<#ffa500>██@}|                                                                                                  |@{<#008000>██@}  Avenue  | States Avenue: %s
|   $180   @{<#ffa500>██@}|                                                                                                  |@{<#008000>██@}   $300   | Virginia Avenue: %s
|------------|                                                                                                  |------------|---------------------------------------|
| Community  |                                                                                                  |  Community | St. James Place: %s
|  Chest II  |                                                                                                  |  Chest III | Tennessee Avenue: %s
|------------|                                                                                                  |------------| New York Avenue: %s
|St. James @{<#ffa500>██@}|                                                                                                  |@{<#008000>██@}   Penn.  |---------------------------------------|
|  Place   @{<#ffa500>██@}|                                                                                                  |@{<#008000>██@}  Avenue  | Kentucky Avenue: %s
|   $180   @{<#ffa500>██@}|                                                                                                  |@{<#008000>██@}   $320   | Indiana Avenue: %s
|------------|                     --    --    ----   --    -  ----   ----   ----  -      -   -                 |------------| Illinois Avenue: %s
|    Penn.   |                    |  \\  /  |  |    | |  \\   | |    | |    | |    | |       \\ /                  |    Short   |---------------------------------------|
|  Railroad  |                    |   \\/   |  |    | |   \\  | |    | |----  |    | |    |   |                   |    Line    | Atlantic Avenue: %s
|    $200    |                    -        -   ----  -    --   ----  -       ----   ----    -                   |    $200    | Ventnor Avenue: %s
|------------|                                                                                                  |------------| Marvin Gardens: %s
| Virginia @{<#ff00ff>██@}|                                                                                                  |            |---------------------------------------|
|  Avenue  @{<#ff00ff>██@}|                                                                                                  | Chance III | Pacific Avenue: %s
|   $160   @{<#ff00ff>██@}|                                                                                                  |            | N. Carol. Avenue: %s
|------------|                                                                                                  |------------| Penn. Avenue: %s
|  States  @{<#ff00ff>██@}|                                                                                                  |@{<#0000ff>██@}   Park   |---------------------------------------|
|  Avenue  @{<#ff00ff>██@}|                                                                                                  |@{<#0000ff>██@}   Place  | Park Place: %s
|   $140   @{<#ff00ff>██@}|                                                                                                  |@{<#0000ff>██@}   $350   | BoardWalk: %s
|------------|                                                                                                  |------------|=======================================|
|  Electric  |                                                                                                  |   Luxury   | Railroad                              |
|  Company   |                                                                                                  |    Tax     |=======================================|
|    $150    |                                                                                                  |   Pay $75  | Reading Railroad: %s
|------------|                                                                                                  |------------| Penn. Railroad: %s
|St. Chas. @{<#ff00ff>██@}|                                                                                                  |@{<#0000ff>██@}   Board  | B. & O. Railroad: %s
|  Place   @{<#ff00ff>██@}|                                                                                                  |@{<#0000ff>██@}   Walk   | Short Line: %s
|   $140   @{<#ff00ff>██@}|                                                                                                  |@{<#0000ff>██@}   $400   |=======================================|
|------------ -------------------------------------------------------------------------------------------------- ------------| Utilities                             |
|            |@{<#00ffff>██████████@}|@{<#00ffff>██████████@}|          |@{<#00ffff>██████████@}|          |          |@{<#d2691e>██████████@}|          |@{<#d2691e>██████████@}|   Pass Go  |=======================================|
|  In Jail/  |   Conn.  |  Vermont |  Chance  | Oriental | Reading  |  Income  |  Baltic  | Community|   Med.   |  /-------  | Electric Company: %s
|  Visiting  |  Avenue  |  Avenue  |     I    |  Avenue  | Railroad |    Tax   |  Avenue  |  Chest I |  Avenue  |  \\-------  | Water Works: %s
|            |   $120   |   $100   |          |   $100   |   $200   | Pay $200 |    $60   |          |    $60   |  Get $200  |                                       |
 -------------------------------------------------------------------------------------------------------------------------------------------------------------------- \n"
  (med_ave_owner ^ med_ave ^ (String.make (39 - String.length " Med. Avenue: " - String.length med_ave_owner - String.length med_ave) ' ') ^ "|")
  (baltic_ave_owner ^ baltic_ave ^ (String.make (39 - String.length " Baltic Avenue: " - String.length baltic_ave_owner - String.length baltic_ave) ' ') ^ "|")
  (oriental_ave_owner ^ oriental_ave ^ (String.make (39 - String.length " Oriental Avenue: " - String.length oriental_ave_owner - String.length oriental_ave) ' ') ^ "|")
  (vermont_ave_owner ^ vermont_ave ^ (String.make (39 - String.length " Vermont Avenue: " - String.length vermont_ave_owner - String.length vermont_ave) ' ') ^ "|")
  (conn_ave_owner ^ conn_ave ^ (String.make (39 - String.length " Conn. Avenue: " - String.length conn_ave_owner - String.length conn_ave) ' ') ^ "|")
  (st_chas_place_owner ^ st_chas_place ^ (String.make (39 - String.length " St. Chas. Place: " - String.length st_chas_place_owner - String.length st_chas_place) ' ') ^ "|")
  (states_ave_owner ^ states_ave ^ (String.make (39 - String.length " States Avenue: " - String.length states_ave_owner - String.length states_ave) ' ') ^ "|")
  (vir_ave_owner ^ vir_ave ^ (String.make (39 - String.length " Virginia Avenue: " - String.length vir_ave_owner - String.length vir_ave) ' ') ^ "|")
  (st_james_place_owner ^ st_james_place ^ (String.make (39 - String.length " St. James Place: " - String.length st_james_place_owner - String.length st_james_place) ' ') ^ "|")
  (tenn_ave_owner ^ tenn_ave ^ (String.make (39 - String.length " Tennessee Avenue: " - String.length tenn_ave_owner - String.length tenn_ave) ' ') ^ "|")
  (ny_ave_owner ^ ny_ave ^ (String.make (39 - String.length " New York Avenue: " - String.length ny_ave_owner - String.length ny_ave) ' ') ^ "|")
  (kent_ave_owner ^ kent_ave ^ (String.make (39 - String.length " Kentucky Avenue: " - String.length kent_ave_owner - String.length kent_ave) ' ') ^ "|")
  (indiana_ave_owner ^ indiana_ave ^ (String.make (39 - String.length " Indiana Avenue: " - String.length indiana_ave_owner - String.length indiana_ave) ' ') ^ "|")
  (illinois_ave_owner ^ illinois_ave ^ (String.make (39 - String.length " Illinois Avenue: " - String.length illinois_ave_owner - String.length illinois_ave) ' ') ^ "|")
  (atl_ave_owner ^ atl_ave ^ (String.make (39 - String.length " Atlantic Avenue: " - String.length atl_ave_owner - String.length atl_ave) ' ') ^ "|")
  (vent_ave_owner ^ vent_ave ^ (String.make (39 - String.length " Ventnor Avenue: " - String.length vent_ave_owner - String.length vent_ave) ' ') ^ "|")
  (marvin_gardens_owner ^ marvin_gardens ^ (String.make (39 - String.length " Marvin Gardens: " - String.length marvin_gardens_owner - String.length marvin_gardens) ' ') ^ "|")
  (pac_ave_owner ^ pac_ave ^ (String.make (39 - String.length " Pacific Avenue: " - String.length pac_ave_owner - String.length pac_ave) ' ') ^ "|")
  (n_carol_ave_owner ^ n_carol_ave ^ (String.make (39 - String.length " N. Carol. Avenue: " - String.length n_carol_ave_owner - String.length n_carol_ave) ' ') ^ "|")
  (penn_ave_owner ^ penn_ave ^ (String.make (39 - String.length " Penn. Avenue: " - String.length penn_ave_owner - String.length penn_ave) ' ') ^ "|")
  (park_place_owner ^ park_place ^ (String.make (39 - String.length " Park Place: " - String.length park_place_owner - String.length park_place) ' ') ^ "|")
  (boardwalk_owner ^ boardwalk ^ (String.make (39 - String.length " BoardWalk: " - String.length boardwalk_owner - String.length boardwalk) ' ') ^ "|")
  (reading_rr_owner ^ reading_rr ^ (String.make (39 - String.length " Reading Railroad: " - String.length reading_rr_owner - String.length reading_rr) ' ') ^ "|")
  (penn_rr_owner ^ penn_rr ^ (String.make (39 - String.length " Penn. Railroad: " - String.length penn_rr_owner - String.length penn_rr) ' ') ^ "|")
  (bo_rr_owner ^ bo_rr ^ (String.make (39 - String.length " B. & O. Railroad: " - String.length bo_rr_owner - String.length bo_rr) ' ') ^ "|")
  (short_line_owner ^ short_line ^ (String.make (39 - String.length " Short Line: " - String.length short_line_owner - String.length short_line) ' ') ^ "|")
  (elec_comp_owner ^ elec_comp ^ (String.make (39 - String.length " Electric Company: " - String.length elec_comp_owner - String.length elec_comp) ' ') ^ "|")
  (water_works_owner ^ water_works ^ (String.make (39 - String.length " Water Works: " - String.length water_works_owner - String.length water_works) ' ') ^ "|")
