open Property
open Player
open Game
open Spectrum

type giver = Player.t option

type receiver = Player.t option

type event = giver * int * receiver

type t = {
  text: string;
  effect: Game.t -> event list
}

let get_card_text card = card.text

let get_card_effect card = function game -> card.effect game

let receive_money money game: event list =
  [(None, money, Some (get_current_player game))]

let give_money money game: event list =
  [(Some (get_current_player game), money, None)]

let move_to_property property_name game =
  let player = get_current_player game in 
  let prev = get_position player in 
  let prev_id = get_property_index game prev in 
  let next = get_property_by_name game property_name in 
  let next_id = get_property_index game next in 
  move_by_position player next;
  Simple.printf "Your current position is %s.\n" (get_name @@ get_position player);
  if next_id < prev_id || property_name = "Pass Go" then 
    (
      print_endline "Congratulations! You passed over Pass Go and collected $200 from bank.";
      receive_money 200 game
    )
  else []

let move_to_nearest_utility game =
  let player = get_current_player game in 
  let position = get_position player in 
  let pos_id = get_property_index game position in 
  let utility_name =
    if pos_id > 12 && pos_id < 19 then "Water Works"
    else "Electric Company"
  in 
  let utility = get_property_by_name game utility_name in 
  move_by_position player utility;
  Simple.printf "Your current position is %s.\n" (get_name @@ get_position player);
  []

let move_to_nearest_railroad game =
  let player = get_current_player game in 
  let position = get_position player in 
  let pos_id = get_property_index game position in 
  let railroad_name =
    if pos_id > 5 && pos_id < 16 then "Penn. Railroad"
    else if pos_id > 15 && pos_id < 26 then "B. & O. Railroad"
    else if pos_id > 25 && pos_id < 36 then "Short Line"
    else "Reading Railroad"
  in 
  let railroad = get_property_by_name game railroad_name in 
  move_by_position player railroad;
  Simple.printf "Your current position is %s.\n" (get_name @@ get_position player);
  []

let jail_free deck game =
  add_jail_free_card (get_current_player game) deck;
  []

let back_three_steps game =
  let player = get_current_player game in 
  move_by_steps game player (-3) false;
  []

let go_to_jail_card game =
  let player = get_current_player game in 
  arrested game player;
  []

let rec house_repair_helper properties house_payment hotel_payment ans =
  match properties with
  | [] -> ans
  | hd :: tl ->
    let num_house = num_houses hd in 
    if num_house = 5 then
      house_repair_helper tl house_payment hotel_payment (ans + hotel_payment)
    else
      house_repair_helper tl house_payment hotel_payment (ans + house_payment * num_house)

let house_repair house_payment hotel_payment game =
  let player = get_current_player game in 
  let properties = get_properties player in 
  let cost = house_repair_helper properties house_payment hotel_payment 0 in 
  [(Some player, cost, None)]

let rec pay_all_players_helper players current_player payment ans =
  match players with
  | [] -> ans
  | hd :: tl -> 
    if hd <> current_player then 
      if payment > 0 then
        pay_all_players_helper tl current_player payment ((Some current_player, payment, Some hd) :: ans)
      else
        pay_all_players_helper tl current_player payment ((Some hd, -payment, Some current_player) :: ans)
    else
      pay_all_players_helper tl current_player payment ans

let pay_all_players payment game =
  let player = get_current_player game in 
  pay_all_players_helper (get_player_list game) player payment []

let chance_cards =
  [
    {
      text = "Advance to Go. (Collect $200) ↩";
      effect = move_to_property "Pass Go"
    };
    {
      text = "Advance to Illinois Avenue. If you pass Go, collect $200. ↩";
      effect = move_to_property "Illinois Avenue"
    };
    {
      text = "Advance to St. Chas. Place. If you pass Go, collect $200. ↩";
      effect = move_to_property "St. Chas. Place"
    };
    {
      text = "Advance to the nearest Utility. If you pass Go, NOT collect $200. ↩";
      effect = move_to_nearest_utility
    };
    {
      text = "Advance to the nearest Railroad. If you pass Go, NOT collect $200. ↩";
      effect = move_to_nearest_railroad
    };
    {
      text = "Bank pays you dividend of $50. ↩"; 
      effect = receive_money 50
    };
    {
      text = "Get Out of Jail Free ↩";
      effect = jail_free "chance"
    };
    {
      text = "Go Back 3 Spaces ↩";
      effect = back_three_steps
    };
    {
      text = "Go directly to Jail. If you pass Go, NOT collect $200. ↩";
      effect = go_to_jail_card
    };
    {
      text = 
        "Make general repairs on all your property. $25 per house. $100 per hotel. ↩";
      effect = house_repair 25 100
    };
    {
      text = "Pay poor tax of $15. ↩";
      effect = give_money 15
    };
    {
      text = "Take a trip to Reading Railroad. If you pass Go, collect $200. ↩";
      effect = move_to_property "Reading Railroad"
    };
    {
      text = "Take a walk on the Boardwalk. Advance to Boardwalk and collect $200 if you pass Go. ↩";
      effect = move_to_property "Boardwalk"
    };
    {
      text = "You have been elected Chairman of the Board. Pay each player $50. ↩";
      effect = pay_all_players 50
    };
    {
      text = "Your building and loan matures. Collect $150. ↩";
      effect = receive_money 150
    };
    {
      text = "You have won a crossword competition. Collect $100. ↩";
      effect = receive_money 100
    }
  ]

let community_chest_cards =
  [
    {
      text = "Advance to Go. (Collect $200) ↩";
      effect = move_to_property "Pass Go"
    };
    {
      text = "Bank error in your favor. Collect $200. ↩";
      effect = receive_money 200
    };
    {
      text = "Doctor's fee. Pay $50. ↩";
      effect = give_money 50
    };
    {
      text = "From sale of stock you get $50. ↩";
      effect = receive_money 50
    };
    {
      text = "Go directly to Jail. If you pass Go, NOT collect $200. ↩";
      effect = go_to_jail_card
    };
    {
      text = "Grand Opera Night. Collect $50 from every player for opening night seats. ↩";
      effect = pay_all_players (-50)
    };
    {
      text = "Get Out of Jail Free ↩";
      effect = jail_free "community chest"
    };
    {
      text = "Holiday Fund matures. Collect $100. ↩";
      effect = receive_money 100
    };
    {
      text = "Income tax refund. Collect $20. ↩";
      effect = receive_money 20
    };
    {
      text = "It is your birthday. Collect $10. ↩";
      effect = receive_money 10
    };
    {
      text = "Life insurance matures. Collect $100. ↩";
      effect = receive_money 100
    };
    {
      text = "Pay hospital fees of $100. ↩";
      effect = give_money 100
    };
    {
      text = "Pay school fees of $150. ↩";
      effect = give_money 150
    };
    {
      text = "Collect $25 consultancy fee. ↩";
      effect = receive_money 25
    };
    {
      text = "You are assessed for street repairs. $40 per house. $115 per hotel. ↩";
      effect = house_repair 40 115
    };
    {
      text = "You inherit $100. ↩";
      effect = receive_money 100
    }
  ]

let get_card deck_type card_id = 
  match deck_type with
  | "chance" -> List.nth chance_cards card_id
  | _ -> List.nth community_chest_cards card_id
