open Property
open Player
open Game
open Print 
open Spectrum

let take_action_by_prob num lower upper =
  if num >= upper then false
  else if num <= lower && num <= upper then true
  else
    (
      let threshold = (float_of_int (upper - num)) /. (float_of_int (upper - lower)) in 
      Random.self_init ();
      let prob = Random.float 1.0 in 
      if prob <= threshold then true else false
    )

let random_int_in_range lower upper =
  Random.self_init ();
  let num = Random.int (upper - lower) in 
  num + lower + 1

let random_float_in_range lower upper =
  Random.self_init ();
  let num = Random.float (upper -. lower) in 
  num +. lower

let random_option_from_list options = 
  Random.self_init ();
  let id = Random.int @@ List.length options in 
  List.nth options id

let handle_mortgage player =
  let l =  mortgageable_properties player in 
  Simple.printf "Your mortgageable properties are listed below:\n";
  ANSITerminal.print_string [ANSITerminal.yellow] (Core.List.to_string ~f:(fun x -> get_name x) l ^ "\n");
  Simple.printf "Select a property for mortgage ↩";
  press_any_key ();

  let property = random_option_from_list l in 
  Simple.printf "%s selected %s. ↩" (get_player_name player) (get_name property);
  press_any_key ();

  let price = get_mortgaged_price property in 
  set_mortgaged property;
  update_money player price;
  Simple.printf "You collected $%d from mortgage. Your current money is $%d. ↩" price (get_money player);
  press_any_key ()

let handle_sell_house player =
  let l = destroyable_properties player in 
  Simple.printf "Your destroyable properties are listed below:\n";
  ANSITerminal.print_string [ANSITerminal.yellow] (Core.List.to_string ~f:(fun x -> get_name x) l ^ "\n");
  Simple.printf "Select a property for selling house ↩";

  let property = random_option_from_list l in 
  Simple.printf "%s selected %s. ↩" (get_player_name player) (get_name property);
  press_any_key ();

  let price = get_house_cost property / 2 in 
  downgrade_property property;
  update_money player price;
  Simple.printf "You collected $%d from selling house on %s. Your current money is $%d. ↩" price (get_name property) (get_money player);
  press_any_key ()

let random_pay_money_option player =
  let options = ref [] in 
  (
    if List.length @@ mortgageable_properties player > 0 then
      options := "mortgage" :: !options
  );
  (
    if List.length @@ destroyable_properties player > 0 then
      options := "sell house" :: !options
  );
  random_option_from_list !options

let rec pay_money_check game player property owed =
  let money = get_money player in 
  if total_assets player + money < owed then
    (
      Simple.printf "Sorry, you don't have enough assets. You are in bankruptcy and will be removed from the game. ↩";
      press_any_key ();
      bankruptcy game player property
    )
  else if money < owed then
    (
      Simple.printf "Sorry, you don't have enough money to pay. You are still short of $%d. ↩" (owed - money);
      press_any_key ();
      Simple.printf "Select your option (mortgage/sell house)";
      match random_pay_money_option player with
      | "mortgage" -> 
        (
          Simple.printf "%s chose to mortgage. ↩" (get_player_name player);
          press_any_key ();
          handle_mortgage player;
          print_hr ();
          pay_money_check game player property owed
        )
      | "sell house" -> 
        (
          Simple.printf "%s chose to sell house. ↩" (get_player_name player);
          press_any_key ();
          handle_sell_house player;
          print_hr ();
          pay_money_check game player property owed
        )
      | _ ->
        (
          Simple.printf "Invalid option. Please select your option again. ↩";
          press_any_key ();
          print_hr ();
          pay_money_check game player property owed
        )
    )

let calculate_property_value player property =
  let num_owned = count_owned_properties player property in 
  let num_required = num_for_monopoly property in 
  if num_owned + 1 = num_required then 
    (random_float_in_range 1.8 2.0) 
  else (random_float_in_range 1.4 1.5)

let handle_on_property game player property =
  let flag =
  (
    match is_owned property with
    | true ->
      (
        let owner = get_property_owner game property in
        (
          if owner = player then
            (
              Simple.printf "This property is owned by you. ↩";
              press_any_key ()
            )
          else if is_mortgaged property then 
            (
              Simple.printf "This property is mortgaged. You don't need to pay the rent. ↩";
              press_any_key ()
            )
          else
            (
              Simple.printf "%s is owned! You need to pay to %s for the rent.\n" (get_name property) (get_player_name owner);

              let rent = rent_to_pay property owner in 
              pay_money_check game player property rent;

              if in_game game player then
                pay_rent game player property rent
            )
        );
        false
      )
    | false ->
      (
        Simple.printf "%s is not owned! Do you want to buy it?\n" (get_name property);
        let flag = take_action_by_prob (get_price property) (get_money player / 2) (get_money player) in 
        if flag then
          (
            buy_property player property;
            Simple.printf "%s decided to buy %s. Your current money is $%d. ↩" (get_player_name player) (get_name property) (get_money player);
            press_any_key ();
            false
          )
        else
          (
            Simple.printf "%s decided not to buy %s. ↩" (get_player_name player) (get_name property);
            press_any_key ();
            true
          )
      )
  )
  in
  print_hr ();
  flag

let handle_auction player property cur_price =
  Simple.printf "@{<#ffff00>%s's Round: Current bidding price is $%d. Do you want to continue bidding?\n@}" (get_player_name player) cur_price;
  let money = get_money player in 
  let ratio = calculate_property_value player property in 
  let value = int_of_float @@ min (float_of_int money /. 2.0) (float_of_int (get_price property) *. ratio) in 
  let flag = take_action_by_prob cur_price (get_price property) value in 
  if flag then 
    (
      let price = random_int_in_range cur_price value in 
      Simple.printf "%s decided to continue bidding and offered $%d. ↩" (get_player_name player) price;
      press_any_key ();
      price
    )
  else
    (
      Simple.printf "%s decided not to continue bidding. ↩" (get_player_name player);
      press_any_key ();
      (-1)
    )

let random_in_jail_option player =
  if num_jail_free_card player > 0 then "use"
  else
    (
      let options = ref ["roll"] in 
      if (get_money player) > 50 then options := "pay" :: !options;
      random_option_from_list !options
    )

let rec handle_in_jail_helper game player =
  let left_turns = get_jailed_turns player in 
  let player_name = get_player_name player in 

  Simple.printf "Choose your option to get out of the jail. (roll/use/pay) ↩";
  press_any_key ();
  let str = random_in_jail_option player in 
  match str with 
  | "roll" ->
    (
      Simple.printf "%s chose to roll. ↩" player_name;
      press_any_key ();
      let dices = roll_dice () in 
      Simple.printf "Your result is %d and %d. ↩" (fst dices) (snd dices);
      press_any_key ();

      if is_doubles dices then 
        (
          Simple.printf "Congratulations! Now you are out of jail. ↩";
          press_any_key ();
          print_hr ();
          out_of_jail player;
          sum_dices dices
        )
      else if left_turns > 1 then 
        (
          Simple.printf "Sorry, you can't get out of the jail. ↩";
          press_any_key ();
          print_hr ();
          next_jailed_turn player;
          (-1)
        )
      else
        (
          Simple.printf "This is your last round in the jail. You must pay $50. ↩";
          press_any_key ();

          pay_money_check game player (get_position player) 50;

          if in_game game player then
            (
              update_money player (-50);
              Simple.printf "You have paid $50 and get out of the jail. Your current money is $%d. ↩" (get_money player);
              press_any_key ();
              print_hr ();
              out_of_jail player;
              sum_dices dices
            )
          else (-1)
        )
    )
  | "use" ->
    (
      let deck_type = List.hd @@ get_jail_free_card player in 
      let old_deck = get_deck game deck_type in 
      let new_deck = old_deck @ [6] in 
      set_deck game new_deck deck_type;
      remove_jail_free_card player;
      Simple.printf "%s used a jail free card. Now you are out of jail. ↩" player_name;
      press_any_key ();
      print_hr ();
      out_of_jail player;

      let dices = roll_dice () in 
      Simple.printf "Your result is %d and %d.\n" (fst dices) (snd dices);
      sum_dices dices
    )
  | "pay" ->
    (
      Simple.printf "%s chose to pay $50. ↩" player_name;
      press_any_key ();
      update_money player (-50);
      Simple.printf "You have paid $50 and get out of the jail. Your current money is $%d. ↩" (get_money player);
      press_any_key ();
      print_hr ();
      out_of_jail player;

      let dices = roll_dice () in 
      Simple.printf "Your result is %d and %d.\n" (fst dices) (snd dices);
      sum_dices dices
    )
  | _ -> 
    (
      Simple.printf "Invalid option. Please select your option again. ↩";
      press_any_key ();
      print_hr ();
      handle_in_jail_helper game player
    )

let handle_in_jail game player =
  Simple.printf "You have %d round(s) left in the jail.\n" (get_jailed_turns player);
  handle_in_jail_helper game player

let handle_unmortgage player =
  let l =  unmortgageable_properties player in 
  Simple.printf "Your unmortgageable properties are listed below:\n";
  ANSITerminal.print_string [ANSITerminal.yellow] (Core.List.to_string ~f:(fun x -> get_name x) l ^ "\n");
  Simple.printf "Select a property for unmortgage ↩";
  press_any_key ();

  let property = random_option_from_list l in 
  Simple.printf "%s selected %s. ↩" (get_player_name player) (get_name property);
  press_any_key ();

  let price = int_of_float @@ 1.1 *. float_of_int (get_mortgaged_price property) in 
  unset_mortgaged property;
  update_money player (-price);
  Simple.printf "You paid $%d for unmortgage. Your current money is $%d. ↩" price (get_money player);
  press_any_key ()

let handle_build_house player =
  let l = buidable_properties player in 
  Simple.printf "Your buildable properties are listed below:\n";
  ANSITerminal.print_string [ANSITerminal.yellow] (Core.List.to_string ~f:(fun x -> get_name x) l ^ "\n");
  Simple.printf "Select a property for building house ↩";
  press_any_key ();

  let property = random_option_from_list l in 
  Simple.printf "%s selected %s. ↩" (get_player_name player) (get_name property);
  press_any_key ();

  let price = get_house_cost property in 
  upgrade_property property;
  update_money player (-price);
  Simple.printf "You paid $%d for building house on %s. Your current money is $%d. ↩" price (get_name property) (get_money player);
  press_any_key ()

let random_action_option player =
  let money = get_money player in 

  let options = ref ["skip"] in 
  (
    if List.length @@ mortgageable_properties player > 0 && money < random_int_in_range 200 250 then
      options := "mortgage" :: !options
  );
  (
    if List.length @@ unmortgageable_properties player > 0 && money > random_int_in_range 500 600 then
      options := "unmortgage" :: !options
  );
  (
    if List.length @@ buidable_properties player > 0 && money > random_int_in_range 450 550 then
      options := "build house" :: !options
  );
  (
    if List.length @@ destroyable_properties player > 0 && money < random_int_in_range 250 350 then
      options := "sell house" :: !options
  );

  random_option_from_list !options

let rec handle_actions game player =
  Simple.printf "Please select your option. (skip/mortgage/unmortgage/build house/sell house) ↩";
  press_any_key ();
  match random_action_option player with 
  | "skip" ->
    (
      Simple.printf "%s skipped this stage. ↩" (get_player_name player);
      press_any_key ();
      print_hr ()
    )
  | "mortgage" -> 
    (
      Simple.printf "%s chose to mortgage. ↩" (get_player_name player);
      press_any_key ();
      handle_mortgage player;
      print_hr ();
      handle_actions game player
    )
  | "unmortgage" -> 
    (
      Simple.printf "%s chose to unmortgage. ↩" (get_player_name player);
      press_any_key ();
      handle_unmortgage player;
      print_hr ();
      handle_actions game player
    )
  | "build house" -> 
    (
      Simple.printf "%s chose to build house. ↩" (get_player_name player);
      press_any_key ();
      handle_build_house player;
      print_hr ();
      handle_actions game player
    )
  | "sell house" -> 
    (
      Simple.printf "%s chose to sell house. ↩" (get_player_name player);
      press_any_key ();
      handle_sell_house player;
      print_hr ();
      handle_actions game player
    )
  | _ ->
    (
      Simple.printf "Invalid option. Please select your option again. ↩";
      press_any_key ();
      print_hr ();
      handle_actions game player
    )
