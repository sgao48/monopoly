open Property
open Player
open Game
open Print 
open Spectrum

let handle_mortgage game player =
  match mortgageable_properties player with
  | [] -> 
    (
      Simple.printf "Sorry, you don't have any mortgageable property. ↩";
      press_any_key ()
    )
  | l -> 
    (
      Simple.printf "Your mortgageable properties are listed below:\n";
      ANSITerminal.print_string [ANSITerminal.yellow] (Core.List.to_string ~f:(fun x -> get_name x) l ^ "\n");

      let flag = ref false in 
      while not !flag do 
        (
          Simple.printf "Select a property for mortgage or enter 'q' to return to the previous level";

          let name = input () in 
          if name <> "q" then
            (
              try 
                (
                  let property = get_property_by_name game name in 

                  if List.exists (fun x -> x = property) l then
                    (
                      let price = get_mortgaged_price property in 

                      let flag2 = ref false in 
                      while not !flag2 do 
                        (
                          Simple.printf "You will collect $%d from mortgaging %s. Do you confirm? (y/n)" price name;
                          match input () with
                          | ""
                          | "y" -> 
                            (
                              set_mortgaged property;
                              update_money player price;
                              Simple.printf "You collected $%d from mortgage. Your current money is $%d. ↩" price (get_money player);
                              press_any_key ();
                              flag := true;
                              flag2 := true
                            )
                          | "n" -> flag2 := true
                          | _ -> 
                            (
                              Simple.printf "Invalid argument. Please enter again. ↩" ;
                              press_any_key ()
                            )
                        )
                      done 
                    )
                  else
                    (
                      Simple.printf "Couldn't find this property. Please select again. ↩" ;
                      press_any_key ()
                    )
                )
              with _ -> 
                (
                  Simple.printf "Couldn't find this property. Please select again. ↩" ;
                  press_any_key ()
                )
            )
          else flag := true 
        )
      done
    )

let handle_sell_house game player =
  match destroyable_properties player with
  | [] ->
    (
      Simple.printf "Sorry, you don't have any destroyable property. ↩";
      press_any_key ()
    )
  | l ->
    (
      Simple.printf "Your destroyable properties are listed below:\n";
      ANSITerminal.print_string [ANSITerminal.yellow] (Core.List.to_string ~f:(fun x -> get_name x) l ^ "\n");

      let flag = ref false in 
      while not !flag do 
        (
          Simple.printf "Select a property for selling house or enter 'q' to return to the previous level";

          let name = input () in 
          if name <> "q" then
            (
              try
                (
                  let property = get_property_by_name game name in 

                  if List.exists (fun x -> x = property) l then
                    (
                      let price = get_house_cost property / 2 in 
        
                      let flag2 = ref false in 
                      while not !flag2 do 
                        (
                          Simple.printf "You will collect $%d from selling house on %s. Do you confirm? (y/n)" price name;
                          match input () with 
                          | ""
                          | "y" ->
                            (
                              downgrade_property property;
                              update_money player price;
                              Simple.printf "You collected $%d from selling house on %s. Your current money is $%d. ↩" price name (get_money player);
                              press_any_key ();
                              flag := true;
                              flag2 := true
                            )
                          | "n" -> flag2 := true
                          | _ ->
                            (
                              Simple.printf "Invalid argument. Please enter again. ↩" ;
                              press_any_key ()
                            )
                        )
                      done 
                    )
                  else
                    (
                      Simple.printf "Couldn't find this property. Please select again. ↩" ;
                      press_any_key ()
                    )
                )
              with _ -> 
                (
                  Simple.printf "Couldn't find this property. Please select again. ↩" ;
                  press_any_key ()
                )
            )
          else flag := true
        )
      done 
    )

let rec pay_money_check game player property owed =
  let money = get_money player in 
  if total_assets player + money < owed then
    (
      Simple.printf "Sorry, you don't have enough assets. You are in bankruptcy and will be removed from the game. ↩";
      press_any_key ();
      bankruptcy game player property;
      print_hr ()
    )
  else if money < owed then
    (
      Simple.printf "Sorry, you don't have enough money to pay. You are still short of $%d. ↩" (owed - money);
      press_any_key ();
      Simple.printf "Select your option (mortgage/sell house)";
      match input() with
      | "mortgage" -> 
        (
          handle_mortgage game player;
          print_hr ();
          pay_money_check game player property owed
        )
      | "sell house" -> 
        (
          handle_sell_house game player;
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
        let flag = ref false in 
        while not !flag do 
          (
            Simple.printf "%s is not owned! Do you want to buy it? (y/n)" (get_name property);
            match input () with  
            | ""
            | "y" ->
              (
                (
                  if get_money player >= get_price property then
                    (
                      buy_property player property;
                      Simple.printf "You have bought %s successfully! Your current money is $%d. ↩" (get_name property) (get_money player);
                      press_any_key ()
                    )
                  else
                    (
                      Simple.printf "Sorry, you don't have enough money to buy it. ↩";
                      press_any_key ()
                    )
                );
                flag := true
              )
            | "n" ->
              (
                Simple.printf "Okay, let's begin auction. ↩";
                press_any_key ();
                flag := true
              )
            | _ ->
              (
                Simple.printf "Invalid argument. Please enter again. ↩" ;
                press_any_key ()
              )
          )
        done;

        if is_owner player property then false
        else true
      )
  )
  in
  print_hr ();
  flag

let handle_auction player cur_price =
  Simple.printf "@{<#ffff00>%s's Round: Current bidding price is $%d. ↩@}" (get_player_name player) cur_price;
  press_any_key ();

  let bidding = ref (-1) in 
  let flag = ref false in 
  while not !flag do 
    (
      Simple.printf "Do you want to continue bidding? (y/n)";
      match input () with
      | ""
      | "y" -> 
        (
          (
            if get_money player <= cur_price then
              (
                Simple.printf "Sorry, you don't have enough money to continue bidding. ↩";
                press_any_key ()
              )
            else
              (
                let flag2 = ref false in 
                while not !flag2 do 
                  (
                    Simple.printf "Please input your bidding price (must be higher than current price): ";
                    try
                      (
                        let x = int_of_string @@ input () in 
                        if x > cur_price && x <= get_money player then 
                          (
                            bidding := x;
                            flag2 := true
                          )
                        else if x <= cur_price then
                          (
                            Simple.printf "Your bidding price is not higher than current price. Please enter again. ↩";
                            press_any_key ()
                          )
                        else if x > get_money player then
                          (
                            Simple.printf "Your bidding price is higher than your money. Please enter again. ↩";
                            press_any_key ()
                          )
                      )
                    with _ ->
                      (
                        Simple.printf "Your input is not a valid number. Please enter again. ↩";
                        press_any_key ()
                      )
                  )
                done
              )
          );
          flag := true
        )
      | "n" ->
        (
          Simple.printf "You withdrew from the bidding. ↩";
          press_any_key ();
          flag := true
        )
      | _ ->
        (
          Simple.printf "Invalid argument. Please enter again. ↩" ;
          press_any_key ()
        )
    )
  done;
  !bidding

let rec handle_in_jail_helper game player =
  let left_turns = get_jailed_turns player in 

  Simple.printf "Choose your option to get out of the jail. (roll/use/pay)";
  match input () with 
  | "roll" ->
    (
      Simple.printf "You chose to roll. ↩";
      press_any_key ();
      Simple.printf "Press enter to roll the dices";
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
      let num_cards = num_jail_free_card player in 
      if num_cards = 0 then 
        (
          Simple.printf "You don't have any jail free card. Please select your option again. ↩";
          press_any_key ();
          print_hr ();
          handle_in_jail_helper game player
        )
      else
        (
          let deck_type = List.hd @@ get_jail_free_card player in 
          let old_deck = get_deck game deck_type in 
          let new_deck = old_deck @ [6] in 
          set_deck game new_deck deck_type;
          remove_jail_free_card player;
          Simple.printf "You used a jail free card. Now you are out of jail. ↩";
          press_any_key ();
          print_hr ();
          out_of_jail player;

          Simple.printf "Press enter to roll the dices";
          press_any_key ();
          let dices = roll_dice () in 
          Simple.printf "Your result is %d and %d.\n" (fst dices) (snd dices);
          sum_dices dices
        )
    )
  | "pay" ->
    (
      Simple.printf "You chose to pay $50. ↩";
      press_any_key ();

      if get_money player < 50 then
        (
          Simple.printf "You don't have enough money. Please select your option again. ↩";
          press_any_key ();
          print_hr ();
          handle_in_jail_helper game player
        )
      else 
        (
          update_money player (-50);
          Simple.printf "You have paid $50 and get out of the jail. Your current money is $%d. ↩" (get_money player);
          press_any_key ();
          print_hr ();
          out_of_jail player;
    
          Simple.printf "Press enter to roll the dices";
          press_any_key ();
          let dices = roll_dice () in 
          Simple.printf "Your result is %d and %d.\n" (fst dices) (snd dices);
          sum_dices dices
        )
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

let handle_unmortgage game player =
  match unmortgageable_properties player with
  | [] ->
    (
      Simple.printf "Sorry, you don't have any unmortgageable property. ↩";
      press_any_key ()
    )
  | l ->
    (
      Simple.printf "Your unmortgageable properties are listed below:\n";
      ANSITerminal.print_string [ANSITerminal.yellow] (Core.List.to_string ~f:(fun x -> get_name x) l ^ "\n");

      let flag = ref false in 
      while not !flag do 
        (
          Simple.printf "Select a property for unmortgage or enter 'q' to return to the previous level";

          let name = input () in 
          if name <> "q" then
            (
              try 
                (
                  let property = get_property_by_name game name in 

                  if List.exists (fun x -> x = property) l then
                    (
                      let price = int_of_float @@ 1.1 *. float_of_int (get_mortgaged_price property) in 
        
                      let flag2 = ref false in 
                      while not !flag2 do 
                        (
                          Simple.printf "You will pay $%d for unmortgaging %s. Do you confirm? (y/n)" price name;
                          match input () with 
                          | ""
                          | "y" -> 
                            (
                              if get_money player >= price then
                                (
                                  unset_mortgaged property;
                                  update_money player (-price);
                                  Simple.printf "You paid $%d for unmortgage. Your current money is $%d. ↩" price (get_money player);
                                  press_any_key ();
                                  flag := true;
                                  flag2 := true
                                )
                              else
                                (
                                  Simple.printf "You don't have enough money to unmortgage. ↩";
                                  press_any_key ();
                                  flag2 := true
                                )
                            )
                          | "n" -> flag2 := true
                          | _ -> 
                            (
                              Simple.printf "Invalid argument. Please enter again. ↩" ;
                              press_any_key ()
                            )
                        )
                      done 
                    )
                  else
                    (
                      Simple.printf "Couldn't find this property. Please select again. ↩" ;
                      press_any_key ()
                    )
                )
              with _ -> 
                (
                  Simple.printf "Couldn't find this property. Please select again. ↩" ;
                  press_any_key ()
                )
            )
          else flag := true
        )
      done 
    )

let handle_build_house game player =
  match buidable_properties player with
  | [] ->
    (
      Simple.printf "Sorry, you don't have any buildable property. ↩";
      press_any_key ()
    )
  | l ->
    (
      Simple.printf "Your buildable properties are listed below:\n";
      ANSITerminal.print_string [ANSITerminal.yellow] (Core.List.to_string ~f:(fun x -> get_name x) l ^ "\n");

      let flag = ref false in 
      while not !flag do 
        (
          Simple.printf "Select a property for building house or enter 'q' to return to the previous level";

          let name = input () in 
          if name <> "q" then
            (
              try
                (
                  let property = get_property_by_name game name in 

                  if List.exists (fun x -> x = property) l then
                    (
                      let price = get_house_cost property in 

                      let flag2 = ref false in 
                      while not !flag2 do 
                        (
                          Simple.printf "You will pay $%d for building house on %s. Do you confirm? (y/n)" price name;
                          match input () with
                          | ""
                          | "y" -> 
                            (
                              if get_money player >= price then
                                (
                                  upgrade_property property;
                                  update_money player (-price);
                                  Simple.printf "You paid $%d for building house on %s. Your current money is $%d. ↩" price name (get_money player);
                                  press_any_key ();
                                  flag := true;
                                  flag2 := true
                                )
                              else
                                (
                                  Simple.printf "You don't have enough money to build house. ↩";
                                  press_any_key ();
                                  flag2 := true
                                )
                            )
                          | "n" -> flag2 := true
                          | _ -> 
                            (
                              Simple.printf "Invalid argument. Please enter again. ↩" ;
                              press_any_key ()
                            )
                        )
                      done 
                    )
                  else
                    (
                      Simple.printf "Couldn't find this property. Please select again. ↩" ;
                      press_any_key ()
                    )
                )
              with _ -> 
                (
                  Simple.printf "Couldn't find this property. Please select again. ↩" ;
                  press_any_key ()
                )
            )
          else flag := true
        )
      done 
    )

let rec handle_actions game player =
  Simple.printf "Please select your action. (skip/mortgage/unmortgage/build house/sell house)";
  match input () with 
  | "" -> print_hr ()
  | "skip" -> print_hr ()
  | "mortgage" -> 
    (
      handle_mortgage game player;
      print_hr ();
      handle_actions game player
    )
  | "unmortgage" ->
    (
      handle_unmortgage game player;
      print_hr ();
      handle_actions game player
    )
  | "build house" -> 
    (
      handle_build_house game player;
      print_hr ();
      handle_actions game player
    )
  | "sell house" -> 
    (
      handle_sell_house game player;
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
