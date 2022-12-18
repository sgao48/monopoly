open Property
open Player
open Game
open Card
open Print
open Spectrum

let auction game property =
  Simple.printf "Auction for %s started!!!\n" (get_name property);

  let auctioneer_list = ref (get_player_list game) in
  let cur_price = ref 0 in

  (while List.length !auctioneer_list > 1 do
    let cur_auctioneer = List.hd !auctioneer_list in 
    let new_price =
      if get_is_real cur_auctioneer then 
        Real.handle_auction cur_auctioneer !cur_price
      else
        Ai.handle_auction cur_auctioneer property !cur_price
    in
    
    if new_price = (-1) then
      auctioneer_list := List.tl !auctioneer_list
    else
      (
        cur_price := new_price;
        auctioneer_list := move_front_to_back !auctioneer_list
      )
  done);

  let winner = List.hd !auctioneer_list in 
  Simple.printf "Congratulations! %s has won the auction. The deal price is $%d.\n" (get_player_name winner) !cur_price;
  buy_property winner property;
  update_money winner (get_price property - !cur_price);
  Simple.printf "The current money of %s is $%d.\n" (get_player_name winner) (get_money winner);

  print_hr ()

let handle_property game player property =
  let is_real = get_is_real player in 
  let flag = 
    if is_real then Real.handle_on_property game player property 
    else Ai.handle_on_property game player property
  in 
  if flag then auction game property

let handle_go_to_jail game player =
  Simple.printf "Oops, you are arrested! ↩";
  press_any_key ();
  arrested game player;
  print_hr ()

let handle_tax game player property =
  let tax = calculate_rent property in 
  Simple.printf "You have to pay $%d for %s. ↩" tax (get_name property);
  press_any_key ();
  
  (
    if get_is_real player then
      Real.pay_money_check game player property tax
    else
      Ai.pay_money_check game player property tax
  );

  if in_game game player then
    (
      update_money player (-tax);
      Simple.printf "You have paid the tax. Your current money is $%d. ↩" (get_money player);
      press_any_key ();
      print_hr ()
    )

let rec handle_effect_helper game player effect_list =
  match effect_list with
  | [] -> ()
  | hd :: tl -> 
    (
      match hd with
      | Some giver, money, None ->
        (
          (
            if get_is_real player then
              Real.pay_money_check game player (get_position player) money
            else
              Ai.pay_money_check game player (get_position player) money
          );

          if in_game game player then
            (
              update_money giver (-money);
              Simple.printf "You lost $%d. Your current money is $%d. ↩" money (get_money giver);
              press_any_key ()
            )
        )
      | None, money, Some receiver ->
        (
          update_money receiver money;
          Simple.printf "You collected $%d. Your current money is $%d. ↩" money (get_money receiver);
          press_any_key ()
        )
      | Some giver, money, Some receiver ->
        (
          (
            if giver = player then
              (
                if get_is_real player then 
                  Real.pay_money_check game player (get_position player) money
                else
                  Ai.pay_money_check game player (get_position player) money
              )
          );

          if in_game game player then
            (
              update_money giver (-money);
              update_money receiver money;
              (
                if giver = player then 
                  Simple.printf "You give $%d to %s. Your current money is $%d. ↩" money (get_player_name receiver) (get_money giver)
                else
                  Simple.printf "You collected $%d from %s. Your current money is $%d. ↩" money (get_player_name giver) (get_money receiver)
              );
              press_any_key ()
            )
        )
      | _, _, _ -> ()
    );
    handle_effect_helper game player tl

let handle_effect game player card =
  handle_effect_helper game player (get_card_effect card game)

let rec draw_card game player deck_type =
  let deck = get_deck game deck_type in 
  let card_id = List.hd deck in 
  let card = get_card deck_type card_id in 
  
  (if get_card_text card = "Get Out of Jail Free ↩" then 
    set_deck game (List.tl deck) deck_type
  else
    set_deck game (move_front_to_back deck) deck_type);
  ANSITerminal.print_string [] (get_card_text card);
  press_any_key ();

  let prev_pos = get_position player in 
  handle_effect game player card;
  print_hr ();
  let cur_pos = get_position player in 
  if prev_pos <> cur_pos then
  (
    if is_property cur_pos then 
      handle_property game player cur_pos
    else if is_go_to_jail cur_pos then  
      handle_go_to_jail game player
    else if is_tax cur_pos then
      handle_tax game player cur_pos
    else if is_chance cur_pos then
      draw_card game player "chance"
    else if is_community_chest cur_pos then 
      draw_card game player "community chest"
  )

let rec normal_round game player = 
  Simple.printf "ROUND %d: Press enter to roll the dices" (get_doubles player + 1);
  press_any_key ();

  let dices = roll_dice () in 
  Simple.printf "Your result is %d and %d.\n" (fst dices) (snd dices);

  (
    if is_doubles dices then 
      (
        add_doubles player;
        if get_doubles player = 3 then 
          (
            Simple.printf "Hey! You are cheating! You are arrested! ↩";
            press_any_key ();
            arrested game player;
            reset_doubles player;
            print_hr ()
          )
      )
    else reset_doubles player 
  );

  if not @@ is_in_jail player then 
    (
      move_by_steps game player (sum_dices dices) true;
      print_hr ();

      let cur_pos = get_position player in 
      (
        if is_property cur_pos then 
          handle_property game player cur_pos
        else if is_go_to_jail cur_pos then  
          handle_go_to_jail game player
        else if is_tax cur_pos then
          handle_tax game player cur_pos
        else if is_chance cur_pos then
          draw_card game player "chance"
        else if is_community_chest cur_pos then 
          draw_card game player "community chest"
      );

      (
        if not @@ is_in_jail player && in_game game player then
          (
            (if get_is_real player then 
              Real.handle_actions game player
            else 
              Ai.handle_actions game player);

            if is_doubles dices then
              (
                Simple.printf "Due to doubles, You can roll another round of dice. ↩";
                press_any_key ();
                print_hr ();
                normal_round game player
              )
          )
      )
    )

let jail_round game player = 
  let steps = 
    if get_is_real player then 
      Real.handle_in_jail game player
    else
      Ai.handle_in_jail game player
  in
  if steps <> (-1) then
    (
      move_by_steps game player steps true;
      print_hr ();

      let cur_pos = get_position player in 
      (
        if is_property cur_pos then 
          handle_property game player cur_pos
        else if is_go_to_jail cur_pos then  
          handle_go_to_jail game player
        else if is_tax cur_pos then
          handle_tax game player cur_pos
        else if is_chance cur_pos then
          draw_card game player "chance"
        else if is_community_chest cur_pos then 
          draw_card game player "community chest"
      );

      (
        if not @@ is_in_jail player && in_game game player then
          (
            if get_is_real player then 
              Real.handle_actions game player
            else 
              Ai.handle_actions game player
          )
      )
    )

let round game = 
  print_hr ();

  let player = get_current_player game in 
  (if get_is_real player then
    ANSITerminal.print_string [] ("Hello! " ^ get_player_name player ^ ". This is your current status:" ^ "\n")
  else
    ANSITerminal.print_string [] (get_player_name player ^ " current status:" ^ "\n"));
  print_player_info player;
  print_hr ();

  match is_in_jail player with
  | true -> jail_round game player
  | false -> normal_round game player

let start_game num_ai = 
  let game_board = Gameboard.game_board in 
  let game = create_game game_board in 

  players_setup game num_ai;
  Simple.printf "The first player is @{<#ffff00>%s@}.\n" (get_player_name @@ get_current_player game);
  Simple.printf "Let's begin the GAME!!! ↩";
  press_any_key ();
  
  let game_over = ref false in 
  (while not !game_over do 
    print_board game;

    round game;
    move_to_next_player game;

    Simple.printf "Your turn is over. ↩";
    press_any_key ();

    if only_one_player game then
      (
        game_over := true;
        print_hr ();
        Simple.printf "Game is over!!! %s is the winner!!! ↩" (game |> get_player_list |> List.hd |> get_player_name);
        press_any_key ()
      )
  done);

  print_ending ();
  Simple.printf "Woo! Hope you enjoyed Monopoly! ʕ•ᴥ•ʔ \nPress enter to exit the game now ↩";
  press_any_key ()

let () =
  print_opening ();
  Simple.printf "Press any key to start game or 'q' to exit:";
  let str = input () in 
  if str <> "q" then
    (
      let num_ai = ref 0 in 
      let flag = ref false in 
      while not !flag do 
        (
          Simple.printf "How many computer players do you want to add? (0-3)";
          try
            (
              let x = int_of_string @@ input () in
              if x >= 0 && x <= 3 then
                (
                  num_ai := x;
                  flag := true
                )
              else
                (
                  Simple.printf "Invalid number of computer players. Please enter again. ↩";
                  press_any_key ()
                )
            )
          with _ ->
            (
              Simple.printf "Your input is not a valid number. Please enter again. ↩";
              press_any_key ()
            )
        )
      done;

      start_game !num_ai
    )
