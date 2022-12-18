open OUnit2
open Property
open Player
open Game

let game = create_game Gameboard.game_board

let dice1 = (3, 2)
let dice2 = (3, 3)

let player = create_player true "player" Gameboard.go
let ai = create_player false "ai" Gameboard.go

let initialize () =
  add_player game player;
  add_player game ai

let med_ave = get_property_by_name game "Med. Avenue"
let boardwalk = get_property_by_name game "Boardwalk"
let baltic_ave = get_property_by_name game "Baltic Avenue"
let pass_go = get_property_by_name game "Pass Go"
let income_tax = get_property_by_name game "Income Tax"
let elec_company = get_property_by_name game "Electric Company"
let reading_rr = get_property_by_name game "Reading Railroad"
let jail = get_property_by_name game "Jail"
let go_to_jail = get_property_by_name game "Go To Jail"
let chance1 = get_property_by_name game "Chance I"
let comm_chest1 = get_property_by_name game "Community Chest I"

(* Property *)
let test_property_main _ =
  assert_equal true @@ is_property med_ave;
  assert_equal false @@ is_property pass_go;
  assert_equal true @@ is_tax income_tax;
  assert_equal false @@ is_tax pass_go;
  assert_equal true @@ is_utilities elec_company;
  assert_equal false @@ is_utilities pass_go;
  assert_equal true @@ is_railroad reading_rr;
  assert_equal false @@ is_railroad pass_go;
  assert_equal true @@ is_jail jail;
  assert_equal false @@ is_jail pass_go;
  assert_equal true @@ is_go_to_jail go_to_jail;
  assert_equal false @@ is_go_to_jail pass_go;
  assert_equal true @@ is_chance chance1;
  assert_equal false @@ is_chance pass_go;
  assert_equal true @@ is_community_chest comm_chest1;
  assert_equal false @@ is_community_chest pass_go;
  assert_equal false @@ is_owned med_ave;
  assert_equal true @@ (set_owner med_ave "player"; is_owned med_ave);
  assert_equal true @@ can_be_upgraded med_ave;
  assert_equal false @@ can_be_upgraded pass_go;
  assert_equal false @@ can_be_upgraded reading_rr;
  assert_equal 1 @@ (upgrade_property baltic_ave; num_houses baltic_ave);
  assert_equal 2 @@ (upgrade_property baltic_ave; num_houses baltic_ave);
  assert_equal 3 @@ (upgrade_property baltic_ave; num_houses baltic_ave);
  assert_equal 4 @@ (upgrade_property baltic_ave; num_houses baltic_ave);
  assert_equal 5 @@ (upgrade_property baltic_ave; num_houses baltic_ave);
  assert_equal 5 @@ (upgrade_property baltic_ave; num_houses baltic_ave);
  assert_equal 0 @@ (upgrade_property pass_go; num_houses pass_go);
  assert_equal 0 @@ (downgrade_property pass_go; num_houses pass_go);
  assert_equal 0 @@ (upgrade_property reading_rr; num_houses reading_rr);
  assert_equal 0 @@ (downgrade_property reading_rr; num_houses reading_rr);
  assert_equal false @@ can_be_upgraded baltic_ave;
  assert_equal true @@ (downgrade_property baltic_ave; downgrade_property baltic_ave; downgrade_property baltic_ave; 
                        downgrade_property baltic_ave; downgrade_property baltic_ave; can_be_upgraded baltic_ave);
  assert_equal 0 @@ (downgrade_property baltic_ave; num_houses baltic_ave);
  assert_equal true @@ can_be_upgraded_type @@ get_type med_ave;
  assert_equal false @@ can_be_upgraded_type @@ get_type pass_go;
  assert_equal 2 @@ calculate_rent med_ave;
  assert_equal 0 @@ (upgrade_property med_ave; downgrade_property med_ave; num_houses med_ave);
  assert_equal 2 @@ num_for_monopoly med_ave;
  assert_equal 3 @@ num_for_monopoly @@ get_property_by_name game "Oriental Avenue";
  assert_equal 3 @@ num_for_monopoly @@ get_property_by_name game "States Avenue";
  assert_equal 3 @@ num_for_monopoly @@ get_property_by_name game "New York Avenue";
  assert_equal 3 @@ num_for_monopoly @@ get_property_by_name game "Indiana Avenue";
  assert_equal 3 @@ num_for_monopoly @@ get_property_by_name game "Marvin Gardens";
  assert_equal 3 @@ num_for_monopoly @@ get_property_by_name game "Pacific Avenue";
  assert_equal (-1) @@ num_for_monopoly pass_go;
  assert_equal 0 @@ (upgrade_property med_ave; reset_level med_ave; num_houses med_ave);
  assert_equal false @@ (release_property med_ave; is_owned med_ave)

let test_property_helper _ =
  assert_equal "Med. Avenue" @@ get_name med_ave;
  assert_equal 60 @@ get_price med_ave;
  assert_equal 50 @@ get_house_cost med_ave;
  assert_equal "player" @@ (set_owner med_ave "player"; get_owner med_ave);
  assert_equal 30 @@ get_mortgaged_price med_ave;
  assert_equal 30 @@ get_value med_ave;
  assert_equal false @@ is_mortgaged med_ave;
  assert_equal true @@ (set_mortgaged med_ave; is_mortgaged med_ave);
  assert_equal 0 @@ get_value med_ave;
  assert_equal false @@ (unset_mortgaged med_ave; is_mortgaged med_ave)

let property_tests = "Property" >: test_list [
  "Property Main" >:: test_property_main;
  "Property Helper" >:: test_property_helper;
  ]

(* Player *)
let test_player_main _ =
  assert_equal false @@ is_doubles dice1;
  assert_equal true @@ is_doubles dice2;
  assert_equal 5 @@ sum_dices dice1;
  assert_equal 0 @@ (reset_doubles player; get_doubles player);
  assert_equal false @@ is_in_jail player;
  assert_equal true @@ (move_to_jail player; is_in_jail player);
  assert_equal false @@ (out_of_jail player; is_in_jail player);
  assert_equal 2 @@ (move_to_jail player; next_jailed_turn player; get_jailed_turns player);
  assert_equal "Reading Railroad" @@ (move_by_position player reading_rr; get_name @@ get_position player);
  assert_equal 1600 @@ (update_money player 100; get_money player);
  assert_equal 1 @@ (add_monopoly player (get_type med_ave); List.length @@ get_monopolies player);
  assert_equal 1 @@ (add_property player med_ave; count_owned_properties player med_ave);
  assert_equal 1 @@ (check_monopoly player boardwalk; List.length @@ get_monopolies player);
  assert_equal false @@ is_monopolied player boardwalk;
  assert_equal true @@ is_monopolied player med_ave;
  assert_equal 0 @@ (remove_property player med_ave; List.length @@ get_properties player);
  assert_equal 1 @@ (buy_property player med_ave; List.length @@ get_properties player);
  assert_equal 4 @@ rent_to_pay med_ave player;
  assert_equal 25 @@ (add_property player reading_rr; rent_to_pay reading_rr player);
  assert_equal true @@ is_built_evenly player med_ave true;
  assert_equal true @@ is_built_evenly player med_ave false;
  assert_equal 1 @@ List.length @@ buidable_properties player;
  assert_equal 130 @@ total_assets player;
  assert_equal true @@ no_house_on_monopoly player med_ave;
  assert_equal true @@ is_owner player med_ave;
  assert_equal true @@ is_mortgageable player med_ave;
  assert_equal 2 @@ List.length @@ mortgageable_properties player;
  assert_equal 0 @@ List.length @@ unmortgageable_properties player;
  assert_equal 1 @@ (return_to_bank player med_ave; List.length @@ get_properties player);
  assert_equal 0 @@ (buy_property player med_ave; return_all_properties player; List.length @@ get_properties player);
  assert_equal 1 @@ (buy_property ai med_ave; swap_owner ai player med_ave; List.length @@ get_properties player);
  assert_equal 2 @@ (buy_property ai boardwalk; give_all_properties ai player; List.length @@ get_properties player);
  assert_equal 0 @@ List.length @@ destroyable_properties player

let test_player_helper _ =
  assert_equal 0 @@ List.length @@ get_jail_free_card player;
  assert_equal 0 @@ num_jail_free_card player;
  assert_equal 1 @@ (add_jail_free_card player "chance"; num_jail_free_card player);
  assert_equal 0 @@ (remove_jail_free_card player; num_jail_free_card player);
  assert_equal "player" @@ get_player_name player;
  assert_equal 1500 @@ (let player2 = create_player true "player2" Gameboard.go in get_money player2);
  assert_equal true @@ get_is_real player;
  assert_equal false @@ get_is_real ai
 
let player_tests = "Player" >: test_list [
  "Player Main" >:: test_player_main;
  "Player Helper" >:: test_player_helper;
  ]

(* Game *)
let test_game_main _ =
  assert_equal false @@ only_one_player game;
  assert_equal true @@ in_game game player;
  assert_equal 1 @@ get_property_index game med_ave;
  assert_equal "Med. Avenue" @@ get_name @@ get_property_at_index game 1;
  assert_equal "Med. Avenue" @@ (move_by_steps game ai 1 true; get_name @@ get_position ai);
  assert_equal "Pass Go" @@ (move_by_steps game ai 39 true; get_name @@ get_position ai);
  assert_equal false @@ (remove_player game ai; in_game game ai);
  assert_equal 3 @@ (arrested game player; get_jailed_turns player)

let test_game_helper _ =
  assert_equal 16 @@ List.length @@ get_deck game "chance";
  assert_equal 16 @@ List.length @@ get_deck game "community chest";
  assert_equal 0 @@ (set_deck game [] "chance"; List.length @@ get_deck game "chance");
  assert_equal 0 @@ (set_deck game [] "community chest"; List.length @@ get_deck game "community chest");
  assert_equal 2 @@ (let list = move_front_to_back [1; 2] in List.hd list);
  assert_equal 0 @@ List.length @@ move_front_to_back [];
  assert_equal "player" @@ (move_to_next_player game; get_player_name @@ get_current_player game);
  assert_equal 2 @@ List.length @@ get_player_list game;
  assert_equal "Pass Go" @@ get_name @@ start_position game;
  assert_equal 2 @@ num_players game;
  assert_equal "player" @@ get_player_name @@ get_player_by_name game "player";
  assert_equal "player" @@ get_player_name @@ get_property_owner game med_ave;
  assert_equal "Jail" @@ get_name @@ get_jail game

let game_tests = "Game" >: test_list [
  "Game Main" >:: test_game_main;
  "Game Helper" >:: test_game_helper;
  ]

(* Test *)
let series = "Project Tests" >::: [
    property_tests;
    player_tests;
    game_tests
  ]

let () = 
  initialize ();
  run_test_tt_main series
