open Property
open Game

let go = create_property "Pass Go" "go"
  (-1) [|0|] (-1)

let med_avenue = create_property "Med. Avenue" "brown"
  60 [|2; 10; 30; 90; 160; 250|] 50

let community_chest_1 = create_property "Community Chest I" "community chest"
  (-1) [|0|] (-1)

let baltic_avenue = create_property "Baltic Avenue" "brown"
  60 [| 4; 20; 60; 180; 320; 450 |] 50

let income_tax = create_property "Income Tax" "tax"
  (-1) [|200|] (-1)

let reading_railroad = create_property "Reading Railroad" "railroad"
  200 [|25|] (-1)

let oriental_avenue = create_property "Oriental Avenue" "light blue"
  100 [|6; 30; 90; 270; 400; 550|] 50

let chance_1 = create_property "Chance I" "chance"
  (-1) [|0|] (-1)

let vermont_avenue = create_property "Vermont Avenue" "light blue"
  100 [|6; 30; 90; 270; 400; 550|] 50

let conn_avenue = create_property "Conn. Avenue" "light blue"
  120 [|8; 40; 100; 300; 450; 600|] 50

let jail = create_property "Jail" "jail"
  (-1) [|0|] (-1)

let st_chars_place = create_property "St. Chas. Place" "pink"
  140 [|10; 50; 150; 450; 625; 750|] 100

let electric_company = create_property "Electric Company" "utilities"
  150 [|0|] 0

let states_avenue = create_property "States Avenue" "pink"
  140 [|10; 50; 150; 450; 625; 750|] 100

let virginia_avenue = create_property "Virginia Avenue" "pink"
  160 [|12; 60; 180; 500; 700; 900|] 100

let penn_railroad = create_property "Penn. Railroad" "railroad"
  200 [|25|] (-1)

let st_james_place = create_property "St. James Place" "orange"
  180 [|14; 70; 200; 250; 750; 950|] 100

let community_chest_2 = create_property "Community Chest II" "community chest"
  (-1) [|0|] (-1)

let tennessee_avenue = create_property "Tennessee Avenue" "orange"
  180 [|14; 70; 200; 250; 750; 950|] 100

let new_york_avenue = create_property "New York Avenue" "orange"
  200 [|16; 80; 220; 600; 800; 1000|] 100

let free_parking = create_property "Free Parking" "free parking"
  (-1) [|0|] (-1)

let kentucky_avenue = create_property "Kentucky Avenue" "red"
  220 [|18; 90; 250; 700; 875; 1050|] 150

let chance_2 = create_property "Chance II" "chance"
  (-1) [|0|] (-1)

let indiana_avenue = create_property "Indiana Avenue" "red"
  220 [|18; 90; 250; 700; 875; 1050|] 150

let illinois_avenue = create_property "Illinois Avenue" "red"
  240 [|20; 100; 300; 750; 925; 1100|] 150

let bo_railroad = create_property "B. & O. Railroad" "railroad"
  200 [|25|] 0

let atlantic_avenue = create_property "Atlantic Avenue" "yellow"
  260 [|22; 110; 330; 800; 975; 1150|] 150
  
let ventnor_avenue = create_property "Ventnor Avenue" "yellow"
  260 [|22; 110; 330; 800; 975; 1150|] 150

let water_works = create_property "Water Works" "utilities"
  150 [|0|] 0

let marvin_gardens = create_property "Marvin Gardens" "yellow"
  280 [|24; 120; 360; 850; 1025; 1200|] 150

let go_to_jail = create_property "Go To Jail" "go to jail"
  (-1) [|0|] (-1)

let pacific_avenue = create_property "Pacific Avenue" "green"
  300 [|26; 130; 390; 900; 1100; 1275|] 200

let n_carol_avenue = create_property "N. Carol. Avenue" "green"
  300 [|26; 130; 390; 900; 1100; 1275|] 200

let community_chest_3 = create_property "Community Chest III" "community chest"
  (-1) [|0|] (-1)

let penn_avenue = create_property "Penn. Avenue" "green"
  320 [|28; 150; 450; 1000; 1200; 1400|] 200

let short_line = create_property "Short Line" "railroad"
  200 [|25|] 0

let chance_3 = create_property "Chance III" "chance"
  (-1) [|0|] (-1)

let park_place = create_property "Park Place" "dark blue"
  350 [|35; 175; 500; 1100; 1300; 1500|] 200

let luxury_tax = create_property "Luxury Tax" "tax"
  (-1) [|75|] (-1)

let boardwalk = create_property "Boardwalk" "dark blue"
  400 [|50; 200; 600; 1400; 1700; 2000|] 200

let game_board = 
  create_gameboard
  [
    go;
    med_avenue;
    community_chest_1;
    baltic_avenue;
    income_tax;
    reading_railroad;
    oriental_avenue;
    chance_1;
    vermont_avenue;
    conn_avenue;
    jail;
    st_chars_place;
    electric_company;
    states_avenue;
    virginia_avenue;
    penn_railroad;
    st_james_place;
    community_chest_2;
    tennessee_avenue;
    new_york_avenue;
    free_parking;
    kentucky_avenue;
    chance_2;
    indiana_avenue;
    illinois_avenue;
    bo_railroad;
    atlantic_avenue;
    ventnor_avenue;
    water_works;
    marvin_gardens;
    go_to_jail;
    pacific_avenue;
    n_carol_avenue;
    community_chest_3;
    penn_avenue;
    short_line;
    chance_3;
    park_place;
    luxury_tax;
    boardwalk;
  ]
