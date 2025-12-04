/* Rescue the princess, by Group 9. */
:- initialization((
    write('==============================================='), nl,
    write('              Rescue the Princess              '), nl,
    write('==============================================='), nl,
    write('Type "start." to begin your adventure.'), nl,
    write('Type "help." to see all available commands.'), nl,
    nl
)).

/* Pyperplan */
:- [pyperplan_runner].

/* Initialize The Game */
:- dynamic gaming/1.
:- dynamic i_am_at/1, item_count/2, shop_item/3, equipped/2.
:- dynamic player_status/6, player_equipments/5, hunger_thirst_penalty/1.
:- dynamic wish_count/1, got_crystal_sword/1.
:- dynamic chest_opened/1, cave_collapsed/1.
:- dynamic in_desert_maze/0, guard_stuck/1.
:- dynamic desert_player_pos/2, desert_guard_pos/2.
:- dynamic basement_door_open/1, basement_treasure/1.
:- dynamic bear_defeated/1, dragon_defeated/1.
:- dynamic monster/5, monster_here/2, monster_status/4, defeated_monster/1.
:- dynamic in_combat/1, current_monster/4, combat_turn/1.

init :-
    /* Clean all dynamic predicate */
    retractall(gaming(_)),

    retractall(i_am_at(_)),
    retractall(item_count(_,_)),
    retractall(shop_item(_,_,_)),
    retractall(equipped(_,_)),

    retractall(player_status(_,_,_,_,_,_)),
    retractall(player_equipments(_,_,_,_,_)),
    retractall(hunger_thirst_penalty(_)),

    retractall(wish_count(_)),
    retractall(got_crystal_sword(_)),

    retractall(chest_opened(_)),
    retractall(cave_collapsed(_)),

    retractall(in_desert_maze),
    retractall(guard_stuck(_)),
    retractall(desert_player_pos(_,_)),
    retractall(desert_guard_pos(_,_)),

    retractall(basement_door_open(_)),
    retractall(basement_treasure(_)),

    retractall(bear_defeated(_)),
    retractall(dragon_defeated(_)),

    retractall(monster_here(_,_)),
    retractall(monster_status(_,_,_,_)),
    retractall(defeated_monster(_)),

    retractall(in_combat(_)),
    retractall(current_monster(_,_,_,_)),
    retractall(combat_turn(_)),

    /* Game Status */
    assert(gaming(true)),

    /* Player */
    assert(player_status(100, 10, 5, 100, 100, 500)),
    assert(hunger_thirst_penalty(false)),
    assert(player_equipments(null, null, null, null, null)),
    assert(i_am_at(adventurers_hall)),

    /* Wish count */
    assert(wish_count(0)),
    assert(got_crystal_sword(false)),

    /* Cave status */
    assert(chest_opened(false)),
    assert(cave_collapsed(false)),

    /* Basement door status */
    assert(basement_door_open(false)),
    assert(basement_treasure(false)),

    /* Boss survival */
    assert(bear_defeated(false)),
    assert(dragon_defeated(false)),

    /* Item inventories */
    assert(shop_item(weapon_shop, iron_sword, 1)),
    assert(shop_item(weapon_shop, iron_helmet, 1)),
    assert(shop_item(weapon_shop, iron_armor, 1)),
    assert(shop_item(weapon_shop, iron_boot, 1)),
    assert(shop_item(weapon_shop, oil_lamp, 1)),

    assert(shop_item(market, apple, 100)),
    assert(shop_item(market, bread, 100)),
    assert(shop_item(market, water, 100)),
    assert(shop_item(market, potion, 100)),

    assert(shop_item(river, crystal_sword, 1)),
    assert(shop_item(deep_cave, obsidian_helmet, 1)),
    assert(shop_item(deep_cave, obsidian_armor, 1)),
    assert(shop_item(deep_cave, obsidian_boot, 1)),
    assert(shop_item(basement, wind_ring, 1)),
    assert(shop_item(basement, elixir, 5)),

    /* Monsters */
    retractall(monster_here(_,_)),
    retractall(monster_status(_,_,_,_)),
    retractall(defeated_monster(_)),
    forall(monster(Name, Location, Health, _, _),
           assert(monster_status(Name, Location, Health, active))).

/* Area Connection */
path(adventurers_hall, east, weapon_shop).
path(adventurers_hall, west, market).
path(adventurers_hall, south, forest).

path(weapon_shop, west, adventurers_hall).

path(market, east, adventurers_hall).

path(forest, north, adventurers_hall).
path(forest, east, river).
path(forest, west, cave).
path(forest, south, desert).

path(river, west, forest).

path(cave, east, forest).
path(cave, down, deep_cave).
path(deep_cave, up, cave).

path(desert, north, forest).
path(desert, south, castle_hall).

path(castle_hall, north, desert).
path(castle_hall, up, attic).
path(attic, down, castle_hall).
path(castle_hall, down, basement).
path(basement, up, castle_hall).

/* Area Descriptions */
describe(adventurers_hall) :-
    write('You are in the Adventurer''s Hall.'), nl,
    write('Your task is to defeat the dragon and rescue the princess.'), nl,
    write('You can sell loot here! You have: '), nl,
    list_loot, nl,
    write('Exits lead: west to Market, east to Weapon Shop, south to Forest.'), nl.

describe(market) :-
    write('Market: Stalls filled with food, drinks, and supplies.'), nl,
    write('Vendors call out their prices. You can purchase restored items here.'), nl,
    (list_shop_items(market) -> true; write('Exit east to Adventurer''s Hall.')), nl.

describe(weapon_shop) :-
    write('Weapon Shop: Various weapons and armor are displayed on the walls.'), nl,
    write('The shopkeeper nods as you enter. You can purchase weaponry here.'), nl,
    (list_shop_items(weapon_shop) -> true; write('Exit west to Adventurer''s Hall.')), nl.

describe(forest) :-
    write('Dark Forest: Tall trees create a canopy overhead, filtering the sunlight.'), nl,
    write('There seems to be danger hidden here.'), nl,
    write('Exits lead: north to Adventurer''s Hall, east to River, west to Cave, south to Desert.'), nl.

describe(river) :-
    write('Crystal River: A very ancient river, with the protection of elf according to legend.'), nl,
    write('The water looks refreshing. Take a break here.'), nl,
    write('I heard that if you throw 100 gold coins into the river, you can make a wish to the elves.'), nl,
    write('But I''m not sure whether it''s true...'), nl,
    write('Exit west to Forest.').

describe(cave) :-
    write('Whispering Cave: A dark opening in the mountainside, cool air wafts from within.'), nl,
    write('Strange echoes can be heard from the depths. The sole source of light gave me a little bit of comfort.'), nl,
    write('Exit east to Forest. Use d. to explore the depths of the cave.').

describe(deep_cave) :-
    (cave_collapsed(true) ->
        write('The cave has completely collapsed! You cannot enter anymore.'), nl
    ;
        defeated_monster(bear) ->
        (chest_opened(true) ->
            write('The treasure chest has been opened and the cave is collapsing! Escape quickly!'), nl,
            write('Exits lead: up to Cave.'), nl
        ;
            write('The giant bear has been defeated! The treasure chest is waiting to be opened.'), nl,
            write('Use "open." to open the treasure chest.'), nl,
            write('Exits lead: up to Cave.'), nl
        )
    ;
        write('Wow! The minerals deep in the cave emitted a beautiful light.'), nl,
        write('You see a treasure chest and are about to open it'), nl,
        write('Breathing sounds came from behind. Oh no, it was a huge bear!'), nl,
        write('Exits lead: up to Cave.'), nl
    ).

describe(desert) :-
    write('Scorching Desert: A vast expanse of golden sand under the scorching sun.'), nl,
    write('You got stuck in the quicksand and fell into the desert maze.'), nl,
    write('Only by finding the exit of the maze can one cross the desert...'), nl.

describe(castle_hall) :-
    write('Dragon Castle: The ancient dragon''s castle.'), nl,
    write('It is said that the power of the dragon is extremely strong.'), nl,
    write('The princess must be trapped in the attic.'), nl,
    write('A huge dragon statue stands in the middle, which seems to be able to rotate...'), nl.

describe(basement) :-
    write('You saw a locked treasure chest, so you looked around...'), nl,
    write('Several oil paintings are displayed in the basement, seem to depict the stories of past humans slaying dragons...'), nl,
    write('You look closer and see four paintings arranged in a row:'), nl,
    write('The first painting depicts a baby dragon drinking the river with an adult.'), nl,
    write('The second painting depicts a knight fighting a dragon that has six heads in the cave.'), nl,
    write('The third painting depicts three dragons circling over the town.'), nl,
    write('The last painting depicts five soldiers show the dragon heads in their hands').

describe(attic) :-
    (defeated_monster(dragon) ->
        write('The dragon has been defeated! Quickly use the key to rescue the princess!'), nl,
        write('Use the key to open the cage: use_key.'), nl
    ;
        write('The princess was trapped in a cage! You try to break the cage.'), nl,
        write('The huge commotion alarmed the dragon!'), nl
    ).

/* Start The Game */
start :-
    (gaming(true) ->
        write("This adventure is not over yet. Please continue your adventure!"), nl
    ;
        init,
        write('Welcome to this world! Hero!'), nl,
        write('You start with 500 gold. Explore this world to your heart''s content!'), nl,
        nl,
        look
    ).

/* Look Around */
look :-
    i_am_at(Place),
    describe(Place),
    nl.

/* Path Conditions */
check_path_condition(forest, west, cave) :-
    (equipped(left, oil_lamp) ; equipped(right, oil_lamp)),
    !.

check_path_condition(forest, west, cave) :-
    write('The cave entrance is too dark to see! You need to equip an oil_lamp to enter.'), nl,
    write('Perhaps it can be available in the Weapon Shop...'), nl,
    !,
    fail.

check_path_condition(cave, down, deep_cave) :-
    cave_collapsed(true),
    write('The cave entrance has collapsed! You cannot enter anymore.'), nl,
    !,
    fail.

check_path_condition(castle_hall, down, basement) :-
    basement_door_open(true),
    !.

check_path_condition(castle_hall, down, basement) :-
    write('It seems there is a hidden door leading to the basement. Think of a way to find it...'), nl,
    !,
    fail.

check_path_condition(_, _, _).

/* Player Movement */
n :- go(north).
s :- go(south).
e :- go(east).
w :- go(west).
u :- go(up).
d :- go(down).

go(Direction) :-
    in_desert_maze,
    (Direction = north ; Direction = south ; Direction = east ; Direction = west),
    !,
    desert_move_player(Direction).

go(Direction) :-
    (in_combat(true) ->
        write('You can''t move until the combat is over'), nl,
        fail
    ;
        i_am_at(Here),
        (path(Here, Direction, There) ->
            (check_path_condition(Here, Direction, There) ->
                retract(i_am_at(Here)),
                assert(i_am_at(There)),
                update_move_status,
                ( There = desert ->
                    look,
                    enter_desert_maze
                ;   i_am_at(There),
                    look,
                    check_monster_encounter
                )
            ;
                true
            )
        ;
            write('It seems no roads to go.')
        )
    ),
    !.

/* Drink Water From River */
drink_river :-
    i_am_at(river),
    player_status(Health, Attack, Defense, Hunger, Thirst, Gold),
    NewThirst is min(100, Thirst + 50),
    NewHealth is min(100, Health + 10),
    retract(player_status(Health, Attack, Defense, Hunger, Thirst, Gold)),
    assert(player_status(NewHealth, Attack, Defense, Hunger, NewThirst, Gold)),
    write('You drink from the river and feel better!'), nl,
    !.

drink_river :-
    \+ i_am_at(river),
    write('You are not at the river!'), nl.

/* Items Specifics */
item_info(iron_sword, weapon, 50, 10, 0).       % name, type, price, attack_bonus, defense_bonus
item_info(iron_helmet, headgear, 50, 0, 10).
item_info(iron_armor, armor, 40, 0, 10).
item_info(iron_boot, footwear, 20, 0, 5).
item_info(oil_lamp, tool, 10, 0, 0).
item_info(crystal_sword, weapon, 0, 150, 0).
item_info(obsidian_helmet, headgear, 0, 0, 65).
item_info(obsidian_armor, armor, 0, 0, 30).
item_info(obsidian_boot, footwear, 0, 0, 25).
item_info(wind_ring, tool, 0, 90, 50).
item_info(key, tool, 0, 0, 0).

item_info(apple, food, 4, 5, 5).                % name, type, price, hunger_restore, thirst_restore
item_info(bread, food, 5, 10, 0).
item_info(water, drink, 2, 0, 20).
item_info(potion, health, 20, 25, 0).           % name, type, price, health_restore, none
item_info(elixir, health, 0, 100, 0).

item_info(mucus_ball, loot, 20, 0, 0).
item_info(goblin_ear, loot, 50, 0, 0).
item_info(bat_wing, loot, 100, 0, 0).
item_info(bearskin, loot, 200, 0, 0).

/* Shop Item Listing */
list_shop_items(Shop) :-
    write('Available items:'), nl,
    shop_item(Shop, Item, Stock),
    item_info(Item, Type, Price, _, _),
    write('- '), write(Item), write(' ('), write(Type), write(') - '),
    write(Price), write(' gold (Available: '), write(Stock), write(')'), nl,
    fail.

/* Sell Items */
sell(Item, Count) :-
    integer(Count), Count > 0,
    i_am_at(adventurers_hall),
    item_count(Item, CurrentCount),
    CurrentCount >= Count,
    item_info(Item, Type, Price, _, _),
    Type = loot,
    TotalPrice = Price * Count,
    player_status(Health, Attack, Defense, Hunger, Thirst, Gold),
    NewGold is Gold + TotalPrice,
    retract(player_status(Health, Attack, Defense, Hunger, Thirst, Gold)),
    assert(player_status(Health, Attack, Defense, Hunger, Thirst, NewGold)),
    NewCount is CurrentCount - Count,
    retract(item_count(Item, CurrentCount)),
    (NewCount > 0 -> assert(item_count(Item, NewCount)) ; true),
    write('You sold '), write(Count), write(' '), write(Item),
    write(' for '), write(TotalPrice), write(' gold.'), nl,
    !.

sell(Item, Count) :-
    integer(Count), Count > 0,
    i_am_at(adventurers_hall),
    item_count(Item, CurrentCount),
    CurrentCount < Count,
    write('Not enough '), write(Item), write('! You only have '),
    write(CurrentCount), write('.'), nl,
    !.

sell(Item, Count) :-
    integer(Count), Count > 0,
    i_am_at(adventurers_hall),
    item_info(Item, Type, _, _, _),
    Type \= loot,
    write('You can only sell loot items here!'), nl,
    !.

sell(Item, Count) :-
    integer(Count), Count > 0,
    i_am_at(adventurers_hall),
    \+ item_info(Item, _, _, _, _),
    write('There is no such item "'), write(Item), write('" in the game!'), nl,
    !.

sell(Item, Count) :-
    integer(Count), Count > 0,
    i_am_at(adventurers_hall),
    item_info(Item, loot, _, _, _),
    \+ item_count(Item, _),
    write('You don''t have any '), write(Item), write(' to sell!'), nl,
    !.

sell(_, Count) :-
    integer(Count), Count =< 0,
    write('Count must be a positive integer!'), nl,
    !.

sell(_, Count) :-
    \+ integer(Count),
    write('Count must be an integer!'), nl,
    !.

sell(_) :-
    \+ i_am_at(adventurers_hall),
    write('You can only sell items at the Adventurer''s Hall!'), nl.

list_loot :-
    i_am_at(adventurers_hall),
    findall(Item-Count-Price,
            (item_count(Item, Count),
             Count > 0,
             item_info(Item, loot, Price, _, _)),
            LootItems),
    (LootItems = [] ->
        write('You have no loot items to sell.'), nl
    ;
        write('=== LOOT ITEMS FOR SALE ==='), nl,
        print_loot_items(LootItems)
    ).

print_loot_items([]).
print_loot_items([Item-Count-Price|Rest]) :-
    write('- '), write(Item), write(' (Price: '), write(Price),
    write(' gold) - Quantity: '), write(Count), nl,
    TotalValue is Price * Count,
    write('  Total value: '), write(TotalValue), write(' gold'), nl,
    print_loot_items(Rest).

/* Buy Items */
buy(Item, Count) :-
    integer(Count), Count > 0,
    i_am_at(Shop),
    (Shop = weapon_shop; Shop = market),
    shop_item(Shop, Item, Stock),
    Stock >= Count,
    item_info(Item, _, Price, _, _),
    player_status(Health, Attack, Defense, Hunger, Thirst, Gold),
    TotalPrice = Price * Count,
    Gold >= TotalPrice,
    NewGold is Gold - TotalPrice,
    NewStock is Stock - Count,
    retract(shop_item(Shop, Item, Stock)),
    assert(shop_item(Shop, Item, NewStock)),
    retract(player_status(Health, Attack, Defense, Hunger, Thirst, Gold)),
    assert(player_status(Health, Attack, Defense, Hunger, Thirst, NewGold)),
    add_to_bag(Item, Count),
    write('You bought '), write(Count), write(' '), write(Item), write(' for '), write(TotalPrice), write(' gold.'), nl,
    !.

buy(Item, Count) :-
    integer(Count), Count > 0,
    i_am_at(Shop),
    (Shop = weapon_shop; Shop = market),
    shop_item(Shop, Item, Stock),
    Stock < Count,
    (
        Stock =:= 0
        -> write('Oops! This item has been sold out.'), nl
        ;  write('Not enough stock! Only '), write(Stock), write(Item), write('.'), nl
    ),
    !.

buy(Item, Count) :-
    i_am_at(Shop),
    (Shop = weapon_shop; Shop = market),
    item_info(Item, _, Price, _, _),
    player_status(_, _, _, _, _, Gold),
    TotalPrice is Price * Count,
    Gold < TotalPrice,
    write('You don''t have enough gold! You need '), write(TotalPrice),
    write(' but only have '), write(Gold), write('.'), nl,
    !.

buy(_, Count) :-
    integer(Count), Count =< 0,
    write('Count must be a positive integer!'), nl,
    !.

buy(_, Count) :-
    \+ integer(Count),
    write('Count must be an integer!'), nl,
    !.

buy(_, _) :-
    write('It seems that there is no such item.'), nl.

/* Bag Management */
add_to_bag(Item, Count) :-
    item_count(Item, CurrentCount),
    NewCount is Count + CurrentCount,
    retract(item_count(Item, CurrentCount)),
    assert(item_count(Item, NewCount)),
    !.

add_to_bag(Item, Count) :-
    \+ item_count(Item, _),
    assert(item_count(Item, Count)).

/* Check Bag */
bag :-
    findall(Item-Count, item_count(Item, Count), Items),
        (Items = [] ->
            write('Your bag is empty.'), nl
        ;
            write('=== BAG INVENTORY ==='), nl,
            print_inventory(Items)
        ).

print_inventory([]).
print_inventory([Item-Count|Rest]) :-
    item_info(Item, Type, _, _, _),
    write(Item), write(' ('), write(Type), write(') - '), write(Count), nl,
    print_inventory(Rest).

/* Use Items */
use(Item) :-
    \+ item_count(Item, _),
    write('There is no such item in your bag.'), nl,
    !.

use(Item) :-
    item_count(Item, Count),
    Count =< 0,
    write('You don''t have any '), write(Item), write(' left.'), nl,
    !.

use(Item) :-
    item_count(Item, Count),
    Count > 0,
    item_info(Item, Type, _, _, _),
    Type = loot,
    !,
    write('You cannot use '), write(Item), write('. But you can sell it in the Adventurer''s Hall'), nl,
    !.

use(Item) :-
    item_count(Item, Count),
    Count > 0,
    item_info(Item, Type, _, _, _),
    (Type = weapon; Type = headgear; Type = armor; Type = footwear; Type = tool),
    !,
    write('You cannot use '), write(Item), write(' directly. Please equip it with equip('), write(Item), write(').'), nl,
    !.

use(Item) :-
    item_count(Item, Count),
    Count > 0,
    in_combat(true),
    combat_turn(player),
    !,
    item_info(Item, Type, _, Effect1, Effect2),
    (Type = food; Type = drink; Type = health),
    use_item_effect(Item, Type, Effect1, Effect2),
    NewCount is Count - 1,
    retract(item_count(Item, Count)),
    (NewCount > 0 -> assert(item_count(Item, NewCount)) ; true),
    write('You used '), write(Item), write(' in combat!'), nl,
    !.

use(Item) :-
    item_count(Item, Count),
    Count > 0,
    \+ in_combat(true),
    !,
    item_info(Item, Type, _, Effect1, Effect2),
    (Type = food; Type = drink; Type = health),
    use_item_effect(Item, Type, Effect1, Effect2),
    NewCount is Count - 1,
    retract(item_count(Item, Count)),
    (NewCount > 0 -> assert(item_count(Item, NewCount)) ; true),
    write('You used '), write(Item), write('.'), nl,
    !.

use_item_effect(Item, food, HungerRestore, ThirstRestore) :-
    player_status(Health, Attack, Defense, Hunger, Thirst, Gold),
    NewHunger is min(100, Hunger + HungerRestore),
    NewThirst is min(100, Thirst + ThirstRestore),
    retract(player_status(Health, Attack, Defense, Hunger, Thirst, Gold)),
    assert(player_status(Health, Attack, Defense, NewHunger, NewThirst, Gold)),
    write('You eat the '), write(Item), write('. Hunger decreased.'), nl.

use_item_effect(Item, drink, _, ThirstRestore) :-
    player_status(Health, Attack, Defense, Hunger, Thirst, Gold),
    NewThirst is min(100, Thirst + ThirstRestore),
    retract(player_status(Health, Attack, Defense, Hunger, Thirst, Gold)),
    assert(player_status(Health, Attack, Defense, Hunger, NewThirst, Gold)),
    write('You drink the '), write(Item), write('. Thirst decreased.'), nl.

use_item_effect(Item, health, HealthRestore, _) :-
    player_status(Health, Attack, Defense, Hunger, Thirst, Gold),
    NewHealth is min(100, Health + HealthRestore),
    retract(player_status(Health, Attack, Defense, Hunger, Thirst, Gold)),
    assert(player_status(NewHealth, Attack, Defense, Hunger, Thirst, Gold)),
    write('You drink the '), write(Item), write(', Health restored by '), write(HealthRestore), nl.

/* Equip Items */
equip(all) :-
    !,
    silent_unequip_all_slots([head, body, foot, right]),
    (equip_best_weapon -> true ; write('There are no weapons available for equipment.'), nl),
    (equip_best_armor(head, headgear) -> true ; write('There are no headgear available for equipment.'), nl),
    (equip_best_armor(body, armor) -> true ; write('There are no armors available for equipment.'), nl),
    (equip_best_armor(foot, footwear) -> true ; write('There are no footwear available for equipment.'), nl).

equip(Item) :-
    Item \= all,
    item_count(Item, Count),
    Count > 0,
    item_info(Item, Type, _, AttackBonus, DefenseBonus),
    can_equip_type(Type, Slot),
    equip_item(Item, Type, Slot, AttackBonus, DefenseBonus),
    write('You equipped the '), write(Item), write(' to '), write(Slot), write(' slot.'), nl,
    !.

equip(Item) :-
    Item \= all,
    item_count(Item, 0),
    write('You don''t have any '), write(Item), write(' left.'), nl.

equip(_) :-
    write('You can''t equip this!'), nl.

can_equip_type(headgear, head) :- !.
can_equip_type(armor, body) :- !.
can_equip_type(footwear, foot) :- !.
can_equip_type(tool, left) :- !.
can_equip_type(weapon, right) :- !.

equip_item(Item, _, Slot, AttackBonus, DefenseBonus) :-
    player_status(_, Attack, Defense, _, _, _),
    player_equipments(Head, Body, Foot, Left, Right),
    get_current_equipped(Slot, CurrentItem),

    (CurrentItem \= null ->
        unequip_effect(CurrentItem, CurrentAttackBonus, CurrentDefenseBonus),
        TempAttack is Attack - CurrentAttackBonus,
        TempDefense is Defense - CurrentDefenseBonus,
        (item_count(CurrentItem, CurrentCount) ->
            retract(item_count(CurrentItem, CurrentCount)),
            NewCurrentCount is CurrentCount + 1,
            assert(item_count(CurrentItem, NewCurrentCount))
        ;
            assert(item_count(CurrentItem, 1))
        ),
        retract(equipped(Slot, CurrentItem))
    ;
        TempAttack = Attack, TempDefense = Defense
    ),
    NewAttack is TempAttack + AttackBonus,
    NewDefense is TempDefense + DefenseBonus,

    update_equipment(Slot, Item, Head, Body, Foot, Left, Right, NewHead, NewBody, NewFoot, NewLeft, NewRight),
    retract(player_equipments(Head, Body, Foot, Left, Right)),
    assert(player_equipments(NewHead, NewBody, NewFoot, NewLeft, NewRight)),
    assert(equipped(Slot, Item)),

    retract(player_status(Health, Attack, Defense, Hunger, Thirst, Gold)),
    assert(player_status(Health, NewAttack, NewDefense, Hunger, Thirst, Gold)),

    retract(item_count(Item, Count)),
    NewCount is Count - 1,
    (NewCount > 0 -> assert(item_count(Item, NewCount)) ; true).

silent_unequip_all_slots([]).
silent_unequip_all_slots([Slot|Slots]) :-
    (has_equipment(Slot) -> unequip(Slot) ; true),
    silent_unequip_all_slots(Slots).

has_equipment(Slot) :-
    get_current_equipped(Slot, Item),
    Item \= null.

equip_best_weapon :-
    findall(Item-Attack,
            (item_count(Item, Count),
             Count > 0,
             item_info(Item, weapon, _, Attack, _)),
            Weapons),
    Weapons \= [],
    find_max_attack(Weapons, BestWeapon, MaxAttack),
    equip(BestWeapon),
    write('Equipped the best weapon: '), write(BestWeapon), write(' (Attack + '), write(MaxAttack), write(')'), nl.

equip_best_weapon :-
    fail.

equip_best_armor(Slot, Type) :-
    findall(Item-Defense,
            (item_count(Item, Count),
             Count > 0,
             item_info(Item, Type, _, _, Defense)),
            Armors),
    Armors \= [],
    find_max_defense(Armors, BestArmor, MaxDefense),
    equip(BestArmor),
    write('Equipped the best '), write(Type), write(': '), write(BestArmor),
    write(' (Defense + '), write(MaxDefense), write(') -> '), write(Slot), nl.

equip_best_armor(_, _) :-
    fail.

equip_quiet(Item) :-
    item_count(Item, Count),
    Count > 0,
    item_info(Item, Type, _, AttackBonus, DefenseBonus),
    can_equip_type(Type, Slot),
    equip_item(Item, Type, Slot, AttackBonus, DefenseBonus),
    !.

equip_quiet(_) :-
    fail.

find_max_attack([Item-Attack], Item, Attack).
find_max_attack([Item1-Attack1, Item2-Attack2|Rest], BestItem, BestAttack) :-
    find_max_attack([Item2-Attack2|Rest], CurrentBest, CurrentAttack),
    (Attack1 >= CurrentAttack ->
        BestItem = Item1, BestAttack = Attack1
    ;
        BestItem = CurrentBest, BestAttack = CurrentAttack
    ).

find_max_defense([Item-Defense], Item, Defense).
find_max_defense([Item1-Defense1, Item2-Defense2|Rest], BestItem, BestDefense) :-
    find_max_defense([Item2-Defense2|Rest], CurrentBest, CurrentDefense),
    (Defense1 >= CurrentDefense ->
        BestItem = Item1, BestDefense = Defense1
    ;
        BestItem = CurrentBest, BestDefense = CurrentDefense
    ).

/* Unequip Items */
unequip(Slot) :-
    (\+ valid_slot(Slot) ->
        write('Invalid slot! Valid slots are: head, body, foot, left, right.'), nl,
        fail
    ;
        true
    ),

    player_status(_, Attack, Defense, _, _, _),
    player_equipments(Head, Body, Foot, Left, Right),
    get_current_equipped(Slot, CurrentItem),

    (CurrentItem \= null ->
        unequip_effect(CurrentItem, AttackBonus, DefenseBonus),
        NewAttack is Attack - AttackBonus,
        NewDefense is Defense - DefenseBonus,

        update_equipment(Slot, null, Head, Body, Foot, Left, Right, NewHead, NewBody, NewFoot, NewLeft, NewRight),
        retract(player_equipments(Head, Body, Foot, Left, Right)),
        assert(player_equipments(NewHead, NewBody, NewFoot, NewLeft, NewRight)),

        retract(equipped(Slot, CurrentItem)),
        assert(equipped(Slot, null)),

        retract(player_status(Health, Attack, Defense, Hunger, Thirst, Gold)),
        assert(player_status(Health, NewAttack, NewDefense, Hunger, Thirst, Gold)),

        (item_count(CurrentItem, Count) ->
            retract(item_count(CurrentItem, Count)),
            NewCount is Count + 1,
            assert(item_count(CurrentItem, NewCount))
        ;
            assert(item_count(CurrentItem, 1))
        ),
        write('You unequipped the '), write(CurrentItem), write(' from your '), write(Slot), write('.'), nl
    ;
        write('You don''t have equipped anything on your '), write(Slot), write('.'), nl
    ),
    !.

unequip(_) :-
    write('You can use ''unequip(head/body/foot/left/right). to remove the equipment.'''), nl.

valid_slot(head).
valid_slot(body).
valid_slot(foot).
valid_slot(left).
valid_slot(right).

get_current_equipped(head, Item) :- player_equipments(Item, _, _, _, _).
get_current_equipped(body, Item) :- player_equipments(_, Item, _, _, _).
get_current_equipped(foot, Item) :- player_equipments(_, _, Item, _, _).
get_current_equipped(left, Item) :- player_equipments(_, _, _, Item, _).
get_current_equipped(right, Item) :- player_equipments(_, _, _, _, Item).

unequip_effect(Item, AttackBonus, DefenseBonus) :-
    item_info(Item, _, _, AttackBonus, DefenseBonus).

update_equipment(head, NewItem, _, Body, Foot, Left, Right, NewItem, Body, Foot, Left, Right).
update_equipment(body, NewItem, Head, _, Foot, Left, Right, Head, NewItem, Foot, Left, Right).
update_equipment(foot, NewItem, Head, Body, _, Left, Right, Head, Body, NewItem, Left, Right).
update_equipment(left, NewItem, Head, Body, Foot, _, Right, Head, Body, Foot, NewItem, Right).
update_equipment(right, NewItem, Head, Body, Foot, Left, _, Head, Body, Foot, Left, NewItem).

/* Check Player Status */
status :-
    player_status(Health, Attack, Defense, Hunger, Thirst, Gold),
    player_equipments(Head, Body, Foot, Left, Right),
    nl, write('=== PLAYER STATUS ==='), nl,
    write('Health: '), write(Health), write('/100'), nl,
    write('Attack: '), write(Attack), nl,
    write('Defense: '), write(Defense), nl,
    write('Hunger: '), write(Hunger), write('/100'), nl,
    write('Thirst: '), write(Thirst), write('/100'), nl,
    write('Gold: '), write(Gold), nl,
    write('=== PLAYER EQUIPMENTS ==='), nl,
    write('  Head: '), write(Head), nl,
    write('  Body: '), write(Body), nl,
    write('  Foot: '), write(Foot), nl,
    write('  Left: '), write(Left), nl,
    write('  Right: '), write(Right), nl,
    check_needs_warning(Health, Hunger, Thirst),
    nl.

check_needs_warning(Health, Hunger, Thirst) :-
    (Health < 20 -> write('WARNING: You have been seriously injured!'), nl ; true),
    (Hunger < 20 -> write('WARNING: You are very hungry!'), nl ; true),
    (Thirst < 20 -> write('WARNING: You are very thirsty!'), nl ; true).

/* Update Player Status */
check_hunger_thirst_penalty(CurrentHealth, NewHunger, NewThirst, FinalHealth) :-
    (NewHunger =< 0 ; NewThirst =< 0) ->
        HealthPenalty = 5,
        TempHealth is max(0, CurrentHealth - HealthPenalty),

        (TempHealth > 0 ->
            (\+ hunger_thirst_penalty(true) ->
                assert(hunger_thirst_penalty(true)),
                write('You lose '), write(HealthPenalty), write(' health due to starvation or dehydration!'), nl,
                write('WARNING: You are starving or dehydrated! You will lose health with every action!'), nl,
                nl
            ;
                write('You lose '), write(HealthPenalty), write(' health due to starvation or dehydration!'), nl,
                nl
            ),
            FinalHealth = TempHealth
        ;
            FinalHealth is 0,
            write('You lose '), write(HealthPenalty), write(' health due to starvation or dehydration!'), nl,
            write('You died of hunger or thirst...'), nl,
            game_over
        )
    ;
        (hunger_thirst_penalty(true) ->
            retract(hunger_thirst_penalty(true)),
            write('You are no longer starving or dehydrated.'), nl
        ;
            true
        ),
        FinalHealth = CurrentHealth.

update_move_status :-
    player_status(Health, Attack, Defense, Hunger, Thirst, Gold),
    NewHunger is max(0, Hunger - 2),
    NewThirst is max(0, Thirst - 3),
    check_hunger_thirst_penalty(Health, NewHunger, NewThirst, FinalHealth),
    retract(player_status(Health, Attack, Defense, Hunger, Thirst, Gold)),
    assert(player_status(FinalHealth, Attack, Defense, NewHunger, NewThirst, Gold)).

/* Wish To The Elf */
wish :-
    i_am_at(river),
    write('You threw 100 gold coins into the river, hoping to receive a response from the elves.'), nl,
    player_status(Health, Attack, Defense, Hunger, Thirst, Gold),
    (Gold >= 100 ->
        NewGold is Gold - 100,
        retract(player_status(Health, Attack, Defense, Hunger, Thirst, Gold)),
        assert(player_status(Health, Attack, Defense, Hunger, Thirst, NewGold)),

        wish_count(Count),
        (Count >= 10 ->
            write('You have made a wish ten times. The elf no longer responds to you.'), nl,
            !
        ;
            (got_crystal_sword(true) ->
                write('You have already received the protection of the elves and can no longer make wishes.'), nl,
                !
            ;
                SuccessRate is 10 + Count * 10,
                random(1, 101, RandomValue),
                (RandomValue =< SuccessRate ->
                    NewCount is Count + 1,
                    retract(wish_count(Count)),
                    assert(wish_count(NewCount)),
                    (got_crystal_sword(_) ->
                        retract(got_crystal_sword(_))
                    ;
                        true
                    ),
                    assert(got_crystal_sword(true)),
                    add_to_bag(crystal_sword, 1),
                    write('The elf has heard your wish! A flash of light passed, and you obtained the Crystal Sword!'), nl,
                    write('Crystal Sword: Attack +150'), nl
                ;
                    NewCount is Count + 1,
                    retract(wish_count(Count)),
                    assert(wish_count(NewCount)),
                    write('It seems nothing happened... You lost 100 gold.'), nl
                )
            )
        )
    ;
        write('You need at least 100 gold to make a wish!'), nl
    ),
    !.

wish :-
    \+ i_am_at(river),
    write('You can only make a wish at the river!'), nl,
    !.

/* Collapse The Cave */
open :-
    i_am_at(deep_cave),
    defeated_monster(bear),
    \+ chest_opened(true),
    !,
    add_to_bag(obsidian_helmet, 1),
    add_to_bag(obsidian_armor, 1),
    add_to_bag(obsidian_boot, 1),
    player_status(Health, Attack, Defense, Hunger, Thirst, Gold),
    NewGold is Gold + 500,
    retract(player_status(Health, Attack, Defense, Hunger, Thirst, Gold)),
    assert(player_status(Health, Attack, Defense, Hunger, Thirst, NewGold)),
    retract(chest_opened(false)),
    assert(chest_opened(true)),
    write('You opened the treasure chest!'), nl,
    write('You obtained:'), nl,
    write('- Obsidian Helmet'), nl,
    write('- Obsidian Armor'), nl,
    write('- Obsidian Boots'), nl,
    write('- 500 gold'), nl,
    write('Suddenly, the cave starts shaking! The treasure chest triggered a mechanism!'), nl,
    write('The cave is collapsing! You need to escape quickly!'), nl,
    retract(cave_collapsed(false)),
    assert(cave_collapsed(true)),
    !.

open :-
    i_am_at(deep_cave),
    \+ defeated_monster(bear),
    write('You cannot open the treasure chest while the bear is still alive!'), nl,
    !.

open :-
    i_am_at(deep_cave),
    chest_opened(true),
    write('The treasure chest has already been opened!'), nl,
    !.

open :-
    \+ i_am_at(deep_cave),
    write('There is no treasure chest to open here!'), nl,
    !.

/* Desert Maze */
desert_cell(1,1,wall). desert_cell(1,2,wall). desert_cell(1,3,wall). desert_cell(1,4,wall). desert_cell(1,5,wall).
desert_cell(1,6,wall). desert_cell(1,7,wall). desert_cell(1,8,wall). desert_cell(1,9,wall). desert_cell(1,10,wall).
desert_cell(1,11,wall). desert_cell(1,12,wall). desert_cell(1,13,wall). desert_cell(1,14,wall). desert_cell(1,15,wall).
desert_cell(1,16,wall). desert_cell(1,17,wall). desert_cell(1,18,wall). desert_cell(1,19,wall). desert_cell(1,20,wall).
desert_cell(1,21,wall). desert_cell(1,22,wall). desert_cell(1,23,wall). desert_cell(1,24,wall). desert_cell(1,25,wall).
desert_cell(1,26,wall). desert_cell(1,27,wall). desert_cell(1,28,wall). desert_cell(1,29,wall). desert_cell(1,30,wall).

desert_cell(2,1,wall). desert_cell(2,2,start). desert_cell(2,3,sand). desert_cell(2,4,sand). desert_cell(2,5,quicksand).
desert_cell(2,6,sand). desert_cell(2,7,sand). desert_cell(2,8,wall). desert_cell(2,9,sand). desert_cell(2,10,sand).
desert_cell(2,11,quicksand). desert_cell(2,12,sand). desert_cell(2,13,wall). desert_cell(2,14,sand). desert_cell(2,15,sand).
desert_cell(2,16,sand). desert_cell(2,17,wall). desert_cell(2,18,sand). desert_cell(2,19,quicksand). desert_cell(2,20,sand).
desert_cell(2,21,wall). desert_cell(2,22,sand). desert_cell(2,23,sand). desert_cell(2,24,sand). desert_cell(2,25,wall).
desert_cell(2,26,sand). desert_cell(2,27,sand). desert_cell(2,28,sand). desert_cell(2,29,sand). desert_cell(2,30,wall).

desert_cell(3,1,wall). desert_cell(3,2,sand). desert_cell(3,3,sand). desert_cell(3,4,wall). desert_cell(3,5,quicksand).
desert_cell(3,6,wall). desert_cell(3,7,sand). desert_cell(3,8,wall). desert_cell(3,9,sand). desert_cell(3,10,wall).
desert_cell(3,11,quicksand). desert_cell(3,12,wall). desert_cell(3,13,wall). desert_cell(3,14,sand). desert_cell(3,15,wall).
desert_cell(3,16,sand). desert_cell(3,17,wall). desert_cell(3,18,sand). desert_cell(3,19,quicksand). desert_cell(3,20,wall).
desert_cell(3,21,wall). desert_cell(3,22,sand). desert_cell(3,23,wall). desert_cell(3,24,sand). desert_cell(3,25,wall).
desert_cell(3,26,quicksand). desert_cell(3,27,wall). desert_cell(3,28,quicksand). desert_cell(3,29,sand). desert_cell(3,30,wall).

desert_cell(4,1,wall). desert_cell(4,2,quicksand). desert_cell(4,3,sand). desert_cell(4,4,wall). desert_cell(4,5,quicksand).
desert_cell(4,6,wall). desert_cell(4,7,sand). desert_cell(4,8,sand). desert_cell(4,9,sand). desert_cell(4,10,wall).
desert_cell(4,11,quicksand). desert_cell(4,12,sand). desert_cell(4,13,sand). desert_cell(4,14,sand). desert_cell(4,15,wall).
desert_cell(4,16,sand). desert_cell(4,17,sand). desert_cell(4,18,sand). desert_cell(4,19,quicksand). desert_cell(4,20,sand).
desert_cell(4,21,sand). desert_cell(4,22,sand). desert_cell(4,23,wall). desert_cell(4,24,sand). desert_cell(4,25,sand).
desert_cell(4,26,quicksand). desert_cell(4,27,sand). desert_cell(4,28,quicksand). desert_cell(4,29,sand). desert_cell(4,30,wall).

desert_cell(5,1,wall). desert_cell(5,2,sand). desert_cell(5,3,wall). desert_cell(5,4,sand). desert_cell(5,5,sand).
desert_cell(5,6,sand). desert_cell(5,7,sand). desert_cell(5,8,wall). desert_cell(5,9,quicksand). desert_cell(5,10,wall).
desert_cell(5,11,sand). desert_cell(5,12,sand). desert_cell(5,13,wall). desert_cell(5,14,quicksand). desert_cell(5,15,wall).
desert_cell(5,16,guard_start). desert_cell(5,17,wall). desert_cell(5,18,sand). desert_cell(5,19,sand). desert_cell(5,20,sand).
desert_cell(5,21,wall). desert_cell(5,22,sand). desert_cell(5,23,wall). desert_cell(5,24,sand). desert_cell(5,25,wall).
desert_cell(5,26,sand). desert_cell(5,27,wall). desert_cell(5,28,sand). desert_cell(5,29,quicksand). desert_cell(5,30,wall).

desert_cell(6,1,wall). desert_cell(6,2,sand). desert_cell(6,3,wall). desert_cell(6,4,sand). desert_cell(6,5,wall).
desert_cell(6,6,sand). desert_cell(6,7,wall). desert_cell(6,8,sand). desert_cell(6,9,sand). desert_cell(6,10,sand).
desert_cell(6,11,sand). desert_cell(6,12,sand). desert_cell(6,13,wall). desert_cell(6,14,quicksand). desert_cell(6,15,wall).
desert_cell(6,16,sand). desert_cell(6,17,wall). desert_cell(6,18,sand). desert_cell(6,19,wall). desert_cell(6,20,sand).
desert_cell(6,21,wall). desert_cell(6,22,sand). desert_cell(6,23,wall). desert_cell(6,24,sand). desert_cell(6,25,wall).
desert_cell(6,26,sand). desert_cell(6,27,wall). desert_cell(6,28,sand). desert_cell(6,29,sand). desert_cell(6,30,wall).

desert_cell(7,1,wall). desert_cell(7,2,quicksand). desert_cell(7,3,sand). desert_cell(7,4,sand). desert_cell(7,5,sand).
desert_cell(7,6,sand). desert_cell(7,7,wall). desert_cell(7,8,sand). desert_cell(7,9,quicksand). desert_cell(7,10,wall).
desert_cell(7,11,sand). desert_cell(7,12,quicksand). desert_cell(7,13,sand). desert_cell(7,14,sand). desert_cell(7,15,sand).
desert_cell(7,16,sand). desert_cell(7,17,wall). desert_cell(7,18,sand). desert_cell(7,19,wall). desert_cell(7,20,sand).
desert_cell(7,21,wall). desert_cell(7,22,sand). desert_cell(7,23,sand). desert_cell(7,24,sand). desert_cell(7,25,wall).
desert_cell(7,26,sand). desert_cell(7,27,sand). desert_cell(7,28,sand). desert_cell(7,29,wall). desert_cell(7,30,wall).

desert_cell(8,1,wall). desert_cell(8,2,sand). desert_cell(8,3,wall). desert_cell(8,4,quicksand). desert_cell(8,5,wall).
desert_cell(8,6,sand). desert_cell(8,7,wall). desert_cell(8,8,sand). desert_cell(8,9,quicksand). desert_cell(8,10,wall).
desert_cell(8,11,sand). desert_cell(8,12,wall). desert_cell(8,13,wall). desert_cell(8,14,sand). desert_cell(8,15,wall).
desert_cell(8,16,sand). desert_cell(8,17,wall). desert_cell(8,18,sand). desert_cell(8,19,wall). desert_cell(8,20,sand).
desert_cell(8,21,wall). desert_cell(8,22,quicksand). desert_cell(8,23,wall). desert_cell(8,24,quicksand). desert_cell(8,25,wall).
desert_cell(8,26,sand). desert_cell(8,27,wall). desert_cell(8,28,sand). desert_cell(8,29,wall). desert_cell(8,30,wall).

desert_cell(9,1,wall). desert_cell(9,2,sand). desert_cell(9,3,sand). desert_cell(9,4,sand). desert_cell(9,5,sand).
desert_cell(9,6,sand). desert_cell(9,7,sand). desert_cell(9,8,sand). desert_cell(9,9,sand). desert_cell(9,10,sand).
desert_cell(9,11,sand). desert_cell(9,12,sand). desert_cell(9,13,sand). desert_cell(9,14,sand). desert_cell(9,15,sand).
desert_cell(9,16,sand). desert_cell(9,17,sand). desert_cell(9,18,sand). desert_cell(9,19,sand). desert_cell(9,20,sand).
desert_cell(9,21,sand). desert_cell(9,22,quicksand). desert_cell(9,23,sand). desert_cell(9,24,sand). desert_cell(9,25,sand).
desert_cell(9,26,sand). desert_cell(9,27,sand). desert_cell(9,28,sand). desert_cell(9,29,sand). desert_cell(9,30,exit).

desert_cell(10,1,wall). desert_cell(10,2,wall). desert_cell(10,3,wall). desert_cell(10,4,wall). desert_cell(10,5,wall).
desert_cell(10,6,wall). desert_cell(10,7,wall). desert_cell(10,8,wall). desert_cell(10,9,wall). desert_cell(10,10,wall).
desert_cell(10,11,wall). desert_cell(10,12,wall). desert_cell(10,13,wall). desert_cell(10,14,wall). desert_cell(10,15,wall).
desert_cell(10,16,wall). desert_cell(10,17,wall). desert_cell(10,18,wall). desert_cell(10,19,wall). desert_cell(10,20,wall).
desert_cell(10,21,wall). desert_cell(10,22,wall). desert_cell(10,23,wall). desert_cell(10,24,wall). desert_cell(10,25,wall).
desert_cell(10,26,wall). desert_cell(10,27,wall). desert_cell(10,28,wall). desert_cell(10,29,wall). desert_cell(10,30,wall).

desert_walkable(CellType) :-
    CellType \= wall.

desert_quicksand(CellType) :-
    CellType = quicksand.

desert_start(Row,Col) :-
    desert_cell(Row,Col,start).

desert_exit(Row,Col) :-
    desert_cell(Row,Col,exit).

desert_guard_start(Row,Col) :-
    desert_cell(Row,Col,guard_start).

enter_desert_maze :-
    (   in_desert_maze ->
        true
    ;
        retractall(in_desert_maze),
        assert(in_desert_maze),
        retractall(desert_player_pos(_,_)),
        retractall(desert_guard_pos(_,_)),
        retractall(guard_stuck(_)),

        desert_start(PR, PC),
        desert_guard_start(GR, GC),

        assert(desert_player_pos(PR, PC)),
        assert(desert_guard_pos(GR, GC)),

        init_guard_state,

        write('Watch out! Don''t be caught by the guard!'), nl,
        show_desert_maze
    ).

leave_desert_maze :-
    retractall(in_desert_maze),
    retractall(desert_player_pos(_,_)),
    retractall(desert_guard_pos(_,_)),
    retractall(guard_stuck(_)),
    catch(delete_file('adversary_problem.pddl.soln'), _, true).

show_desert_maze :-
    ( in_desert_maze ->
        forall(between(1,10,R),
               ( forall(between(1,30,C), desert_print_cell(R,C)), nl ) )
    ;   true
    ).

desert_print_cell(R,C) :-
    desert_player_pos(PR,PC),
    desert_guard_pos(GR,GC),
    ( R =:= PR, C =:= PC ->
        write('P')
    ; R =:= GR, C =:= GC ->
        write('G')
    ; desert_cell(R,C,Type),
      ( Type = wall -> write('#')
      ; Type = sand -> write('.')
      ; Type = quicksand -> write('~')
      ; Type = start -> write('.')
      ; Type = exit -> write('X')
      ; write('.')
      )
    ).

desert_dir_delta(north, -1, 0).
desert_dir_delta(south,  1, 0).
desert_dir_delta(west,   0,-1).
desert_dir_delta(east,   0, 1).

extra_desert_penalty :-
    player_status(Health,Attack,Defense,Hunger,Thirst,Gold),
    NewHunger is max(0,Hunger-2),
    NewThirst is max(0,Thirst-2),
    retract(player_status(Health,Attack,Defense,Hunger,Thirst,Gold)),
    assert(player_status(Health,Attack,Defense,NewHunger,NewThirst,Gold)),
    check_hunger_thirst_penalty(Health,NewHunger,NewThirst,_).

desert_move_guard :-
    guard_stuck(true),
    !,
    write('Guard is stuck in quicksand and cannot move!'), nl,
    retract(guard_stuck(true)),
    assert(guard_stuck(false)).

desert_move_player(Direction) :-
    desert_dir_delta(Direction, DR, DC),
    desert_player_pos(R, C),
    NR is R + DR,
    NC is C + DC,
    (   desert_cell(NR, NC, Type), Type \= wall ->
        retract(desert_player_pos(R, C)),
        assert(desert_player_pos(NR, NC)),
        (   desert_cell(NR, NC, quicksand) ->
            extra_desert_penalty,
            write('You step into quicksand! Breaking free consumed more energy!'), nl
        ;   true
        ),
        (   \+ desert_cell(NR, NC, exit) ->
            (guard_move_pddl -> true ; guard_move_simple)
        ;   true
        ),
        desert_check_state,
        show_desert_maze
    ;   write('Sand wall blocks your way.'), nl
    ).

desert_check_state :-
    desert_player_pos(PR,PC),
    desert_guard_pos(GR,GC),
    ( PR =:= GR, PC =:= GC ->
        write('The guard catches you in the desert!'), nl,
        game_over
    ;   desert_exit(PR,PC) ->
        write('You find a narrow path leading out of the desert...'), nl,
        nl,
        leave_desert_maze,
        retract(i_am_at(desert)),
        assert(i_am_at(castle_hall)),
        look
    ;   true
    ).

/* Rotate The Statue */
rotate_statue :-
    i_am_at(castle_hall),
    basement_door_open(State),
    (State = true ->
        retract(basement_door_open(true)),
        assert(basement_door_open(false)),
        write('You rotate the dragon statue. The basement door closes with a heavy sound.'), nl
    ;
        retract(basement_door_open(false)),
        assert(basement_door_open(true)),
        write('You rotate the dragon statue. A hidden door to the basement opens!'), nl
    ),
    !.

rotate_statue :-
    \+ i_am_at(castle_hall),
    write('There is no dragon statue here to rotate!'), nl,
    !.

/* Unlock The Treasure Chest */
enter(Password) :-
    i_am_at(basement),
    basement_treasure(State),
    player_status(Health, Attack, Defense, Hunger, Thirst, Gold),
    (State = false ->
        (Password = 2635 ->
            retract(basement_treasure(false)),
            assert(basement_treasure(true)),
            player_status(Health, Attack, Defense, Hunger, Thirst, Gold),
            NewGold is Gold + 500,
            retract(player_status(Health, Attack, Defense, Hunger, Thirst, Gold)),
            assert(player_status(Health, Attack, Defense, Hunger, Thirst, NewGold)),
            write('You unlock the treasure chest!'), nl,
            write('You obtained 500 gold.'), nl,
            add_to_bag(elixir, 5),
            write('You obtained 5 Elixirs(Health + 100).'), nl,
            add_to_bag(wind_ring, 1),
            write('You obtained Wind Ring: Attack + 90, Defense + 50).'), nl
        ;
            write('The password seems incorrect. Let''s look for more clues...'), nl
        )
    ;
        write('The treasure chest has already been unlocked.'), nl
    ),
    !.

enter(Password) :-
    \+ integer(Password),
    write('Password must be a number!'), nl,
    !.

enter(_) :-
    \+ i_am_at(basement),
    write('There is no place to enter the password!'), nl,
    !.

/* Rescue The Princess */
use_key :-
    i_am_at(attic),
    defeated_monster(dragon),
    item_count(key, Count),
    Count > 0,
    !,
    write('You used the key to open the cage and successfully rescued the princess!'), nl,
    write('=== Game victory! ==='), nl,
    write('Congratulations on completing the mission of saving the princess!'), nl,
    halt.

use_key :-
    i_am_at(attic),
    \+ defeated_monster(dragon),
    write('You don''t have key! Defeat the dragon to obtain the key!'), nl,
    !.

use_key :-
    \+ i_am_at(attic),
    item_count(key, Count),
    (Count > 0 ->
        write('There is no place to use key.'), nl
    ;
        write('You don''t have any key in your bag.'), nl
    ),
    !.

use_key :-
    \+ i_am_at(attic),
    \+ item_count(key, _),
    write('You don''t have any key in your bag.'), nl,
    !.

/* Monster System */
monster(slime, forest, 10, 1, 0).                   % Monster Definition: name, area, health, attack, defense
monster(goblin, forest, 50, 10, 5).
monster(bat, cave, 5, 5, 0).
monster(bear, deep_cave, 500, 50, 50).
monster(dragon, attic, 1000, 200, 150).

monster_encounter_chance(forest, 90).               % Monster encounter chance: area, probability
monster_encounter_chance(cave, 80).
monster_encounter_chance(deep_cave, 100).
monster_encounter_chance(attic, 100).

check_monster_encounter :-
    i_am_at(Location),
    \+ in_combat(true),
    monster_encounter_chance(Location, Chance),
    random(1, 101, Random),
    Random =< Chance,
    find_monster_for_location(Location, Monster),
    \+ monster_here(Location, _),
    ( (Monster = bear; Monster = dragon) ->
        ( \+ defeated_monster(Monster) ->
            assert(monster_here(Location, Monster)),
            write('Oh no! The '), write(Monster), write(' is charging towards you!'), nl,
            start_combat(Monster)
        ;
            true
        )
    ;
        assert(monster_here(Location, Monster)),
        write('Oh no! The '), write(Monster), write(' is charging towards you!'), nl,
        start_combat(Monster)
    ),
    !.
check_monster_encounter.

find_monster_for_location(Location, Monster) :-
    findall(Name, monster(Name, Location, _, _, _), Monsters),
    Monsters \= [],
    random_member(Monster, Monsters).

/* Combat System */
check_combat_move :-
    in_combat(_),
    write('Use "attack." to attack or "escape." to try to escape.'), nl,
    !,
    fail.
check_combat_move.

start_combat(Monster) :-
    monster(Monster, _, Health, Attack, Defense),
    assert(in_combat(true)),
    assert(current_monster(Monster, Health, Attack, Defense)),
    assert(combat_turn(player)),
    write('=== COMBAT START! ==='), nl,
    write('You encounter a '), write(Monster), write('!'), nl,
    display_combat_status,
    !.

display_combat_status :-
    current_monster(Monster, MonsterHealth, MonsterAttack, MonsterDefense),
    player_status(PlayerHealth, PlayerAttack, PlayerDefense, PlayerHunger, PlayerThirsty, _),
    nl, write('=== COMBAT STATUS ==='), nl,
    write('Monster: '), write(Monster), nl,
    write('Monster Health: '), write(MonsterHealth), nl,
    write('Monster Attack: '), write(MonsterAttack), nl,
    write('Monster Defense: '), write(MonsterDefense), nl,
    write('---'), nl,
    write('Your Health: '), write(PlayerHealth), write('/100'), nl,
    write('Your Attack: '), write(PlayerAttack), nl,
    write('Your Defense: '), write(PlayerDefense), nl,
    write('Your Hunger: '), write(PlayerHunger), nl,
    write('Your Hunger: '), write(PlayerThirsty), nl,
    nl,
    (combat_turn(player) ->
        write('It''s your turn! Commands: attack., escape., use(Item).'), nl
    ;
        write('It''s the monster''s turn!'), nl
    ).

attack :-
    in_combat(true),
    combat_turn(player),
    !,
    player_attack,
    (check_combat_end -> true; true).

attack :-
    in_combat(true),
    combat_turn(monster),
    write('It''s not your turn! The monster is about to attack!'), nl,
    !.

attack :-
    \+ in_combat(_),
    write('You are not in combat!'), nl.

escape :-
    in_combat(true),
    combat_turn(player),
    !,
    HungerCost = 1, ThirstCost = 2,
    player_status(Health, Attack, Defense, Hunger, Thirst, Gold),
    NewHunger is max(0, Hunger - HungerCost),
    NewThirst is max(0, Thirst - ThirstCost),
    check_hunger_thirst_penalty(Health, NewHunger, NewThirst, FinalHealth),
    retract(player_status(Health, Attack, Defense, Hunger, Thirst, Gold)),
    assert(player_status(FinalHealth, Attack, Defense, NewHunger, NewThirst, Gold)),

    random(1, 101, EscapeChance),
    (EscapeChance =< 50 ->
        write('You successfully escaped from the combat!'), nl,
        end_combat(escape)
    ;
        write('Escape failed! The monster blocks your path.'), nl,
        retract(combat_turn(player)),
        assert(combat_turn(monster)),
        monster_attack_action
    ).

escape :-
    in_combat(true),
    combat_turn(monster),
    write('You cannot escape during the monster''s turn!'), nl,
    !.

escape :-
    \+ in_combat(_),
    write('You are not in combat!'), nl.

use_in_combat(Item) :-
    in_combat(true),
    combat_turn(player),
    use(Item),
    !.

use_in_combat(_) :-
    in_combat(true),
    combat_turn(monster),
    write('You cannot use items during the monster''s turn!'), nl,
    !.

use_in_combat(Item) :-
    \+ in_combat(_),
    use(Item).

/* Player Attack */
player_attack :-
    in_combat(true),
    combat_turn(player),
    !,
    current_monster(Monster, MonsterHealth, MonsterAttack, MonsterDefense),
    player_status(_, PlayerAttack, _, _, _, _),

    HungerCost = 2, ThirstCost = 3,
    player_status(Health, Attack, Defense, Hunger, Thirst, Gold),
    NewHunger is max(0, Hunger - HungerCost),
    NewThirst is max(0, Thirst - ThirstCost),
    check_hunger_thirst_penalty(Health, NewHunger, NewThirst, FinalHealth),
    retract(player_status(Health, Attack, Defense, Hunger, Thirst, Gold)),
    assert(player_status(FinalHealth, Attack, Defense, NewHunger, NewThirst, Gold)),

    random(1, 101, DodgeChance),
    (DodgeChance =< 20 ->
        write('The '), write(Monster), write(' dodged your attack!'), nl
    ;
        Damage is max(1, PlayerAttack - MonsterDefense),
        NewMonsterHealth is MonsterHealth - Damage,
        retract(current_monster(Monster, MonsterHealth, MonsterAttack, MonsterDefense)),
        (NewMonsterHealth > 0 ->
            assert(current_monster(Monster, NewMonsterHealth, MonsterAttack, MonsterDefense)),
            write('You hit the '), write(Monster), write(' for '), write(Damage), write(' damage!'), nl,
            write('Monster health: '), write(NewMonsterHealth), nl
        ;
            write('You deliver the final blow! The '), write(Monster), write(' is defeated!'), nl,
            end_combat(victory),
            give_combat_rewards(Monster),
            !
        )
    ),
    (in_combat(true) ->
        retract(combat_turn(player)),
        assert(combat_turn(monster)),
        write('It''s the monster''s turn!'), nl,
        monster_attack_action
    ;
        true
    ).

/* Monster Attack */
monster_attack_action :-
    current_monster(Monster, _, MonsterAttack, _),
    player_status(PlayerHealth, PlayerAttack, PlayerDefense, Hunger, Thirst, Gold),

    Damage is max(1, MonsterAttack - PlayerDefense),
    NewPlayerHealth is PlayerHealth - Damage,

    retract(player_status(PlayerHealth, PlayerAttack, PlayerDefense, Hunger, Thirst, Gold)),
    assert(player_status(NewPlayerHealth, PlayerAttack, PlayerDefense, Hunger, Thirst, Gold)),

    write('The '), write(Monster), write(' attacks you for '), write(Damage), write(' damage!'), nl,
    write('Your health: '), write(NewPlayerHealth), nl,

    (NewPlayerHealth =< 0 ->
        write('You have been defeated! Game Over.'), nl,
        end_combat(defeat),
        game_over
    ;
        retract(combat_turn(monster)),
        assert(combat_turn(player)),
        write('It''s your turn! Type "attack." to attack or "escape." to try to escape.'), nl
    ).

monster_attack :-
    in_combat(true),
    combat_turn(monster),
    !,
    monster_attack_action.

monster_turn :-
    in_combat(true),
    combat_turn(monster),
    !,
    monster_attack.

monster_turn :-
    \+ combat_turn(monster),
    write('It''s not the monster''s turn!'), nl.

/* End The Combat */
check_combat_end :-
   current_monster(Monster, Health, _, _),
    (Health =< 0 ->
        write('The '), write(Monster), write(' has been defeated!'), nl,
        end_combat(victory),
        give_combat_rewards(Monster)
    ;
        true
    ).

end_combat(Result) :-
    retractall(in_combat(_)),
    retractall(current_monster(_,_,_,_)),
    retractall(combat_turn(_)),
    i_am_at(Location),
    retractall(monster_here(Location, _)),
    (Result = victory -> write('Victory!') ; true),
    (Result = escape -> write('You escaped safely.') ; true),
    nl.

/* Reward System */
give_combat_rewards(Monster) :-
    monster_reward(Monster, ItemReward, ItemCount),
    (ItemReward \= none ->
        add_to_bag(ItemReward, ItemCount),
        write('You obtained '), write(ItemCount), write(' '), write(ItemReward), write('!'), nl
    ;
        true
    ),
    (Monster = bear ->
        assert(defeated_monster(Monster)),
        retractall(bear_defeated(_)),
        assert(bear_defeated(true)),
        write('The '), write(Monster), write(' has been permanently defeated!'), nl
    ;
    Monster = dragon ->
        assert(defeated_monster(Monster)),
        retractall(dragon_defeated(_)),
        assert(dragon_defeated(true)),
        write('The '), write(Monster), write(' has been permanently defeated!'), nl,
        write('Now use the key to rescue the princess! (use_key.)')
    ;
        true
    ).

monster_reward(slime, mucus_ball, 3).                  % monster, item, count
monster_reward(goblin, goblin_ear, 2).
monster_reward(bat, bat_wing, 2).
monster_reward(bear, bearskin, 1).
monster_reward(dragon, key, 1).

/* Game over */
game_over :-
    assert(gaming(false)),
    write('=== GAME OVER ==='), nl,
    halt.

/* Game instructions */
help :-
    nl,
    write('=== Rescue the Princess ==='), nl,
    write('Available commands:'), nl,
    write('start.                       - Start the game'), nl,
    write('n. s. e. w. u. d.            - Move in directions'), nl,
    write('attack.                      - Attack the monster'), nl,
    write('escape.                      - Try to escape from combat'), nl,
    write('status.                      - Check your status'), nl,
    write('bag.                         - Check bag'), nl,
    write('buy(Item, Count).            - Buy items in shops'), nl,
    write('sell(Item, Count).           - sell your loot in hall'), nl,
    write('use(Item).                   - Use consumable items'), nl,
    write('equip(Item).                 - equip weapon/armor/tool'), nl,
    write('equip(all).                  - equip all the best weapon/armor'), nl,
    write('unequip(Slot).               - unequip weapon/armor/tool from slot'), nl,
    write('drink_river.                 - Drink from river'), nl,
    write('wish.                        - Wish to the elf'), nl,
    write('open.                        - Open the chest in cave'), nl,
    write('rotate_statue.               - Rotate the dragon statue'), nl,
    write('enter(Password).             - Enter the password'), nl,
    write('use_key.                     - Rescue the princess'), nl,
    write('help.                        - Show these instructions'), nl,
    write('halt.                        - Quit game'), nl,
    nl.
