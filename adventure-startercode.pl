/* <Rescue the princess>, by <Group 9>. */

:- dynamic i_am_at/1, item_count/2, shop_item/3, equipped/2.
:- retractall(i_am_at(_)), retractall(item_count(_,_)).
:- dynamic player_status/6, player_equipments/5, hunger_thirst_penalty/1.
:- retractall(player_status(_,_,_,_,_,_)), retractall(player_equipments(_,_,_,_,_)), retractall(hunger_thirst_penalty(_)).

/* Initial player status: health, attack, defense, hunger, thirst, gold */
player_status(100, 10, 5, 100, 100, 500).
hunger_thirst_penalty(false).

/* initial player equipments: head, body, foot, left, right */
player_equipments(null, null, null, null, null).

/* Initial position*/
i_am_at(adventurers_hall).

/* adventurers_hall connections */
path(adventurers_hall, east, weapon_shop).
path(adventurers_hall, west, market).
path(adventurers_hall, south, forest).

/* weapon_shop connections */
path(weapon_shop, west, adventurers_hall).

/* market connections */
path(market, east, adventurers_hall).

/* forest connections */
path(forest, north, adventurers_hall).
path(forest, east, river).
path(forest, west, cave).
path(forest, south, desert).

/* river connections */
path(river, west, forest).

/* cave connections */
path(cave, east, forest).
path(cave, down, deep_cave).
path(deep_cave, up, cave).

/* desert connections */
path(desert, north, forest).
path(desert, south, castle_hall).

/* dragon_castle connections */
path(castle_hall, north, desert).
path(castle_hall, up, attic).
path(attic, down, castle_hall).
path(castle_hall, down, basement).
path(basement, up, castle_hall).

/* Area descriptions */
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
    write('It will be a serious test on physical strength.'), nl,
    write('Exits lead: north to Forest, south to Dragon Castle'), nl.

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

/* Basic movement rules */
n :- go(north).
s :- go(south).
e :- go(east).
w :- go(west).
u :- go(up).
d :- go(down).

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
                i_am_at(There),
                look,
                check_monster_encounter
            ;
                true
            )
        ;
            write('It seems no roads to go.')
        )
    ),
    !.

/* Path conditions */
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

/* Look around */
look :-
    i_am_at(Place),
    describe(Place),
    nl.

/* Drink water from river */
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

/* Item specifics */
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

/* Item inventories */
shop_item(weapon_shop, iron_sword, 1).
shop_item(weapon_shop, iron_helmet, 1).
shop_item(weapon_shop, iron_armor, 1).
shop_item(weapon_shop, iron_boot, 1).
shop_item(weapon_shop, oil_lamp, 1).

shop_item(market, apple, 100).
shop_item(market, bread, 100).
shop_item(market, water, 100).
shop_item(market, potion, 100).

shop_item(river, crystal_sword, 1).
shop_item(deep_cave, obsidian_helmet, 1).
shop_item(deep_cave, obsidian_armor, 1).
shop_item(deep_cave, obsidian_boot, 1).
shop_item(basement, wind_ring, 1).
shop_item(basement, elixir, 5).

/* Shop item listing */
list_shop_items(Shop) :-
    write('Available items:'), nl,
    shop_item(Shop, Item, Stock),
    item_info(Item, Type, Price, _, _),
    write('- '), write(Item), write(' ('), write(Type), write(') - '),
    write(Price), write(' gold (Available: '), write(Stock), write(')'), nl,
    fail.

/* Sell items */
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

/* Buy items */
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

/* Bag management */
add_to_bag(Item, Count) :-
    item_count(Item, CurrentCount),
    NewCount is Count + CurrentCount,
    retract(item_count(Item, CurrentCount)),
    assert(item_count(Item, NewCount)),
    !.

add_to_bag(Item, Count) :-
    \+ item_count(Item, _),
    assert(item_count(Item, Count)).

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

/* Equip items */
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

/* Unequip items */
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

/* Check player status */
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

/* Update player status */
check_hunger_thirst_penalty(CurrentHealth, NewHunger, NewThirst, FinalHealth) :-
    (NewHunger =< 0 ; NewThirst =< 0) ->
        (\+ hunger_thirst_penalty(true) ->
            assert(hunger_thirst_penalty(true)),
            write('WARNING: You are starving or dehydrated! You will lose health with every action!'), nl
        ;
            true
        ),
        HealthPenalty = 5,
        FinalHealth is max(0, CurrentHealth - HealthPenalty),
        (FinalHealth > 0 ->
            write('You lose '), write(HealthPenalty), write(' health due to starvation or dehydration!'), nl
        ;
            write('You have died from starvation or dehydration!'), nl
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
    assert(player_status(FinalHealth, Attack, Defense, NewHunger, NewThirst, Gold)),
    (FinalHealth =< 0 ->
        write('You have died! Game Over.'), nl,
        game_over
    ;
        true
    ).

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

/* Wish to the elf */
:- dynamic wish_count/1, got_crystal_sword/1.
:- retractall(wish_count(_)), retractall(got_crystal_sword(_)).
wish_count(0).
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

/* Collapse the cave */
:- dynamic chest_opened/1, cave_collapsed/1.
:- retractall(chest_opened(_)), retractall(cave_collapsed(_)).
chest_opened(false).
cave_collapsed(false).

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

/* Desert maze */


/* Rotate dragon statue */
:- dynamic basement_door_open/1.
:- retractall(basement_door_open(_)).
basement_door_open(false).

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

/* Unlock the treasure chest */
:- dynamic basement_treasure/1.
:- retractall(basement_treasure(_)).
basement_treasure(false).

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
/* Rescue the princess */
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
:- dynamic bear_defeated/1, dragon_defeated/1.
:- retractall(bear_defeated(_)), retractall(dragon_defeated(_)).
bear_defeated(false).
dragon_defeated(false).

:- dynamic monster/5, monster_here/2, monster_status/4, defeated_monster/1.
:- retractall(monster(_,_,_,_,_)), retractall(monster_here(_,_)), retractall(monster_status(_,_,_,_)), retractall(defeated_monster(_)).

monster(slime, forest, 10, 1, 0).                   % Monster Definition: name, area, health, attack, defense
monster(goblin, forest, 50, 10, 5).
monster(bat, cave, 5, 5, 0).
monster(bear, deep_cave, 500, 50, 50).
monster(dragon, attic, 1000, 200, 150).

monster_encounter_chance(forest, 90).               % Monster encounter chance: area, probability
monster_encounter_chance(cave, 80).
monster_encounter_chance(deep_cave, 100).
monster_encounter_chance(desert, 0).
monster_encounter_chance(attic, 100).

init_monsters :-
    retractall(monster_here(_,_)),
    retractall(monster_status(_,_,_,_)),
    retractall(defeated_monster(_)),
    forall(monster(Name, Location, Health, _, _),
           assert(monster_status(Name, Location, Health, active))).

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
:- dynamic in_combat/1, current_monster/4, combat_turn/1.
:- retractall(in_combat(_)), retractall(current_monster(_,_,_,_)), retractall(combat_turn(_)).

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

/* Player attack */
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

/* Monster attack */
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

/* End the combat */
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

/* Reward system */
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
    write('=== GAME OVER ==='), nl,
    write('You have been defeated in combat.'), nl,
    write('Type "start." to restart the game.'), nl,
    halt.

/* Game instructions */
help :-
    nl,
    write('=== Rescue the Princess ==='), nl,
    write('Available commands:'), nl,
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

/* Start the game */
start :-
    write('Welcome to this world! Hero!'), nl,
    write('You start with 500 gold. Explore this world to your heart''s content!'), nl,
    init_monsters,
    write('Type "help." for help.'), nl,
    nl,
    look.