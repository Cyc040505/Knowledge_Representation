/* <Rescue the princess>, by <Group 9>. */

:- dynamic i_am_at/1, item_count/2, shop_item/3, equipped/2.
:- retractall(i_am_at(_)), retractall(item_count(_,_)).
:- dynamic player_status/6, player_equipments/5.
:- retractall(player_status(_,_,_,_,_,_)), retractall(player_equipments(_,_,_,_,_)).

/* Initial player status: health, attack, defense, hunger, thirst, gold */
player_status(100, 10, 5, 100, 100, 500).

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

/* desert connections */
path(desert, north, forest).
path(desert, south, dragon_castle).

/* dragon_castle connections */
path(dragon_castle, north, desert).

/* Area descriptions */
describe(adventurers_hall) :-
    write('You are in the Adventurer''s Hall.'), nl,
    write('Your task is to defeat the dragon and rescue the princess.'), nl,
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
    write('Exit west to Forest.').

describe(cave) :-
    write('Whispering Cave: A dark opening in the mountainside, cool air wafts from within.'), nl,
    write('Strange echoes can be heard from the depths. The sole source of light gave me a little bit of comfort.'), nl,
    write('Exit east to Forest.').

describe(desert) :-
    write('Scorching Desert: A vast expanse of golden sand under the scorching sun.'), nl,
    write('It will be a serious test on physical strength.'), nl,
    write('Exits lead: north to Forest, south to Dragon Castle'), nl.

describe(dragon_castle) :-
    write('Dragon Castle: The ancient dragon''s castle.'), nl,
    write('It is said that the power of the dragon is extremely strong.'), nl,
    write('The princess must be trapped here.'), nl,
    write('It seems there are stairs, leading respectively to the basement and the attic.'), nl.

/* Basic movement rules */
n :- go(north).
s :- go(south).
e :- go(east).
w :- go(west).
u :- go(up).
d :- go(down).

go(Direction) :-
    i_am_at(Here),
    (path(Here, Direction, There) ->
        (check_path_condition(Here, Direction, There) ->
            retract(i_am_at(Here)),
            assert(i_am_at(There)),
            update_move_status,
            i_am_at(There),
            look
        ;
            true
        )
    ;
        write('It seems no roads to go.')
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

item_info(apple, food, 4, 5, 5).                % name, type, price, hunger_restore, thirst_restore
item_info(bread, food, 5, 10, 0).
item_info(water, drink, 2, 0, 20).

item_info(potion, health, 20, 25, 0).           % name, type, price, health_restore, none

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

/* Shop item listing */
list_shop_items(Shop) :-
    write('Available items:'), nl,
    shop_item(Shop, Item, Stock),
    item_info(Item, Type, Price, _, _),
    write('- '), write(Item), write(' ('), write(Type), write(') - '),
    write(Price), write(' gold (Available: '), write(Stock), write(')'), nl,
    fail.

/* Buy items */
buy(Item) :-
    i_am_at(Shop),
    (Shop = weapon_shop; Shop = market),
    shop_item(Shop, Item, Stock),
    Stock > 0,
    item_info(Item, _, Price, _, _),
    player_status(Health, Attack, Defense, Hunger, Thirst, Gold),
    Gold >= Price,
    NewGold is Gold - Price,
    NewStock is Stock - 1,
    retract(shop_item(Shop, Item, Stock)),
    assert(shop_item(Shop, Item, NewStock)),
    retract(player_status(Health, Attack, Defense, Hunger, Thirst, Gold)),
    assert(player_status(Health, Attack, Defense, Hunger, Thirst, NewGold)),
    add_to_bag(Item),
    write('You bought a '), write(Item), write(' for '), write(Price), write(' gold.'), nl,
    !.

buy(Item) :-
    i_am_at(Shop),
    (Shop = weapon_shop; Shop = market),
    shop_item(Shop, Item, 0),
    write('Oops! This item seems to have sold out.'), nl,
    !.

buy(Item) :-
    i_am_at(Shop),
    (Shop = weapon_shop; Shop = market),
    item_info(Item, _, Price, _, _),
    player_status(_, _, _, _, _, Gold),
    Gold < Price,
    write('You don''t have enough gold! You need '), write(Price),
    write(' but only have '), write(Gold), write('.'), nl,
    !.

buy(_) :-
    write('It seems that there is no such item.'), nl.

/* Bag management */
add_to_bag(Item) :-
    item_count(Item, Count),
    NewCount is Count + 1,
    retract(item_count(Item, Count)),
    assert(item_count(Item, NewCount)),
    !.

add_to_bag(Item) :-
    assert(item_count(Item, 1)).

/* Use Items */
use(Item) :-
    item_count(Item, Count),
    Count > 0,
    item_info(Item, Type, _, Effect1, Effect2),
    (Type = food; Type = drink; Type = health),
    use_item_effect(Item, Type, Effect1, Effect2),
    NewCount is Count - 1,
    retract(item_count(Item, Count)),
    (NewCount > 0 -> assert(item_count(Item, NewCount)) ; true),
    !.

use(Item) :-
    item_count(Item, Count),
    Count > 0,
    item_info(Item, Type, _, _, _),
    (Type = weapon; Type = headgear; Type = armor; Type = footwear; Type = tool),
    write('You cannot use '), write(Item), write(' directly. Please equip it with equip('), write(Item), write(').'), nl,
    !.

use(Item) :-
    item_count(Item, 0),
    write('You don''t have any '), write(Item), write(' left.'), nl,
    !.

use(_) :-
    write('There is no such item in your bag.'), nl.

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
equip(Item) :-
    item_count(Item, Count),
    Count > 0,
    item_info(Item, Type, _, AttackBonus, DefenseBonus),
    can_equip_type(Type, Slot),
    equip_item(Item, Type, Slot, AttackBonus, DefenseBonus),
    write('You equipped the '), write(Item), write(' to '), write(Slot), write(' slot.'), nl,
    !.

equip(Item) :-
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
update_move_status :-
    player_status(Health, Attack, Defense, Hunger, Thirst, Gold),
    NewHunger is max(0, Hunger - 2),
    NewThirst is max(0, Thirst - 5),
    retract(player_status(Health, Attack, Defense, Hunger, Thirst, Gold)),
    assert(player_status(Health, Attack, Defense, NewHunger, NewThirst, Gold)).


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



/* Game instructions */
help :-
    nl,
    write('=== Rescue the Princess ==='), nl,
    write('Available commands:'), nl,
    write('n. s. e. w. u. d.            - Move in directions'), nl,
    write('status.                      - Check your status'), nl,
    write('bag.                         - Check bag'), nl,
    write('buy(Item).                   - Buy items in shops'), nl,
    write('use(Item).                   - Use consumable items'), nl,
    write('equip(Item).                 - equip weapon/armor/tool'), nl,
    write('unequip(Slot).               - unequip weapon/armor/tool from slot'), nl,
    write('drink_river.                 - Drink from river (at river only)'), nl,
    write('look.                        - Look around'), nl,
    write('help.                        - Show these instructions'), nl,
    write('halt.                        - Quit game'), nl,
    nl.

/* Start the game */
start :-
    write('Welcome to this world! Hero!'), nl,
    write('You start with 500 gold. Explore this world to your heart''s content!'), nl,
    write('Type "help." for help.'), nl.