# Rescue the Princess
### Start Game
- Install SWI-Prolog: https://www.swi-prolog.org/download/stable
- Install Pyperplan: `pip install pyperplan`
- Start Game: `swipl -q -s Rescue_the_Princess.pl`

### Game Instructions
**_Game Background_**  
You are a brave warrior, entrusted with the mission of finding and rescuing the princess 
who was captured by the ancient dragon. You can freely explore the map, handle various emergencies, 
and upgrade your equipment to complete the tasks.

**_Area Introduction_**  
- **Adventurer's Hall:** This is your starting point. You can sell your loot here to earn gold coins.
- **Market:** Here you can purchase consumables that restore health/hunger/thirsty.
- **Weapon Shop:** Here you can purchase basic equipment and all the tools needed for the games.
- **Forest:** Here you may meet some random events, defeats monsters and get the loot.
- **River:** This is the only safe place outside the city. Making a wish has a chance to trigger an elf event and obtain a special weapon.
- **Cave:** Here you will fight against the giant bear guarding the treasure. After winning, you will be able to obtain special armor.
- **Desert:** Here you will pass through the desert maze. Be careful of the guard in the maze and don't get caught.
- **Dragon Castle:** There is a special item in the basement that can help you defeat the dragon and rescue the princess from the attic.

**_Desert Maze_**
- **Sand wall:** "#", You can't pass them.
- **Sand:** ".", the normal roads.
- **Quicksand:** "~", passing through quicksand will consume extra energy. This can also hold back the actions of guard.
- **Player:** "P", your current position.
- **Guard:** "G", the current position of guard.
- **Exit:** "X", your destination.
```
##############################
#P..~..#..~.#...#.~.#...#....#
#..#~#.#.#~##.#.#.~##.#.#~#~.#
#~.#~#...#~...#...~...#..~.~.#
#.#....#~#..#~#G#...#.#.#.#.~#
#.#.#.#.....#~#.#.#.#.#.#.#..#
#~....#.~#.~....#.#.#...#...##
#.#~#.#.~#.##.#.#.#.#~#~#.#.##
#....................~.......X
##############################
```

**_Monster Introduction_**
- **Slime:** Cute little guys, only appear in the forest. (Health: 10, Attack: 1, Defense: 0)
- **Goblin:** The common monsters in the forest have occupied the entire forest. (Health: 50, Attack: 10, Defense: 5)
- **Bat:** The creatures in the cave move very quickly, so you can't defeat them. (Health: 5, Attack: 5, Defense: 0)
- **Bear:** The guardian of the cave treasure is extremely fierce. (Health: 500, Attack: 50, Defense: 50)
- **Maze Guard:** A huge stone statue that will eliminate the explorers who enter the maze and cannot be defeated.
- **Dragon:** The final boss, very strong, is the one that captured the princess (Health: 1000, Attack: 200, Defense: 150)

### Action Instructions
- Start the game: `start.`
- Move direction: `n. s. e. w. u. d.`
- Attack monster: `attack.`
- Escape from combat: `escape.`
- Check player's status: `status.`
- Check player's bag: `bag.`
- Buy items in the shop: `but(Item, Count).`
- Sell your loot in the hall: `sell(Item, Count).`
- Use supplies: `Use(Item).`
- Equip weapon/armor/tool: `equip(Item).`
- Equip the best weapon & armor: `equip(all).`
- Unequip the item from slot: `unequip(Slot).`
- Drink from river: `drink_river.`
- Wish to the elf: `wish.`
- Rotate the dragon statue: `rotate_statue.`
- Enter the password: `enter(Password).`
- Rescue the princess: `use_key.`
- Look around: `look.`
- Help: `help.`
- Quit the game: `halt.`

### Recommendation Strategy
1. Buy some supplies at the Market(10 for each item)
2. Buy all equipments at Weapon Shop
3. Obtain loot in the Forest and sell them in the Adventurer's Hall
4. Wish to the elf to obtain Crystal Sword
5. Defeat bear in the cave and obtain obsidian armor
6. Obtain Wind Ring in the basement
7. Defeat the dragon and rescue the princess
