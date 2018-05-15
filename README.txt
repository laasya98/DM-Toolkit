DM toolkit

To run: type "make play" within the main project folder.
        You will be prompted to load a game file.

File Loading:
  You can load any valid state file. Included with this demo are
  "data/init_state.csv" and "data/teststate.csv".
  You can also load the default, currently set to "data/teststate.csv", by
  typing "def".

Things to Try:

  To start, I recommend adding a character, buying and selling, making a
  Battle event, and trying to both use the fight command and
  cast MagicMissile and Heal.

  "help" : displays a help menu. While incomplete, this can give some info
  on which commands are valid and how to invoke them.

  "roll [X]d[Y]t[Z]": rolls [X] die with [Y] sides each and takes [Z] of them.

  "move [exit]": Moves the party to a new location, by exit name.
  "get exits": Gives current room exits.

  "who is [name]": Gives details about a character.
  "characters": Lists characters present.

  "inv [name]": Lists a character's inventory.

  "quickbuild [name] [class] [race]": creates a new character based off the
  three arguments, and adds that character to the current party.
  Type \"help races\" or \"help classes\" to get a list of the classes in the
  game.

  "quickevent [name] [form] creates a new event with those values, and starts it.
  The list of acceptable event forms are:
    Battle: A combat event that tracks turn order and character attacks.
            Type \"help battle\" for more information about battles and combat.
    Shop:   A shop event wherein the player can buy and sell items.
            Type \"help battle\" for more information about shops.
    Interaction:  The default event. Neither a battle nor a shop.
            Use this to remove one of the other types.

  "use [character] [item]": has [character] use [item], if possible.
  "equip [character] [item]": has [character] equip [item], if possible.

  Within a Battle event, some special actions can be taken.
    Fight: "fight [attacker] [defender]"
        Use this command to have the [attacker] character attempt to hit the
        [defender] character with their equipped weapon. If no weapon is equipped,
        an unarmed strike is made instead.
    Turn: "turn" moves the turn to the next player. Once all players have
        taken a turn, the turn number increases. This is used for spellcasting,
        to deal with wait times.
    Cast: "cast [castor] [spell] [target list]" casts the spell on the targets.
        Use "spell [name]" to get information on a specific spell and how
        many targets it has. Casting may take multiple turns.

  Within a Shop event, some special actions can be taken.
      Buy: \"buy [character] [item] [quantity]\"
          Use this command to have the [character] buy [quantity] of [item]
          from the shop. This will fail if the item is not available or the
          character doesn't have enough money.
      Sell: \"sell [character] [item] [quantity]\"
          Use this command to have the [character] sell [quantity] of [item]
          to the shop. This will fail if the item is not owned by the character.


Note:

We added the line "rm -rf save" to the makefile to clean out save data when
cleaning the code.
