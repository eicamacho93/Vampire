% object/6
% object(Name, Description, Hidden, Action, ConditionsForAction, EffectsOfAction)

object(crucifix, 'A blessed cross made from silver',hidden,give(crucifix,vampire),[in(Agent,Room), has(Agent,crucifix), in(vampire, Room)],[remove(vampire, Room), remove(crucifix, Agent)]).

object(ancientBook, 'An ancient book, bound in a leathery material with a large crescent moon on the cover.',visible,read(ancientBook, crypt),[in(Agent,crypt),has(Agent,ancientBook)],[visible(silverKey)]).

object(magicPotion,'A small opaque yellow vial filled with a noxtious liquid.  The label on the front says -Herbicide-',visible,pour(magicPotion,fountain),[in(Agent,courtyard),has(Agent,magicPotion)],[visible(cloak)]).

object(crown,'A heavy circlet made of gold, inset with large red gems and worthy of a king.',visible,place(crown, vampireKing),[in(Agent,throneRoom),has(Agent,crown), agentStatus(vampireKind, dead)],[game(won)]).

object(gear,'A tiny round gear that have been removed from a machine.',visible,place(gear,clockTower),[in(Agent,clockTower),has(Agent,gear)],[unlock(treasureRoom)]).

object(brassKey,'A heavy brass key intended for an old lock',visible,open(brassKey, storageDoor),[in(Agent,royalChamber),has(Agent,brassKey)],[unlock(royalChamber)]).

object(helmet,'A unpolished metal helmet. It is dented from violent battles.',visible,place(helmet,armor),[in(Agent,diningHall),has(Agent,helmet)],[visible(weaponRoom)]).

object(rustySword,'Rusty, and questionably made for a man, yet it has a fine edge.',visible, cut(rustySword, tapestry),[in(Agent,artRoom),has(Agent,rustySword)],[visible(library)]).

object(rustySword,'Rusty, and questionably made for a man, yet it has a fine edge.',visible, cut(rustySword, curtain),[in(Agent,throneRoom),has(Agent,rustySword)],[kill(vampireKind)]).

object(cloak,'A very good cloak.',hidden,place(cloak,statue),[in(Agent,westStairs),has(Agent,cloak)],[visible(royalChamber)]).

object(silverKey,'A beautiful silver key.', hidden, unlock(silverKey,bigCellDoor), [in(Agent,prison),has(Agent,silverKey), locked(bigCell)],[unlock(bigCell)]).

object(silverKey,'A beautiful silver key.', hidden, unlock(silverKey,smallCellDoor), [in(Agent,prison),has(Agent,silverKey), locked(smallCell)],[unlock(smallCell)]).

object(stone,'It is the right size and weight to make it good for throwing.',visible,throw(stone,mirror),[in(Agent,bedroom),has(Agent,stone)],[visible(secretStairs)]).

object(woodenStake,'A wooden stake',hidden,stab(woodenStake,vampire),[inRoom(Agent,room),has(Agent,woodenStake),alive(vampire,true)],[remove(vampire, room), remove(woodenStake, Agent)]).

object(garlic,'The garlic has a strong odor to scare away evil spirits.',visible,throw(garlic,vampire),[inRoom(Agent,room),has(Agent,garlic),in(vampire, room)],[remove(vampire, room), remove(garlic, Agent)]).

object(holyWater,'A bottle of with religious markings, useful for chasing cursed spirits away.',visible,throw(holyWater, vampire),[in(Agent,room),has(Agent,holyWater),alive(vampire,true)],[remove(vampire, room), remove(holyWater, Agent)]).

object(dragonKey,'A special key with a dragon on the handle. It is very heavy and pure gold.',visible,open(dragonKey, dragonChest),[in(Agent,lobby),has(Agent,dragonKey), hidden(woodenStake)],[visible(woodenStake)]).

object(candle,'A lit candle, in a plain brass candlestick.',visible,light(secretStairs),[in(Agent,secretStairs),has(Agent,candle)],[unlock(throneRoom)]).