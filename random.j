// Randomization API

/**
Returns a random integer in the range [lowBound, highBound].

@note If lowBound > highBound then it just swaps the values.

@bug If you call `GetRandomInt(INT_MIN, INT_MAX)` or `GetRandomInt(INT_MAX, INT_MIN)`
it will always return the same value, namely `INT_MIN` or `INT_MAX`.

@note See <http://hiveworkshop.com/threads/random.286109#post-3073222> for an overview of the algorithm used.

@param lowBound
The inclusive lower bound of the random number returned

@param highBound
The inclusive higher bound of the random number returned
*/
native GetRandomInt takes integer lowBound, integer highBound returns integer

native GetRandomReal takes real lowBound, real highBound returns real



native CreateUnitPool           takes nothing returns unitpool

native DestroyUnitPool          takes unitpool whichPool returns nothing

native UnitPoolAddUnitType      takes unitpool whichPool, integer unitId, real weight returns nothing

native UnitPoolRemoveUnitType   takes unitpool whichPool, integer unitId returns nothing

native PlaceRandomUnit          takes unitpool whichPool, player forWhichPlayer, real x, real y, real facing returns unit



/**
Creates an empty itempool handle.

Item pools are initially empty, but can have item-types added
to them via `ItemPoolAddItemType`. Item pools only serve for random item
placing, via `PlaceRandomItem`.
*/
native CreateItemPool           takes nothing returns itempool

native DestroyItemPool          takes itempool whichItemPool returns nothing

/**
Adds an item-id to the itempool.

@param whichItemPool
The itempool to add the item to

@param itemId
The rawcode of the item

@param weight
The weight of the item
The weight determines how likely it is for the item to be chose by `PlaceRandomItem`.
*/
native ItemPoolAddItemType      takes itempool whichItemPool, integer itemId, real weight returns nothing

native ItemPoolRemoveItemType   takes itempool whichItemPool, integer itemId returns nothing

/**
Draws a random itemid from the itempool and creates the item.

@param whichItemPool
The itempool to draw from

@param x
The x-coordinate of the item

@param y
The y-coordinate of the item
*/
native PlaceRandomItem          takes itempool whichItemPool, real x, real y returns item



// Choose any random unit/item. (NP means Neutral Passive)

/**
Returns the rawcode ID of a random unit of the specified level. The unit chosen
will come from the set of units that include or are assigned to the base tileset
of the map. Passing a level of -1 is equivalent to picking a creep of any level.
If there are no units of the specified level, the returned value is 0.

@param level
The level of the units to choose from.
*/
native ChooseRandomCreep        takes integer level returns integer

/**
Returns the rawcode ID of a random neutral passive building,
such as the buildings "Goblin Merchant" or "Tavern".

@note The building returned is not necessarily on the map already.
*/
native ChooseRandomNPBuilding   takes nothing returns integer

/**
Returns the rawcode ID of a random item of the specified level. Passing a level
of -1 will return an item of any level. If there are no items of the specified
level, the id returned will be 0.

@param level
The level of the items to choose from. Passing a level of -1 is equivalent to any level.

@note The item returned is not chosen from preplaced items on the map, but rather any item of that level.
*/
native ChooseRandomItem         takes integer level returns integer

/**
Returns the rawcode ID of a random item of the specified level and item type.
Passing a level of -1 will return an item of any level. If there are no items
of the specified level, the id returned will be 0.

@param whichType
The classification of items to choose from.

@param level
The level of the items to choose from. Passing a level of -1 is equivalent to any level.

@note The item returned is not chosen from preplaced items on the map, but rather any item of that level.
*/
native ChooseRandomItemEx       takes itemtype whichType, integer level returns integer

native SetRandomSeed            takes integer seed returns nothing
