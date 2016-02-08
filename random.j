// Randomization API

native GetRandomInt takes integer lowBound, integer highBound returns integer

native GetRandomReal takes real lowBound, real highBound returns real



native CreateUnitPool           takes nothing returns unitpool

native DestroyUnitPool          takes unitpool whichPool returns nothing

native UnitPoolAddUnitType      takes unitpool whichPool, integer unitId, real weight returns nothing

native UnitPoolRemoveUnitType   takes unitpool whichPool, integer unitId returns nothing

native PlaceRandomUnit          takes unitpool whichPool, player forWhichPlayer, real x, real y, real facing returns unit



native CreateItemPool           takes nothing returns itempool

native DestroyItemPool          takes itempool whichItemPool returns nothing

native ItemPoolAddItemType      takes itempool whichItemPool, integer itemId, real weight returns nothing

native ItemPoolRemoveItemType   takes itempool whichItemPool, integer itemId returns nothing

native PlaceRandomItem          takes itempool whichItemPool, real x, real y returns item



// Choose any random unit/item. (NP means Neutral Passive)

native ChooseRandomCreep        takes integer level returns integer

/**
NP meas Neutral Passive.
*/
native ChooseRandomNPBuilding   takes nothing returns integer

native ChooseRandomItem         takes integer level returns integer

native ChooseRandomItemEx       takes itemtype whichType, integer level returns integer

native SetRandomSeed            takes integer seed returns nothing
