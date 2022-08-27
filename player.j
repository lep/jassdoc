// Player API

/**
@bug Crashes the game if used with wrong values, that is values greather than 15
or values lower than 0.

@pure
*/
constant native Player takes integer number returns player

/**
@async
*/
constant native GetLocalPlayer takes nothing returns player

constant native IsPlayerAlly takes player whichPlayer, player otherPlayer returns boolean

constant native IsPlayerEnemy takes player whichPlayer, player otherPlayer returns boolean

constant native IsPlayerInForce takes player whichPlayer, force whichForce returns boolean

constant native IsPlayerObserver takes player whichPlayer returns boolean

constant native IsVisibleToPlayer takes real x, real y, player whichPlayer returns boolean

constant native IsLocationVisibleToPlayer takes location whichLocation, player whichPlayer returns boolean

constant native IsFoggedToPlayer takes real x, real y, player whichPlayer returns boolean

constant native IsLocationFoggedToPlayer takes location whichLocation, player whichPlayer returns boolean

constant native IsMaskedToPlayer takes real x, real y, player whichPlayer returns boolean

constant native IsLocationMaskedToPlayer takes location whichLocation, player whichPlayer returns boolean



constant native GetPlayerRace takes player whichPlayer returns race

constant native GetPlayerId takes player whichPlayer returns integer

constant native GetPlayerUnitCount takes player whichPlayer, boolean includeIncomplete returns integer

constant native GetPlayerTypedUnitCount takes player whichPlayer, string unitName, boolean includeIncomplete, boolean includeUpgrades returns integer

constant native GetPlayerStructureCount takes player whichPlayer, boolean includeIncomplete returns integer

constant native GetPlayerState takes player whichPlayer, playerstate whichPlayerState returns integer

constant native GetPlayerScore takes player whichPlayer, playerscore whichPlayerScore returns integer

constant native GetPlayerAlliance takes player sourcePlayer, player otherPlayer, alliancetype whichAllianceSetting returns boolean



constant native GetPlayerHandicap takes player whichPlayer returns real

/**
@patch 1.32
*/
constant native GetPlayerHandicapReviveTime takes player whichPlayer returns real

/**
@patch 1.32
*/
constant native GetPlayerHandicapDamage takes player whichPlayer returns real

constant native GetPlayerHandicapXP takes player whichPlayer returns real

constant native SetPlayerHandicap takes player whichPlayer, real handicap returns nothing

constant native SetPlayerHandicapXP takes player whichPlayer, real handicap returns nothing


/**
@patch 1.32
*/
constant native SetPlayerHandicapReviveTime takes player whichPlayer, real handicap returns nothing

/**
@patch 1.32
*/
constant native SetPlayerHandicapDamage takes player whichPlayer, real handicap returns nothing


constant native SetPlayerTechMaxAllowed takes player whichPlayer, integer techid, integer maximum returns nothing

constant native GetPlayerTechMaxAllowed takes player whichPlayer, integer techid returns integer

/**
In upgrades that have multiple levels, it will research the upgrade by the
number of levels specified. (it adds the number of levels to the current
research level, see `SetPlayerTechResearched` to set the research level)

@param whichPlayer The player who the upgrade will be researched for.

@param techid The four digit rawcode ID of the upgrade.

@param levels The number of levels to add to the current research level of the upgrade.
*/
constant native AddPlayerTechResearched takes player whichPlayer, integer techid, integer levels returns nothing

constant native SetPlayerTechResearched takes player whichPlayer, integer techid, integer setToLevel returns nothing

constant native GetPlayerTechResearched takes player whichPlayer, integer techid, boolean specificonly returns boolean

/**
Gets the level of a tech of a player. This could be an upgrade, a unit type or an equivalent as selected in tech requirement fields.
In case of an upgrade, this would be the level of the upgrade the given player has and 0 if the player has not researched the upgrade at all.
In case of a unit type or an equivalent, it would be the amount of units of that type or which fulfill the equivalent condition for the given player.

@param whichPlayer The player whose tech level to query.

@param techid The id of the tech item. Either an upgrade like Iron Plating 'Rhar', a unit type like Footman 'hfoo' or one of the following special equivalent ids:
'HERO' - any hero
'TALT' - any altar
'TWN1' - town hall tier 1
'TWN2' - town hall tier 2
'TWN3' - town hall tier 3
'TWN4' - town hall tier 4
'TWN5' - town hall tier 5
'TWN6' - town hall tier 6
'TWN7' - town hall tier 7
'TWN8' - town hall tier 8
'TWN9' - town hall tier 9

@param specificonly When this is false, it will consider some additional dependencies between the techs:
When specificonly is false, the Human Guard Tower 'hgtw' will also be considered when querying for the Scout Tower 'hwtw' (even if the Guard Tower is preplaced, i.e. not doing the upgrade on runtime, so this checks the Teechtree - Upgrades To 'uupt' field?).
Higher tier townhalls will be considered when querying for lower tier hownhalls, i.e. querying for Great Hall 'ogre' will also consider Stronghold 'ostr' and Fortress 'ofrt' when specificonly is false.
Ability morph does not seem to be considered when specificonly is false, tested with Berserker Upgrade of Headhunter.
Techtree - Dependency Equivalents 'udep' seems to be considered even if specificonly is true, i.e. when you set Scout Tower as an equivalent for Farm 'hhou', querying for 'hhou' will also consider Scout Towers.
*/
constant native GetPlayerTechCount takes player whichPlayer, integer techid, boolean specificonly returns integer



native SetPlayerUnitsOwner takes player whichPlayer, integer newOwner returns nothing

/**
Reveals a player's remaining buildings to a force. The black mask over the
buildings will be removed as if the territory had been discovered

@note his function will not check whether the player has a town hall before revealing.

@param whichPlayer The player to reveal.

@param toWhichPlayers The players who will see whichPlayer's buildings.

@param flag If true, the buildings will be revealed. If false, the buildings
will not be revealed. Note that if you set it to false, it will not hide the buildings with a black mask.
*/
native CripplePlayer takes player whichPlayer, force toWhichPlayers, boolean flag returns nothing



native SetPlayerAbilityAvailable takes player whichPlayer, integer abilid, boolean avail returns nothing



native SetPlayerState takes player whichPlayer, playerstate whichPlayerState, integer value returns nothing

native RemovePlayer takes player whichPlayer, playergameresult gameResult returns nothing



/**
Used to store hero level data for the scorescreen, before units are moved
to neutral passive in melee games.

@param whichPlayer
The player to store hero data for.
*/
native CachePlayerHeroData takes player whichPlayer returns nothing

/**
Decreases (reduces) a specific player’s specific upgrade by a specific amount of levels.

@note Even though this native takes an integer and integers can be both negatives and positive numbers, in this specific case this native does not allow for an increment by setting the integer to negative.

@patch 1.29
*/
native BlzDecPlayerTechResearched                  takes player whichPlayer, integer techid, integer levels returns nothing

/**
@patch 1.32
*/
native BlzGetPlayerTownHallCount                   takes player whichPlayer returns integer

/**
@patch 1.33
*/
native BlzQueueImmediateOrderById      takes unit whichUnit, integer order returns boolean

/**
@patch 1.33
*/
native BlzQueuePointOrderById          takes unit whichUnit, integer order, real x, real y returns boolean

/**
@patch 1.33
*/
native BlzQueueTargetOrderById         takes unit whichUnit, integer order, widget targetWidget returns boolean

/**
@patch 1.33
*/
native BlzQueueInstantPointOrderById   takes unit whichUnit, integer order, real x, real y, widget instantTargetWidget returns boolean

/**
@patch 1.33
*/
native BlzQueueInstantTargetOrderById  takes unit whichUnit, integer order, widget targetWidget, widget instantTargetWidget returns boolean

/**
@patch 1.33
*/
native BlzQueueBuildOrderById          takes unit whichPeon, integer unitId, real x, real y returns boolean

/**
@patch 1.33
*/
native BlzQueueNeutralImmediateOrderById   takes player forWhichPlayer,unit neutralStructure, integer unitId returns boolean

/**
@patch 1.33
*/
native BlzQueueNeutralPointOrderById       takes player forWhichPlayer,unit neutralStructure, integer unitId, real x, real y returns boolean

/**
@patch 1.33
*/
native BlzQueueNeutralTargetOrderById      takes player forWhichPlayer,unit neutralStructure, integer unitId, widget target returns boolean


/**
Returns the number of orders the unit currently has queued up.
@patch 1.33
*/
native BlzGetUnitOrderCount takes unit whichUnit returns integer

/**
Clears either all orders or only queued up orders.
@patch 1.33
*/
native BlzUnitClearOrders takes unit whichUnit, boolean onlyQueued returns nothing

/**
Stops the current order and optionally clears the queue.
@patch 1.33
*/
native BlzUnitForceStopOrder takes unit whichUnit, boolean clearQueue returns nothing
