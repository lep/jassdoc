// Player API

constant native Player takes integer number returns player

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

constant native GetPlayerHandicapXP takes player whichPlayer returns real

constant native SetPlayerHandicap takes player whichPlayer, real handicap returns nothing

constant native SetPlayerHandicapXP takes player whichPlayer, real handicap returns nothing



constant native SetPlayerTechMaxAllowed takes player whichPlayer, integer techid, integer maximum returns nothing

constant native GetPlayerTechMaxAllowed takes player whichPlayer, integer techid returns integer

constant native AddPlayerTechResearched takes player whichPlayer, integer techid, integer levels returns nothing

constant native SetPlayerTechResearched takes player whichPlayer, integer techid, integer setToLevel returns nothing

constant native GetPlayerTechResearched takes player whichPlayer, integer techid, boolean specificonly returns boolean

constant native GetPlayerTechCount takes player whichPlayer, integer techid, boolean specificonly returns integer



native SetPlayerUnitsOwner takes player whichPlayer, integer newOwner returns nothing

native CripplePlayer takes player whichPlayer, force toWhichPlayers, boolean flag returns nothing



native SetPlayerAbilityAvailable takes player whichPlayer, integer abilid, boolean avail returns nothing



native SetPlayerState takes player whichPlayer, playerstate whichPlayerState, integer value returns nothing

native RemovePlayer takes player whichPlayer, playergameresult gameResult returns nothing



/**
Used to store hero level data for the scorescreen, before units are moved
to neutral passive in melee games.
*/
native CachePlayerHeroData takes player whichPlayer returns nothing
