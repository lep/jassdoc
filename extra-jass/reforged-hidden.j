// These are native functions that are exported by the game's API,
// but left undeclared for modding use for some reason.
// Be cautious if you decide to use them.
// Sorted alphabetically

/**

*/
native BlzDeleteHeroAbility takes unit unitHandle, integer abilID returns nothing // "(Hunit;I)V");


/**
"Exists, but not registered as func." Does it mean it cannot be defined via `native` keyword at all?
*/
native BlzGetDestructableSkin takes destructable destHandle returns integer


/**

*/
native BlzGetHeroPrimaryStat takes unit whichHero returns integer


/**

*/
native BlzGetHeroPrimaryStatById takes unit whichHero returns integer


/**

*/
native BlzGetHeroStat takes unit whichHero, integer whichStat, boolean includeBonuses returns integer


/**

*/
native BlzGetUnitArmorType takes unit whichUnit returns integer


/**

*/
native BlzGetUnitMovementType takes unit whichUnit returns integer


/**

*/
native BlzSetCameraGuardBand takes real minx, real maxx, real miny, real maxy returns boolean // "(RRRR)B");


/**
"Exists, but not registered as func." Does it mean it cannot be defined via `native` keyword at all?
*/
native BlzSetDestructableSkin takes destructable destHandle, integer skinID returns nothing


/**

*/
native BlzSetHeroPrimaryStat takes unit whichHero, integer whichStat returns nothing


/**

*/
native BlzSetHeroStatEx takes unit whichHero, integer whichStat, integer statValue, boolean permanent returns nothing


/**

*/
native BlzSetUnitMovementType takes unit whichUnit, integer movementType returns nothing


/**

*/
native ClearStackedSound takes string soundLabel, real x, real y returns nothing // "(SRR)V");


/**

*/
native ClearStackedSoundRect takes string soundLabel, rect hRect returns nothing // "(SHrect;)V");


/**

*/
native DebugBreak takes integer unused returns nothing // "(I)V");


/**

*/
native DialogSetAsync takes dialog dialogHandle returns nothing // "(Hdialog;)V");


/**
native, but same as the function `GetPlayerStartLocationX` in Blizzard.j?
*/
native GetPlayerStartLocationX takes player whichPlayer returns real


/**
native, but same as the function `GetPlayerStartLocationY` in Blizzard.j?
*/
native GetPlayerStartLocationY takes player whichPlayer returns real


/**

*/
native SetCinematicSceneWithSkinId takes integer skinId, playercolor playerColor, string speaker, string dialogue, real sceneDuration, real dialogueDuration returns nothing // "(IHplayercolor;SSRR)V");


/**

*/
native SetSoundFacialAnimationPlaybackMode takes sound soundHandle, integer facialAnimationPlaybackMode returns boolean // "(Hsound;I)B");


/**

*/
native SetStackedSound takes string soundLabel, real x, real y returns nothing // "(SRR)V");


/**

*/
native SetStackedSoundRect takes string soundLabel, rect hRect returns nothing // "(SHrect;)V");
