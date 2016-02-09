//============================================================================

// Native types. All native functions take extended handle types when

// possible to help prevent passing bad values to native functions

//

type agent			    extends     handle  // all reference counted objects

type event              extends     agent  // a reference to an event registration

type player             extends     agent  // a single player reference

type widget             extends     agent  // an interactive game object with life

type unit               extends     widget  // a single unit reference

type destructable       extends     widget

type item               extends     widget

type ability            extends     agent

type buff               extends     ability

type force              extends     agent

type group              extends     agent

type trigger            extends     agent

type triggercondition   extends     agent

type triggeraction      extends     handle

type timer              extends     agent

type location           extends     agent

type region             extends     agent

type rect               extends     agent

type boolexpr           extends     agent

type sound              extends     agent

type conditionfunc      extends     boolexpr

type filterfunc         extends     boolexpr

type unitpool           extends     handle

type itempool           extends     handle

type race               extends     handle

type alliancetype       extends     handle

type racepreference     extends     handle

type gamestate          extends     handle

type igamestate         extends     gamestate

type fgamestate         extends     gamestate

type playerstate        extends     handle

type playerscore        extends     handle

type playergameresult   extends     handle

type unitstate          extends     handle

type aidifficulty       extends     handle



type eventid            extends     handle

type gameevent          extends     eventid

type playerevent        extends     eventid

type playerunitevent    extends     eventid

type unitevent          extends     eventid

type limitop            extends     eventid

type widgetevent        extends     eventid

type dialogevent        extends     eventid

type unittype           extends     handle



type gamespeed          extends     handle

type gamedifficulty     extends     handle

type gametype           extends     handle

type mapflag            extends     handle

type mapvisibility      extends     handle

type mapsetting         extends     handle

type mapdensity         extends     handle

type mapcontrol         extends     handle

type playerslotstate    extends     handle

type volumegroup        extends     handle

type camerafield        extends     handle

type camerasetup        extends     handle

type playercolor        extends     handle

type placement          extends     handle

type startlocprio       extends     handle

type raritycontrol      extends     handle

type blendmode          extends     handle

type texmapflags        extends     handle

type effect             extends     agent

type effecttype         extends     handle

type weathereffect      extends     handle

type terraindeformation extends     handle

type fogstate           extends     handle

type fogmodifier        extends     agent

type dialog             extends     agent

type button             extends     agent

type quest              extends     agent

type questitem          extends     agent

type defeatcondition    extends     agent

type timerdialog        extends     agent

type leaderboard        extends     agent

type multiboard         extends     agent

type multiboarditem     extends     agent

type trackable          extends     agent

type gamecache          extends     agent

type version            extends     handle

type itemtype           extends     handle

type texttag            extends     handle

type attacktype         extends     handle

type damagetype         extends     handle

type weapontype         extends     handle

type soundtype          extends     handle

type lightning          extends     handle

type pathingtype        extends     handle

type image              extends     handle

type ubersplat          extends     handle

type hashtable          extends     agent



constant native ConvertRace                 takes integer i returns race

constant native ConvertAllianceType         takes integer i returns alliancetype

constant native ConvertRacePref             takes integer i returns racepreference

constant native ConvertIGameState           takes integer i returns igamestate

constant native ConvertFGameState           takes integer i returns fgamestate

constant native ConvertPlayerState          takes integer i returns playerstate

constant native ConvertPlayerScore          takes integer i returns playerscore

constant native ConvertPlayerGameResult     takes integer i returns playergameresult

constant native ConvertUnitState            takes integer i returns unitstate

constant native ConvertAIDifficulty         takes integer i returns aidifficulty

constant native ConvertGameEvent            takes integer i returns gameevent

constant native ConvertPlayerEvent          takes integer i returns playerevent

constant native ConvertPlayerUnitEvent      takes integer i returns playerunitevent

constant native ConvertWidgetEvent          takes integer i returns widgetevent

constant native ConvertDialogEvent          takes integer i returns dialogevent

constant native ConvertUnitEvent            takes integer i returns unitevent

constant native ConvertLimitOp              takes integer i returns limitop

constant native ConvertUnitType             takes integer i returns unittype

constant native ConvertGameSpeed            takes integer i returns gamespeed

constant native ConvertPlacement            takes integer i returns placement

constant native ConvertStartLocPrio         takes integer i returns startlocprio

constant native ConvertGameDifficulty       takes integer i returns gamedifficulty

constant native ConvertGameType             takes integer i returns gametype

constant native ConvertMapFlag              takes integer i returns mapflag

constant native ConvertMapVisibility        takes integer i returns mapvisibility

constant native ConvertMapSetting           takes integer i returns mapsetting

constant native ConvertMapDensity           takes integer i returns mapdensity

constant native ConvertMapControl           takes integer i returns mapcontrol

constant native ConvertPlayerColor          takes integer i returns playercolor

constant native ConvertPlayerSlotState      takes integer i returns playerslotstate

constant native ConvertVolumeGroup          takes integer i returns volumegroup

constant native ConvertCameraField          takes integer i returns camerafield

constant native ConvertBlendMode            takes integer i returns blendmode

constant native ConvertRarityControl        takes integer i returns raritycontrol

constant native ConvertTexMapFlags          takes integer i returns texmapflags

/**
@note Can be used for extended typecasting.
<http://www.hiveworkshop.com/forums/j-280/t-232039/>
*/
constant native ConvertFogState             takes integer i returns fogstate

constant native ConvertEffectType           takes integer i returns effecttype

constant native ConvertVersion              takes integer i returns version

constant native ConvertItemType             takes integer i returns itemtype

/**
@note Blizzard only defined attack-types 0 to 6 but there is a hidden one:
ConvertAttackType(7).
<http://www.hiveworkshop.com/forums/t-269/h-227993/>
*/
constant native ConvertAttackType           takes integer i returns attacktype

constant native ConvertDamageType           takes integer i returns damagetype

constant native ConvertWeaponType           takes integer i returns weapontype

constant native ConvertSoundType            takes integer i returns soundtype

constant native ConvertPathingType          takes integer i returns pathingtype



constant native OrderId                     takes string  orderIdString     returns integer

constant native OrderId2String              takes integer orderId           returns string

constant native UnitId                      takes string  unitIdString      returns integer

/**
@bug Always returns null after the game is loaded/if the game is a replay.
*/
constant native UnitId2String               takes integer unitId            returns string


/**
@bug Not working correctly
*/
constant native AbilityId                   takes string  abilityIdString   returns integer

/**
@bug Not working correctly
*/
constant native AbilityId2String            takes integer abilityId         returns string



// Looks up the "name" field for any object (unit, item, ability)

constant native GetObjectName               takes integer objectId          returns string



globals



//===================================================

// Game Constants    

//===================================================



    // pfff

    constant boolean            FALSE                           = false

    constant boolean            TRUE                            = true

    constant integer            JASS_MAX_ARRAY_SIZE             = 8192



    constant integer            PLAYER_NEUTRAL_PASSIVE          = 15

    constant integer            PLAYER_NEUTRAL_AGGRESSIVE       = 12



    constant playercolor        PLAYER_COLOR_RED                = ConvertPlayerColor(0)

    constant playercolor        PLAYER_COLOR_BLUE               = ConvertPlayerColor(1)

    constant playercolor        PLAYER_COLOR_CYAN               = ConvertPlayerColor(2)

    constant playercolor        PLAYER_COLOR_PURPLE             = ConvertPlayerColor(3)

    constant playercolor        PLAYER_COLOR_YELLOW             = ConvertPlayerColor(4)

    constant playercolor        PLAYER_COLOR_ORANGE             = ConvertPlayerColor(5)

    constant playercolor        PLAYER_COLOR_GREEN              = ConvertPlayerColor(6)

    constant playercolor        PLAYER_COLOR_PINK               = ConvertPlayerColor(7)

    constant playercolor        PLAYER_COLOR_LIGHT_GRAY         = ConvertPlayerColor(8)

    constant playercolor        PLAYER_COLOR_LIGHT_BLUE         = ConvertPlayerColor(9)

    constant playercolor        PLAYER_COLOR_AQUA               = ConvertPlayerColor(10)

    constant playercolor        PLAYER_COLOR_BROWN              = ConvertPlayerColor(11)



    constant race               RACE_HUMAN                      = ConvertRace(1)

    constant race               RACE_ORC                        = ConvertRace(2)

    constant race               RACE_UNDEAD                     = ConvertRace(3)

    constant race               RACE_NIGHTELF                   = ConvertRace(4)

    constant race               RACE_DEMON                      = ConvertRace(5)

    constant race               RACE_OTHER                      = ConvertRace(7)



    constant playergameresult   PLAYER_GAME_RESULT_VICTORY      = ConvertPlayerGameResult(0)

    constant playergameresult   PLAYER_GAME_RESULT_DEFEAT       = ConvertPlayerGameResult(1)

    constant playergameresult   PLAYER_GAME_RESULT_TIE          = ConvertPlayerGameResult(2)

    constant playergameresult   PLAYER_GAME_RESULT_NEUTRAL      = ConvertPlayerGameResult(3)



    constant alliancetype       ALLIANCE_PASSIVE                = ConvertAllianceType(0)

    constant alliancetype       ALLIANCE_HELP_REQUEST           = ConvertAllianceType(1)

    constant alliancetype       ALLIANCE_HELP_RESPONSE          = ConvertAllianceType(2)

    constant alliancetype       ALLIANCE_SHARED_XP              = ConvertAllianceType(3)

    constant alliancetype       ALLIANCE_SHARED_SPELLS          = ConvertAllianceType(4)

    constant alliancetype       ALLIANCE_SHARED_VISION          = ConvertAllianceType(5)

    constant alliancetype       ALLIANCE_SHARED_CONTROL         = ConvertAllianceType(6)

    constant alliancetype       ALLIANCE_SHARED_ADVANCED_CONTROL= ConvertAllianceType(7)

    constant alliancetype       ALLIANCE_RESCUABLE              = ConvertAllianceType(8)

    constant alliancetype       ALLIANCE_SHARED_VISION_FORCED   = ConvertAllianceType(9)



    constant version            VERSION_REIGN_OF_CHAOS          = ConvertVersion(0)

    constant version            VERSION_FROZEN_THRONE           = ConvertVersion(1)



    constant attacktype         ATTACK_TYPE_NORMAL              = ConvertAttackType(0)

    constant attacktype         ATTACK_TYPE_MELEE               = ConvertAttackType(1)

    constant attacktype         ATTACK_TYPE_PIERCE              = ConvertAttackType(2)

    constant attacktype         ATTACK_TYPE_SIEGE               = ConvertAttackType(3)

    constant attacktype         ATTACK_TYPE_MAGIC               = ConvertAttackType(4)

    constant attacktype         ATTACK_TYPE_CHAOS               = ConvertAttackType(5)

    constant attacktype         ATTACK_TYPE_HERO                = ConvertAttackType(6)



    constant damagetype         DAMAGE_TYPE_UNKNOWN             = ConvertDamageType(0)

    constant damagetype         DAMAGE_TYPE_NORMAL              = ConvertDamageType(4)

    constant damagetype         DAMAGE_TYPE_ENHANCED            = ConvertDamageType(5)

    constant damagetype         DAMAGE_TYPE_FIRE                = ConvertDamageType(8)

    constant damagetype         DAMAGE_TYPE_COLD                = ConvertDamageType(9)

    constant damagetype         DAMAGE_TYPE_LIGHTNING           = ConvertDamageType(10)

    constant damagetype         DAMAGE_TYPE_POISON              = ConvertDamageType(11)

    constant damagetype         DAMAGE_TYPE_DISEASE             = ConvertDamageType(12)

    constant damagetype         DAMAGE_TYPE_DIVINE              = ConvertDamageType(13)

    constant damagetype         DAMAGE_TYPE_MAGIC               = ConvertDamageType(14)

    constant damagetype         DAMAGE_TYPE_SONIC               = ConvertDamageType(15)

    constant damagetype         DAMAGE_TYPE_ACID                = ConvertDamageType(16)

    constant damagetype         DAMAGE_TYPE_FORCE               = ConvertDamageType(17)

    constant damagetype         DAMAGE_TYPE_DEATH               = ConvertDamageType(18)

    constant damagetype         DAMAGE_TYPE_MIND                = ConvertDamageType(19)

    constant damagetype         DAMAGE_TYPE_PLANT               = ConvertDamageType(20)

    constant damagetype         DAMAGE_TYPE_DEFENSIVE           = ConvertDamageType(21)

    constant damagetype         DAMAGE_TYPE_DEMOLITION          = ConvertDamageType(22)

    constant damagetype         DAMAGE_TYPE_SLOW_POISON         = ConvertDamageType(23)

    constant damagetype         DAMAGE_TYPE_SPIRIT_LINK         = ConvertDamageType(24)

    constant damagetype         DAMAGE_TYPE_SHADOW_STRIKE       = ConvertDamageType(25)

    constant damagetype         DAMAGE_TYPE_UNIVERSAL           = ConvertDamageType(26)



    constant weapontype         WEAPON_TYPE_WHOKNOWS            = ConvertWeaponType(0)

    constant weapontype         WEAPON_TYPE_METAL_LIGHT_CHOP    = ConvertWeaponType(1)

    constant weapontype         WEAPON_TYPE_METAL_MEDIUM_CHOP   = ConvertWeaponType(2)

    constant weapontype         WEAPON_TYPE_METAL_HEAVY_CHOP    = ConvertWeaponType(3)

    constant weapontype         WEAPON_TYPE_METAL_LIGHT_SLICE   = ConvertWeaponType(4)

    constant weapontype         WEAPON_TYPE_METAL_MEDIUM_SLICE  = ConvertWeaponType(5)

    constant weapontype         WEAPON_TYPE_METAL_HEAVY_SLICE   = ConvertWeaponType(6)

    constant weapontype         WEAPON_TYPE_METAL_MEDIUM_BASH   = ConvertWeaponType(7)

    constant weapontype         WEAPON_TYPE_METAL_HEAVY_BASH    = ConvertWeaponType(8)

    constant weapontype         WEAPON_TYPE_METAL_MEDIUM_STAB   = ConvertWeaponType(9)

    constant weapontype         WEAPON_TYPE_METAL_HEAVY_STAB    = ConvertWeaponType(10)

    constant weapontype         WEAPON_TYPE_WOOD_LIGHT_SLICE    = ConvertWeaponType(11)

    constant weapontype         WEAPON_TYPE_WOOD_MEDIUM_SLICE   = ConvertWeaponType(12)

    constant weapontype         WEAPON_TYPE_WOOD_HEAVY_SLICE    = ConvertWeaponType(13)

    constant weapontype         WEAPON_TYPE_WOOD_LIGHT_BASH     = ConvertWeaponType(14)

    constant weapontype         WEAPON_TYPE_WOOD_MEDIUM_BASH    = ConvertWeaponType(15)

    constant weapontype         WEAPON_TYPE_WOOD_HEAVY_BASH     = ConvertWeaponType(16)

    constant weapontype         WEAPON_TYPE_WOOD_LIGHT_STAB     = ConvertWeaponType(17)

    constant weapontype         WEAPON_TYPE_WOOD_MEDIUM_STAB    = ConvertWeaponType(18)

    constant weapontype         WEAPON_TYPE_CLAW_LIGHT_SLICE    = ConvertWeaponType(19)

    constant weapontype         WEAPON_TYPE_CLAW_MEDIUM_SLICE   = ConvertWeaponType(20)

    constant weapontype         WEAPON_TYPE_CLAW_HEAVY_SLICE    = ConvertWeaponType(21)

    constant weapontype         WEAPON_TYPE_AXE_MEDIUM_CHOP     = ConvertWeaponType(22)

    constant weapontype         WEAPON_TYPE_ROCK_HEAVY_BASH     = ConvertWeaponType(23)



    constant pathingtype        PATHING_TYPE_ANY                = ConvertPathingType(0)

    constant pathingtype        PATHING_TYPE_WALKABILITY        = ConvertPathingType(1)

    constant pathingtype        PATHING_TYPE_FLYABILITY         = ConvertPathingType(2)

    constant pathingtype        PATHING_TYPE_BUILDABILITY       = ConvertPathingType(3)

    constant pathingtype        PATHING_TYPE_PEONHARVESTPATHING = ConvertPathingType(4)

    constant pathingtype        PATHING_TYPE_BLIGHTPATHING      = ConvertPathingType(5)

    constant pathingtype        PATHING_TYPE_FLOATABILITY       = ConvertPathingType(6)

    constant pathingtype        PATHING_TYPE_AMPHIBIOUSPATHING  = ConvertPathingType(7)



//===================================================

// Map Setup Constants    

//===================================================



    constant racepreference     RACE_PREF_HUMAN                     = ConvertRacePref(1)

    constant racepreference     RACE_PREF_ORC                       = ConvertRacePref(2)

    constant racepreference     RACE_PREF_NIGHTELF                  = ConvertRacePref(4)

    constant racepreference     RACE_PREF_UNDEAD                    = ConvertRacePref(8)

    constant racepreference     RACE_PREF_DEMON                     = ConvertRacePref(16)

    constant racepreference     RACE_PREF_RANDOM                    = ConvertRacePref(32)

    constant racepreference     RACE_PREF_USER_SELECTABLE           = ConvertRacePref(64)



    constant mapcontrol         MAP_CONTROL_USER                    = ConvertMapControl(0)

    constant mapcontrol         MAP_CONTROL_COMPUTER                = ConvertMapControl(1)

    constant mapcontrol         MAP_CONTROL_RESCUABLE               = ConvertMapControl(2)

    constant mapcontrol         MAP_CONTROL_NEUTRAL                 = ConvertMapControl(3)

    constant mapcontrol         MAP_CONTROL_CREEP                   = ConvertMapControl(4)

    constant mapcontrol         MAP_CONTROL_NONE                    = ConvertMapControl(5)



    constant gametype           GAME_TYPE_MELEE                     = ConvertGameType(1)

    constant gametype           GAME_TYPE_FFA                       = ConvertGameType(2)

    constant gametype           GAME_TYPE_USE_MAP_SETTINGS          = ConvertGameType(4)

    constant gametype           GAME_TYPE_BLIZ                      = ConvertGameType(8)

    constant gametype           GAME_TYPE_ONE_ON_ONE                = ConvertGameType(16)

    constant gametype           GAME_TYPE_TWO_TEAM_PLAY             = ConvertGameType(32)

    constant gametype           GAME_TYPE_THREE_TEAM_PLAY           = ConvertGameType(64)

    constant gametype           GAME_TYPE_FOUR_TEAM_PLAY            = ConvertGameType(128)



    constant mapflag            MAP_FOG_HIDE_TERRAIN                = ConvertMapFlag(1)

    constant mapflag            MAP_FOG_MAP_EXPLORED                = ConvertMapFlag(2)

    constant mapflag            MAP_FOG_ALWAYS_VISIBLE              = ConvertMapFlag(4)



    constant mapflag            MAP_USE_HANDICAPS                   = ConvertMapFlag(8)

    constant mapflag            MAP_OBSERVERS                       = ConvertMapFlag(16)

    constant mapflag            MAP_OBSERVERS_ON_DEATH              = ConvertMapFlag(32)



    constant mapflag            MAP_FIXED_COLORS                    = ConvertMapFlag(128)

    

    constant mapflag            MAP_LOCK_RESOURCE_TRADING           = ConvertMapFlag(256)

    constant mapflag            MAP_RESOURCE_TRADING_ALLIES_ONLY    = ConvertMapFlag(512)



    constant mapflag            MAP_LOCK_ALLIANCE_CHANGES           = ConvertMapFlag(1024)

    constant mapflag            MAP_ALLIANCE_CHANGES_HIDDEN         = ConvertMapFlag(2048)



    constant mapflag            MAP_CHEATS                          = ConvertMapFlag(4096)

    constant mapflag            MAP_CHEATS_HIDDEN                   = ConvertMapFlag(8192)



    constant mapflag            MAP_LOCK_SPEED                      = ConvertMapFlag(8192*2)

    constant mapflag            MAP_LOCK_RANDOM_SEED                = ConvertMapFlag(8192*4)

    constant mapflag            MAP_SHARED_ADVANCED_CONTROL         = ConvertMapFlag(8192*8)

    constant mapflag            MAP_RANDOM_HERO                     = ConvertMapFlag(8192*16)

    constant mapflag            MAP_RANDOM_RACES                    = ConvertMapFlag(8192*32)

    constant mapflag            MAP_RELOADED                        = ConvertMapFlag(8192*64)



    constant placement          MAP_PLACEMENT_RANDOM                = ConvertPlacement(0)   // random among all slots

    constant placement          MAP_PLACEMENT_FIXED                 = ConvertPlacement(1)   // player 0 in start loc 0...

    constant placement          MAP_PLACEMENT_USE_MAP_SETTINGS      = ConvertPlacement(2)   // whatever was specified by the script

    constant placement          MAP_PLACEMENT_TEAMS_TOGETHER        = ConvertPlacement(3)   // random with allies next to each other    



    constant startlocprio       MAP_LOC_PRIO_LOW                    = ConvertStartLocPrio(0)

    constant startlocprio       MAP_LOC_PRIO_HIGH                   = ConvertStartLocPrio(1)

    constant startlocprio       MAP_LOC_PRIO_NOT                    = ConvertStartLocPrio(2)



    constant mapdensity         MAP_DENSITY_NONE                    = ConvertMapDensity(0)

    constant mapdensity         MAP_DENSITY_LIGHT                   = ConvertMapDensity(1)

    constant mapdensity         MAP_DENSITY_MEDIUM                  = ConvertMapDensity(2)

    constant mapdensity         MAP_DENSITY_HEAVY                   = ConvertMapDensity(3)



    constant gamedifficulty     MAP_DIFFICULTY_EASY                 = ConvertGameDifficulty(0)

    constant gamedifficulty     MAP_DIFFICULTY_NORMAL               = ConvertGameDifficulty(1)

    constant gamedifficulty     MAP_DIFFICULTY_HARD                 = ConvertGameDifficulty(2)

    constant gamedifficulty     MAP_DIFFICULTY_INSANE               = ConvertGameDifficulty(3)



    constant gamespeed          MAP_SPEED_SLOWEST                   = ConvertGameSpeed(0)

    constant gamespeed          MAP_SPEED_SLOW                      = ConvertGameSpeed(1)

    constant gamespeed          MAP_SPEED_NORMAL                    = ConvertGameSpeed(2)

    constant gamespeed          MAP_SPEED_FAST                      = ConvertGameSpeed(3)

    constant gamespeed          MAP_SPEED_FASTEST                   = ConvertGameSpeed(4)



    constant playerslotstate    PLAYER_SLOT_STATE_EMPTY             = ConvertPlayerSlotState(0)

    constant playerslotstate    PLAYER_SLOT_STATE_PLAYING           = ConvertPlayerSlotState(1)

    constant playerslotstate    PLAYER_SLOT_STATE_LEFT              = ConvertPlayerSlotState(2)



//===================================================

// Sound Constants

//===================================================

    constant volumegroup        SOUND_VOLUMEGROUP_UNITMOVEMENT      = ConvertVolumeGroup(0)

    constant volumegroup        SOUND_VOLUMEGROUP_UNITSOUNDS        = ConvertVolumeGroup(1)

    constant volumegroup        SOUND_VOLUMEGROUP_COMBAT            = ConvertVolumeGroup(2)

    constant volumegroup        SOUND_VOLUMEGROUP_SPELLS            = ConvertVolumeGroup(3)

    constant volumegroup        SOUND_VOLUMEGROUP_UI                = ConvertVolumeGroup(4)

    constant volumegroup        SOUND_VOLUMEGROUP_MUSIC             = ConvertVolumeGroup(5)

    constant volumegroup        SOUND_VOLUMEGROUP_AMBIENTSOUNDS     = ConvertVolumeGroup(6)

    constant volumegroup        SOUND_VOLUMEGROUP_FIRE              = ConvertVolumeGroup(7)





//===================================================

// Game, Player, and Unit States

//

// For use with TriggerRegister<X>StateEvent

//

//===================================================



    constant igamestate GAME_STATE_DIVINE_INTERVENTION          = ConvertIGameState(0)

    constant igamestate GAME_STATE_DISCONNECTED                 = ConvertIGameState(1)

    constant fgamestate GAME_STATE_TIME_OF_DAY                  = ConvertFGameState(2)



    constant playerstate PLAYER_STATE_GAME_RESULT               = ConvertPlayerState(0)



    // current resource levels

    //

    constant playerstate PLAYER_STATE_RESOURCE_GOLD             = ConvertPlayerState(1)

    constant playerstate PLAYER_STATE_RESOURCE_LUMBER           = ConvertPlayerState(2)

    constant playerstate PLAYER_STATE_RESOURCE_HERO_TOKENS      = ConvertPlayerState(3)

    constant playerstate PLAYER_STATE_RESOURCE_FOOD_CAP         = ConvertPlayerState(4)

    constant playerstate PLAYER_STATE_RESOURCE_FOOD_USED        = ConvertPlayerState(5)

    constant playerstate PLAYER_STATE_FOOD_CAP_CEILING          = ConvertPlayerState(6)



    constant playerstate PLAYER_STATE_GIVES_BOUNTY              = ConvertPlayerState(7)

    constant playerstate PLAYER_STATE_ALLIED_VICTORY            = ConvertPlayerState(8)

    constant playerstate PLAYER_STATE_PLACED                    = ConvertPlayerState(9)

    constant playerstate PLAYER_STATE_OBSERVER_ON_DEATH         = ConvertPlayerState(10)

    constant playerstate PLAYER_STATE_OBSERVER                  = ConvertPlayerState(11)

    constant playerstate PLAYER_STATE_UNFOLLOWABLE              = ConvertPlayerState(12)



    // taxation rate for each resource

    //

    constant playerstate PLAYER_STATE_GOLD_UPKEEP_RATE          = ConvertPlayerState(13)

    constant playerstate PLAYER_STATE_LUMBER_UPKEEP_RATE        = ConvertPlayerState(14)



    // cumulative resources collected by the player during the mission

    //

    constant playerstate PLAYER_STATE_GOLD_GATHERED             = ConvertPlayerState(15)

    constant playerstate PLAYER_STATE_LUMBER_GATHERED           = ConvertPlayerState(16)



    constant playerstate PLAYER_STATE_NO_CREEP_SLEEP            = ConvertPlayerState(25)



    constant unitstate UNIT_STATE_LIFE                          = ConvertUnitState(0)

    constant unitstate UNIT_STATE_MAX_LIFE                      = ConvertUnitState(1)

    constant unitstate UNIT_STATE_MANA                          = ConvertUnitState(2)

    constant unitstate UNIT_STATE_MAX_MANA                      = ConvertUnitState(3)



    constant aidifficulty AI_DIFFICULTY_NEWBIE                  = ConvertAIDifficulty(0)

    constant aidifficulty AI_DIFFICULTY_NORMAL                  = ConvertAIDifficulty(1)

    constant aidifficulty AI_DIFFICULTY_INSANE                  = ConvertAIDifficulty(2)



    // player score values

    constant playerscore PLAYER_SCORE_UNITS_TRAINED             = ConvertPlayerScore(0)

    constant playerscore PLAYER_SCORE_UNITS_KILLED              = ConvertPlayerScore(1)

    constant playerscore PLAYER_SCORE_STRUCT_BUILT              = ConvertPlayerScore(2)

    constant playerscore PLAYER_SCORE_STRUCT_RAZED              = ConvertPlayerScore(3)

    constant playerscore PLAYER_SCORE_TECH_PERCENT              = ConvertPlayerScore(4)

    constant playerscore PLAYER_SCORE_FOOD_MAXPROD              = ConvertPlayerScore(5)

    constant playerscore PLAYER_SCORE_FOOD_MAXUSED              = ConvertPlayerScore(6)

    constant playerscore PLAYER_SCORE_HEROES_KILLED             = ConvertPlayerScore(7)

    constant playerscore PLAYER_SCORE_ITEMS_GAINED              = ConvertPlayerScore(8)

    constant playerscore PLAYER_SCORE_MERCS_HIRED               = ConvertPlayerScore(9)

    constant playerscore PLAYER_SCORE_GOLD_MINED_TOTAL          = ConvertPlayerScore(10)

    constant playerscore PLAYER_SCORE_GOLD_MINED_UPKEEP         = ConvertPlayerScore(11)

    constant playerscore PLAYER_SCORE_GOLD_LOST_UPKEEP          = ConvertPlayerScore(12)

    constant playerscore PLAYER_SCORE_GOLD_LOST_TAX             = ConvertPlayerScore(13)

    constant playerscore PLAYER_SCORE_GOLD_GIVEN                = ConvertPlayerScore(14)

    constant playerscore PLAYER_SCORE_GOLD_RECEIVED             = ConvertPlayerScore(15)

    constant playerscore PLAYER_SCORE_LUMBER_TOTAL              = ConvertPlayerScore(16)

    constant playerscore PLAYER_SCORE_LUMBER_LOST_UPKEEP        = ConvertPlayerScore(17)

    constant playerscore PLAYER_SCORE_LUMBER_LOST_TAX           = ConvertPlayerScore(18)

    constant playerscore PLAYER_SCORE_LUMBER_GIVEN              = ConvertPlayerScore(19)

    constant playerscore PLAYER_SCORE_LUMBER_RECEIVED           = ConvertPlayerScore(20)

    constant playerscore PLAYER_SCORE_UNIT_TOTAL                = ConvertPlayerScore(21)

    constant playerscore PLAYER_SCORE_HERO_TOTAL                = ConvertPlayerScore(22)

    constant playerscore PLAYER_SCORE_RESOURCE_TOTAL            = ConvertPlayerScore(23)

    constant playerscore PLAYER_SCORE_TOTAL                     = ConvertPlayerScore(24)

        

//===================================================

// Game, Player and Unit Events

//

//  When an event causes a trigger to fire these

//  values allow the action code to determine which

//  event was dispatched and therefore which set of

//  native functions should be used to get information

//  about the event.

//

// Do NOT change the order or value of these constants

// without insuring that the JASS_GAME_EVENTS_WAR3 enum

// is changed to match.

//

//===================================================



    //===================================================

    // For use with TriggerRegisterGameEvent    

    //===================================================    



    constant gameevent EVENT_GAME_VICTORY                       = ConvertGameEvent(0)

    constant gameevent EVENT_GAME_END_LEVEL                     = ConvertGameEvent(1)



    constant gameevent EVENT_GAME_VARIABLE_LIMIT                = ConvertGameEvent(2)

    constant gameevent EVENT_GAME_STATE_LIMIT                   = ConvertGameEvent(3)   



    constant gameevent EVENT_GAME_TIMER_EXPIRED                 = ConvertGameEvent(4)



    constant gameevent EVENT_GAME_ENTER_REGION                  = ConvertGameEvent(5)

    constant gameevent EVENT_GAME_LEAVE_REGION                  = ConvertGameEvent(6)



    constant gameevent EVENT_GAME_TRACKABLE_HIT                 = ConvertGameEvent(7)

    constant gameevent EVENT_GAME_TRACKABLE_TRACK               = ConvertGameEvent(8)



    constant gameevent EVENT_GAME_SHOW_SKILL                    = ConvertGameEvent(9)    

    constant gameevent EVENT_GAME_BUILD_SUBMENU                 = ConvertGameEvent(10)



    //===================================================

    // For use with TriggerRegisterPlayerEvent

    //===================================================

    constant playerevent EVENT_PLAYER_STATE_LIMIT               = ConvertPlayerEvent(11)

    constant playerevent EVENT_PLAYER_ALLIANCE_CHANGED          = ConvertPlayerEvent(12)



    constant playerevent EVENT_PLAYER_DEFEAT                    = ConvertPlayerEvent(13)

    constant playerevent EVENT_PLAYER_VICTORY                   = ConvertPlayerEvent(14)

    constant playerevent EVENT_PLAYER_LEAVE                     = ConvertPlayerEvent(15)

    constant playerevent EVENT_PLAYER_CHAT                      = ConvertPlayerEvent(16)

    constant playerevent EVENT_PLAYER_END_CINEMATIC             = ConvertPlayerEvent(17)



    //===================================================

    // For use with TriggerRegisterPlayerUnitEvent

    //===================================================



    constant playerunitevent EVENT_PLAYER_UNIT_ATTACKED                 = ConvertPlayerUnitEvent(18)

    constant playerunitevent EVENT_PLAYER_UNIT_RESCUED                  = ConvertPlayerUnitEvent(19)



    constant playerunitevent EVENT_PLAYER_UNIT_DEATH                    = ConvertPlayerUnitEvent(20)

    constant playerunitevent EVENT_PLAYER_UNIT_DECAY                    = ConvertPlayerUnitEvent(21)



    constant playerunitevent EVENT_PLAYER_UNIT_DETECTED                 = ConvertPlayerUnitEvent(22)

    constant playerunitevent EVENT_PLAYER_UNIT_HIDDEN                   = ConvertPlayerUnitEvent(23)



    constant playerunitevent EVENT_PLAYER_UNIT_SELECTED                 = ConvertPlayerUnitEvent(24)

    constant playerunitevent EVENT_PLAYER_UNIT_DESELECTED               = ConvertPlayerUnitEvent(25)



    constant playerunitevent EVENT_PLAYER_UNIT_CONSTRUCT_START          = ConvertPlayerUnitEvent(26)

    constant playerunitevent EVENT_PLAYER_UNIT_CONSTRUCT_CANCEL         = ConvertPlayerUnitEvent(27)

    constant playerunitevent EVENT_PLAYER_UNIT_CONSTRUCT_FINISH         = ConvertPlayerUnitEvent(28)



    constant playerunitevent EVENT_PLAYER_UNIT_UPGRADE_START            = ConvertPlayerUnitEvent(29)

    constant playerunitevent EVENT_PLAYER_UNIT_UPGRADE_CANCEL           = ConvertPlayerUnitEvent(30)

    constant playerunitevent EVENT_PLAYER_UNIT_UPGRADE_FINISH           = ConvertPlayerUnitEvent(31)



    constant playerunitevent EVENT_PLAYER_UNIT_TRAIN_START              = ConvertPlayerUnitEvent(32)

    constant playerunitevent EVENT_PLAYER_UNIT_TRAIN_CANCEL             = ConvertPlayerUnitEvent(33)

    constant playerunitevent EVENT_PLAYER_UNIT_TRAIN_FINISH             = ConvertPlayerUnitEvent(34)



    constant playerunitevent EVENT_PLAYER_UNIT_RESEARCH_START           = ConvertPlayerUnitEvent(35)

    constant playerunitevent EVENT_PLAYER_UNIT_RESEARCH_CANCEL          = ConvertPlayerUnitEvent(36)

    constant playerunitevent EVENT_PLAYER_UNIT_RESEARCH_FINISH          = ConvertPlayerUnitEvent(37)

    constant playerunitevent EVENT_PLAYER_UNIT_ISSUED_ORDER             = ConvertPlayerUnitEvent(38)

    constant playerunitevent EVENT_PLAYER_UNIT_ISSUED_POINT_ORDER       = ConvertPlayerUnitEvent(39)

    constant playerunitevent EVENT_PLAYER_UNIT_ISSUED_TARGET_ORDER      = ConvertPlayerUnitEvent(40)

    constant playerunitevent EVENT_PLAYER_UNIT_ISSUED_UNIT_ORDER        = ConvertPlayerUnitEvent(40)    // for compat



    constant playerunitevent EVENT_PLAYER_HERO_LEVEL                    = ConvertPlayerUnitEvent(41)

    constant playerunitevent EVENT_PLAYER_HERO_SKILL                    = ConvertPlayerUnitEvent(42)



    constant playerunitevent EVENT_PLAYER_HERO_REVIVABLE                = ConvertPlayerUnitEvent(43)



    constant playerunitevent EVENT_PLAYER_HERO_REVIVE_START             = ConvertPlayerUnitEvent(44)

    constant playerunitevent EVENT_PLAYER_HERO_REVIVE_CANCEL            = ConvertPlayerUnitEvent(45)

    constant playerunitevent EVENT_PLAYER_HERO_REVIVE_FINISH            = ConvertPlayerUnitEvent(46)

    constant playerunitevent EVENT_PLAYER_UNIT_SUMMON                   = ConvertPlayerUnitEvent(47)

    constant playerunitevent EVENT_PLAYER_UNIT_DROP_ITEM                = ConvertPlayerUnitEvent(48)

    constant playerunitevent EVENT_PLAYER_UNIT_PICKUP_ITEM              = ConvertPlayerUnitEvent(49)

    constant playerunitevent EVENT_PLAYER_UNIT_USE_ITEM                 = ConvertPlayerUnitEvent(50)



    constant playerunitevent EVENT_PLAYER_UNIT_LOADED                   = ConvertPlayerUnitEvent(51)

    

    //===================================================

    // For use with TriggerRegisterUnitEvent

    //===================================================



    constant unitevent EVENT_UNIT_DAMAGED                               = ConvertUnitEvent(52)

    constant unitevent EVENT_UNIT_DEATH                                 = ConvertUnitEvent(53)

    constant unitevent EVENT_UNIT_DECAY                                 = ConvertUnitEvent(54)

    constant unitevent EVENT_UNIT_DETECTED                              = ConvertUnitEvent(55)

    constant unitevent EVENT_UNIT_HIDDEN                                = ConvertUnitEvent(56)

    constant unitevent EVENT_UNIT_SELECTED                              = ConvertUnitEvent(57)

    constant unitevent EVENT_UNIT_DESELECTED                            = ConvertUnitEvent(58)

                                                                        

    constant unitevent EVENT_UNIT_STATE_LIMIT                           = ConvertUnitEvent(59)                                                                        



    // Events which may have a filter for the "other unit"              

    //                                                                  

    constant unitevent EVENT_UNIT_ACQUIRED_TARGET                       = ConvertUnitEvent(60)

    constant unitevent EVENT_UNIT_TARGET_IN_RANGE                       = ConvertUnitEvent(61)

    constant unitevent EVENT_UNIT_ATTACKED                              = ConvertUnitEvent(62)

    constant unitevent EVENT_UNIT_RESCUED                               = ConvertUnitEvent(63)

                                                                        

    constant unitevent EVENT_UNIT_CONSTRUCT_CANCEL                      = ConvertUnitEvent(64)

    constant unitevent EVENT_UNIT_CONSTRUCT_FINISH                      = ConvertUnitEvent(65)

                                                                        

    constant unitevent EVENT_UNIT_UPGRADE_START                         = ConvertUnitEvent(66)

    constant unitevent EVENT_UNIT_UPGRADE_CANCEL                        = ConvertUnitEvent(67)

    constant unitevent EVENT_UNIT_UPGRADE_FINISH                        = ConvertUnitEvent(68)

                                                                        

    // Events which involve the specified unit performing               

    // training of other units                                          

    //                                                                  

    constant unitevent EVENT_UNIT_TRAIN_START                           = ConvertUnitEvent(69)

    constant unitevent EVENT_UNIT_TRAIN_CANCEL                          = ConvertUnitEvent(70)

    constant unitevent EVENT_UNIT_TRAIN_FINISH                          = ConvertUnitEvent(71)

                                                                        

    constant unitevent EVENT_UNIT_RESEARCH_START                        = ConvertUnitEvent(72)

    constant unitevent EVENT_UNIT_RESEARCH_CANCEL                       = ConvertUnitEvent(73)

    constant unitevent EVENT_UNIT_RESEARCH_FINISH                       = ConvertUnitEvent(74)

                                                                        

    constant unitevent EVENT_UNIT_ISSUED_ORDER                          = ConvertUnitEvent(75)

    constant unitevent EVENT_UNIT_ISSUED_POINT_ORDER                    = ConvertUnitEvent(76)

    constant unitevent EVENT_UNIT_ISSUED_TARGET_ORDER                   = ConvertUnitEvent(77)

                                                                       

    constant unitevent EVENT_UNIT_HERO_LEVEL                            = ConvertUnitEvent(78)

    constant unitevent EVENT_UNIT_HERO_SKILL                            = ConvertUnitEvent(79)

                                                                        

    constant unitevent EVENT_UNIT_HERO_REVIVABLE                        = ConvertUnitEvent(80)

    constant unitevent EVENT_UNIT_HERO_REVIVE_START                     = ConvertUnitEvent(81)

    constant unitevent EVENT_UNIT_HERO_REVIVE_CANCEL                    = ConvertUnitEvent(82)

    constant unitevent EVENT_UNIT_HERO_REVIVE_FINISH                    = ConvertUnitEvent(83)

                                                                        

    constant unitevent EVENT_UNIT_SUMMON                                = ConvertUnitEvent(84)

                                                                        

    constant unitevent EVENT_UNIT_DROP_ITEM                             = ConvertUnitEvent(85)

    constant unitevent EVENT_UNIT_PICKUP_ITEM                           = ConvertUnitEvent(86)

    constant unitevent EVENT_UNIT_USE_ITEM                              = ConvertUnitEvent(87)



    constant unitevent EVENT_UNIT_LOADED                                = ConvertUnitEvent(88)



    constant widgetevent EVENT_WIDGET_DEATH                             = ConvertWidgetEvent(89)



    constant dialogevent EVENT_DIALOG_BUTTON_CLICK                      = ConvertDialogEvent(90)

    constant dialogevent EVENT_DIALOG_CLICK                             = ConvertDialogEvent(91)



    //===================================================

    // Frozen Throne Expansion Events

    // Need to be added here to preserve compat

    //===================================================



    //===================================================

    // For use with TriggerRegisterGameEvent    

    //===================================================    



    constant gameevent          EVENT_GAME_LOADED                       = ConvertGameEvent(256)

    constant gameevent          EVENT_GAME_TOURNAMENT_FINISH_SOON       = ConvertGameEvent(257)

    constant gameevent          EVENT_GAME_TOURNAMENT_FINISH_NOW        = ConvertGameEvent(258)

    constant gameevent          EVENT_GAME_SAVE                         = ConvertGameEvent(259)



    //===================================================

    // For use with TriggerRegisterPlayerEvent

    //===================================================



    constant playerevent        EVENT_PLAYER_ARROW_LEFT_DOWN            = ConvertPlayerEvent(261)

    constant playerevent        EVENT_PLAYER_ARROW_LEFT_UP              = ConvertPlayerEvent(262)

    constant playerevent        EVENT_PLAYER_ARROW_RIGHT_DOWN           = ConvertPlayerEvent(263)

    constant playerevent        EVENT_PLAYER_ARROW_RIGHT_UP             = ConvertPlayerEvent(264)

    constant playerevent        EVENT_PLAYER_ARROW_DOWN_DOWN            = ConvertPlayerEvent(265)

    constant playerevent        EVENT_PLAYER_ARROW_DOWN_UP              = ConvertPlayerEvent(266)

    constant playerevent        EVENT_PLAYER_ARROW_UP_DOWN              = ConvertPlayerEvent(267)

    constant playerevent        EVENT_PLAYER_ARROW_UP_UP                = ConvertPlayerEvent(268)



    //===================================================

    // For use with TriggerRegisterPlayerUnitEvent

    //===================================================



    constant playerunitevent    EVENT_PLAYER_UNIT_SELL                  = ConvertPlayerUnitEvent(269)

    constant playerunitevent    EVENT_PLAYER_UNIT_CHANGE_OWNER          = ConvertPlayerUnitEvent(270)

    constant playerunitevent    EVENT_PLAYER_UNIT_SELL_ITEM             = ConvertPlayerUnitEvent(271)

    constant playerunitevent    EVENT_PLAYER_UNIT_SPELL_CHANNEL         = ConvertPlayerUnitEvent(272)

    constant playerunitevent    EVENT_PLAYER_UNIT_SPELL_CAST            = ConvertPlayerUnitEvent(273)

    constant playerunitevent    EVENT_PLAYER_UNIT_SPELL_EFFECT          = ConvertPlayerUnitEvent(274)

    constant playerunitevent    EVENT_PLAYER_UNIT_SPELL_FINISH          = ConvertPlayerUnitEvent(275)

    constant playerunitevent    EVENT_PLAYER_UNIT_SPELL_ENDCAST         = ConvertPlayerUnitEvent(276)

    constant playerunitevent    EVENT_PLAYER_UNIT_PAWN_ITEM             = ConvertPlayerUnitEvent(277)



    //===================================================

    // For use with TriggerRegisterUnitEvent

    //===================================================



    constant unitevent          EVENT_UNIT_SELL                         = ConvertUnitEvent(286)

    constant unitevent          EVENT_UNIT_CHANGE_OWNER                 = ConvertUnitEvent(287)

    constant unitevent          EVENT_UNIT_SELL_ITEM                    = ConvertUnitEvent(288)

    constant unitevent          EVENT_UNIT_SPELL_CHANNEL                = ConvertUnitEvent(289)

    constant unitevent          EVENT_UNIT_SPELL_CAST                   = ConvertUnitEvent(290)

    constant unitevent          EVENT_UNIT_SPELL_EFFECT                 = ConvertUnitEvent(291)

    constant unitevent          EVENT_UNIT_SPELL_FINISH                 = ConvertUnitEvent(292)

    constant unitevent          EVENT_UNIT_SPELL_ENDCAST                = ConvertUnitEvent(293)

    constant unitevent          EVENT_UNIT_PAWN_ITEM                    = ConvertUnitEvent(294)



    //===================================================

    // Limit Event API constants    

    // variable, player state, game state, and unit state events

    // ( do NOT change the order of these... )

    //===================================================

    constant limitop LESS_THAN                              = ConvertLimitOp(0)

    constant limitop LESS_THAN_OR_EQUAL                     = ConvertLimitOp(1)

    constant limitop EQUAL                                  = ConvertLimitOp(2)

    constant limitop GREATER_THAN_OR_EQUAL                  = ConvertLimitOp(3)

    constant limitop GREATER_THAN                           = ConvertLimitOp(4)

    constant limitop NOT_EQUAL                              = ConvertLimitOp(5)



//===================================================

// Unit Type Constants for use with IsUnitType()

//===================================================



    constant unittype UNIT_TYPE_HERO                        = ConvertUnitType(0)

    constant unittype UNIT_TYPE_DEAD                        = ConvertUnitType(1)

    constant unittype UNIT_TYPE_STRUCTURE                   = ConvertUnitType(2)



    constant unittype UNIT_TYPE_FLYING                      = ConvertUnitType(3)

    constant unittype UNIT_TYPE_GROUND                      = ConvertUnitType(4)



    constant unittype UNIT_TYPE_ATTACKS_FLYING              = ConvertUnitType(5)

    constant unittype UNIT_TYPE_ATTACKS_GROUND              = ConvertUnitType(6)



    constant unittype UNIT_TYPE_MELEE_ATTACKER              = ConvertUnitType(7)

    constant unittype UNIT_TYPE_RANGED_ATTACKER             = ConvertUnitType(8)



    constant unittype UNIT_TYPE_GIANT                       = ConvertUnitType(9)

    constant unittype UNIT_TYPE_SUMMONED                    = ConvertUnitType(10)

    constant unittype UNIT_TYPE_STUNNED                     = ConvertUnitType(11)

    constant unittype UNIT_TYPE_PLAGUED                     = ConvertUnitType(12)

    constant unittype UNIT_TYPE_SNARED                      = ConvertUnitType(13)



    constant unittype UNIT_TYPE_UNDEAD                      = ConvertUnitType(14)

    constant unittype UNIT_TYPE_MECHANICAL                  = ConvertUnitType(15)

    constant unittype UNIT_TYPE_PEON                        = ConvertUnitType(16)

    constant unittype UNIT_TYPE_SAPPER                      = ConvertUnitType(17)

    constant unittype UNIT_TYPE_TOWNHALL                    = ConvertUnitType(18)    

    constant unittype UNIT_TYPE_ANCIENT                     = ConvertUnitType(19)

    

    constant unittype UNIT_TYPE_TAUREN                      = ConvertUnitType(20)

    constant unittype UNIT_TYPE_POISONED                    = ConvertUnitType(21)

    constant unittype UNIT_TYPE_POLYMORPHED                 = ConvertUnitType(22)

    constant unittype UNIT_TYPE_SLEEPING                    = ConvertUnitType(23)

    constant unittype UNIT_TYPE_RESISTANT                   = ConvertUnitType(24)

    constant unittype UNIT_TYPE_ETHEREAL                    = ConvertUnitType(25)

    constant unittype UNIT_TYPE_MAGIC_IMMUNE                = ConvertUnitType(26)



//===================================================

// Unit Type Constants for use with ChooseRandomItemEx()

//===================================================



    constant itemtype ITEM_TYPE_PERMANENT                   = ConvertItemType(0)

    constant itemtype ITEM_TYPE_CHARGED                     = ConvertItemType(1)

    constant itemtype ITEM_TYPE_POWERUP                     = ConvertItemType(2)

    constant itemtype ITEM_TYPE_ARTIFACT                    = ConvertItemType(3)

    constant itemtype ITEM_TYPE_PURCHASABLE                 = ConvertItemType(4)

    constant itemtype ITEM_TYPE_CAMPAIGN                    = ConvertItemType(5)

    constant itemtype ITEM_TYPE_MISCELLANEOUS               = ConvertItemType(6)

    constant itemtype ITEM_TYPE_UNKNOWN                     = ConvertItemType(7)

    constant itemtype ITEM_TYPE_ANY                         = ConvertItemType(8)



    // Deprecated, should use ITEM_TYPE_POWERUP

    constant itemtype ITEM_TYPE_TOME                        = ConvertItemType(2)



//===================================================

// Animatable Camera Fields

//===================================================



    constant camerafield CAMERA_FIELD_TARGET_DISTANCE       = ConvertCameraField(0)

    constant camerafield CAMERA_FIELD_FARZ                  = ConvertCameraField(1)

    constant camerafield CAMERA_FIELD_ANGLE_OF_ATTACK       = ConvertCameraField(2)

    constant camerafield CAMERA_FIELD_FIELD_OF_VIEW         = ConvertCameraField(3)

    constant camerafield CAMERA_FIELD_ROLL                  = ConvertCameraField(4)

    constant camerafield CAMERA_FIELD_ROTATION              = ConvertCameraField(5)

    constant camerafield CAMERA_FIELD_ZOFFSET               = ConvertCameraField(6)



    constant blendmode   BLEND_MODE_NONE                    = ConvertBlendMode(0)

    constant blendmode   BLEND_MODE_DONT_CARE               = ConvertBlendMode(0)

    constant blendmode   BLEND_MODE_KEYALPHA                = ConvertBlendMode(1)

    constant blendmode   BLEND_MODE_BLEND                   = ConvertBlendMode(2)

    constant blendmode   BLEND_MODE_ADDITIVE                = ConvertBlendMode(3)

    constant blendmode   BLEND_MODE_MODULATE                = ConvertBlendMode(4)

    constant blendmode   BLEND_MODE_MODULATE_2X             = ConvertBlendMode(5)

    

    constant raritycontrol  RARITY_FREQUENT                 = ConvertRarityControl(0)

    constant raritycontrol  RARITY_RARE                     = ConvertRarityControl(1)



    constant texmapflags    TEXMAP_FLAG_NONE                = ConvertTexMapFlags(0)

    constant texmapflags    TEXMAP_FLAG_WRAP_U              = ConvertTexMapFlags(1)

    constant texmapflags    TEXMAP_FLAG_WRAP_V              = ConvertTexMapFlags(2)

    constant texmapflags    TEXMAP_FLAG_WRAP_UV             = ConvertTexMapFlags(3)



    constant fogstate       FOG_OF_WAR_MASKED               = ConvertFogState(1)

    constant fogstate       FOG_OF_WAR_FOGGED               = ConvertFogState(2)

    constant fogstate       FOG_OF_WAR_VISIBLE              = ConvertFogState(4)



//===================================================

// Camera Margin constants for use with GetCameraMargin

//===================================================



    constant integer        CAMERA_MARGIN_LEFT              = 0

    constant integer        CAMERA_MARGIN_RIGHT             = 1

    constant integer        CAMERA_MARGIN_TOP               = 2

    constant integer        CAMERA_MARGIN_BOTTOM            = 3



//===================================================

// Effect API constants

//===================================================



    constant effecttype     EFFECT_TYPE_EFFECT              = ConvertEffectType(0)

    constant effecttype     EFFECT_TYPE_TARGET              = ConvertEffectType(1)

    constant effecttype     EFFECT_TYPE_CASTER              = ConvertEffectType(2)

    constant effecttype     EFFECT_TYPE_SPECIAL             = ConvertEffectType(3)

    constant effecttype     EFFECT_TYPE_AREA_EFFECT         = ConvertEffectType(4)

    constant effecttype     EFFECT_TYPE_MISSILE             = ConvertEffectType(5)

    constant effecttype     EFFECT_TYPE_LIGHTNING           = ConvertEffectType(6)



    constant soundtype      SOUND_TYPE_EFFECT               = ConvertSoundType(0)

    constant soundtype      SOUND_TYPE_EFFECT_LOOPED        = ConvertSoundType(1)



endglobals

