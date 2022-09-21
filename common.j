//============================================================================
// Native types. All native functions take extended handle types when
// possible to help prevent passing bad values to native functions
//
/**
@patch 1.24b
*/
type agent			    extends     handle  // all reference counted objects
type event              extends     agent  // a reference to an event registration
type player             extends     agent  // a single player reference

/**
Types `unit`, `destructable`, `item` extend from widget.

All API functions that accept widget, will also take any of the children types.
However if doesn't work the other way around, then you need to explicitly cast the type by pushing it through a hashtable, "downcasting":
Put the widget object in a hashtable and retrieve it as unit/destructable/item - needed since 1.24b, source: [The Helper Wiki](https://web.archive.org/web/20100118203210/http://wiki.thehelper.net/wc3/jass/common.j/Widget_API).

*/
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

/**
Represents a game speed option at which the map can run. There are five
predefined settings, but only SLOWEST, SLOW, NORMAL can be set and used.
Setting FAST or FASTEST will automatically set it to NORMAL instead.

Warcraft 3 game speed setting:

| Menu Name      | Game constant      | Speed | 10 seconds is |
|----------------|--------------------|-------|---------------|
| High (default) | `MAP_SPEED_NORMAL` | 1.0x  | 10s           |
| Medium         | `MAP_SPEED_SLOW`   | 0.8x  | 12.5s         |
| Slow           | `MAP_SPEED_SLOWEST`| 0.6x  | 16.667s       |


@note
This setting is actually just a multiplier for a "base game speed",
which is normally set to 1.0x (100%).

You can achieve a higher "base game speed" via a .wgc file only for testing.
The in-game `gamespeed` is actually a multiplier on top of that. The results
are shown in the table below:

| Base game speed | Game speed option | Effective speed |
| --------------: | ----------------: | --------------: |
|              1x |              0.6x |           0.60x |
|              1x |              0.8x |           0.80x |
|              1x |              1.0x |           1.00x |
|              2x |              0.6x |           1.20x |
|              2x |              0.8x |           1.60x |
|              2x |              1.0x |           2.00x |
|              3x |              0.6x |           1.80x |
|              3x |              0.8x |           2.40x |
|              3x |              1.0x |           3.00x |

Further reading:

- [How to run maps with high gamespeed](https://www.hiveworkshop.com/threads/how-to-run-maps-with-high-gamespeed.37255/)
- [WGC file format specification](https://github.com/ChiefOfGxBxL/WC3MapSpecification/pull/4)

@bug (v1.27, v1.32.10 tested) Restarting the map from the F10 in-game menu
will reset the "base game speed" back to 1x.

*/
type gamespeed          extends     handle

type gamedifficulty     extends     handle
type gametype           extends     handle
type mapflag            extends     handle
type mapvisibility      extends     handle
type mapsetting         extends     handle
type mapdensity         extends     handle
type mapcontrol         extends     handle

/**
@patch 1.32
*/
type minimapicon        extends     handle

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

/**
Gamecaches are designed for transferring data between maps in a campaign, by
storing data on the hard disk. In multi-player games however, the data is never
stored on the hard disk, making online campaigns and saving/loading data between
games impossible.

To be able to read and write from a gamecache across maps you have to use a
consistent name between them, that is the first parameter for `InitGameCache`.
If you're developing a campaign it would be reasonable to use something like
`set my_gc = InitGameCache("my_campaign.w3v")`.

Once you've setup your `gamecache` it is time to fill it with data. There are
only five natives to store data inside a `gamecache`, that is

* `StoreInteger`
* `StoreReal`
* `StoreBoolean`
* `StoreString`
* `StoreUnit`

These should be enough to track stats like gold, lumber and all your important
`unit`s. Just storing values inside the `gamecache` is not enough to transfer
them between maps though; in fact those natives still work in multiplayer.
Persisting stored values to disk is achieved by calling `SaveGameCache(my_gc)`.

Now if you create your `gamecache` again via `InitGameCache("my_campaign.w3v")`
(in another map or another game) it should have all previously stored values
available and can be queried via `HaveStoredString`, `RestoreUnit`, `GetStoredInteger`, etc.
*/
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

/**
@patch 1.29
*/
type mousebuttontype    extends     handle

/**
@patch 1.30
*/
type animtype           extends     handle

/**
@patch 1.30
*/
type subanimtype        extends     handle
type image              extends     handle
type ubersplat          extends     handle

/**
@patch 1.24
*/
type hashtable          extends     agent

/**
@patch 1.31
*/
type framehandle        extends     handle

/**
@patch 1.31
*/
type originframetype    extends     handle

/**
@patch 1.31
*/
type framepointtype     extends     handle

/**
@patch 1.31
*/
type textaligntype      extends     handle

/**
@patch 1.31
*/
type frameeventtype     extends     handle

/**
@patch 1.31
*/
type oskeytype          extends     handle

/**
@patch 1.31
*/
type abilityintegerfield            extends handle

/**
@patch 1.31
*/
type abilityrealfield               extends handle

/**
@patch 1.31
*/
type abilitybooleanfield            extends handle

/**
@patch 1.31
*/
type abilitystringfield             extends handle

/**
@patch 1.31
*/
type abilityintegerlevelfield       extends handle

/**
@patch 1.31
*/
type abilityreallevelfield          extends handle

/**
@patch 1.31
*/
type abilitybooleanlevelfield       extends handle

/**
@patch 1.31
*/
type abilitystringlevelfield        extends handle

/**
@patch 1.31
*/
type abilityintegerlevelarrayfield  extends handle

/**
@patch 1.31
*/
type abilityreallevelarrayfield     extends handle

/**
@patch 1.31
*/
type abilitybooleanlevelarrayfield  extends handle

/**
@patch 1.31
*/
type abilitystringlevelarrayfield   extends handle

/**
@patch 1.31
*/
type unitintegerfield               extends handle

/**
@patch 1.31
*/
type unitrealfield                  extends handle

/**
@patch 1.31
*/
type unitbooleanfield               extends handle

/**
@patch 1.31
*/
type unitstringfield                extends handle

/**
@patch 1.31
*/
type unitweaponintegerfield         extends handle

/**
@patch 1.31
*/
type unitweaponrealfield            extends handle

/**
@patch 1.31
*/
type unitweaponbooleanfield         extends handle

/**
@patch 1.31
*/
type unitweaponstringfield          extends handle

/**
@patch 1.31
*/
type itemintegerfield               extends handle

/**
@patch 1.31
*/
type itemrealfield                  extends handle

/**
@patch 1.31
*/
type itembooleanfield               extends handle

/**
@patch 1.31
*/
type itemstringfield                extends handle

/**
@patch 1.31
*/
type movetype                       extends handle

/**
@patch 1.31
*/
type targetflag                     extends handle

/**
@patch 1.31
*/
type armortype                      extends handle

/**
@patch 1.31
*/
type heroattribute                  extends handle

/**
@patch 1.31
*/
type defensetype                    extends handle

/**
@patch 1.31
*/
type regentype                      extends handle

/**
@patch 1.31
*/
type unitcategory                   extends handle

/**
@patch 1.31
*/
type pathingflag                    extends handle

/**
@patch 1.32
*/
type commandbuttoneffect	    extends handle

/**
Returns the race that corresponds to the given integer.
@param i The integer representation of the race
@pure
*/
constant native ConvertRace                 takes integer i returns race



/**
Returns the alliancetype that corresponds to the given integer.
@param i The integer representation of the alliancetype
@pure
*/
constant native ConvertAllianceType         takes integer i returns alliancetype



/**
Returns the racepreference that corresponds to the given integer.
@param i The integer representation of the racepreference
@pure
*/
constant native ConvertRacePref             takes integer i returns racepreference



/**
Returns the igamestate that corresponds to the given integer.
@param i The integer representation of the igamestate
@pure
*/
constant native ConvertIGameState           takes integer i returns igamestate



/**
Returns the fgamestate that corresponds to the given integer.
@param i The integer representation of the fgamestate
@pure
*/
constant native ConvertFGameState           takes integer i returns fgamestate



/**
Returns the playerstate that corresponds to the given integer.
@param i The integer representation of the playerstate
@pure
*/
constant native ConvertPlayerState          takes integer i returns playerstate



/**
Returns the playerscore that corresponds to the given integer.
@param i The integer representation of the playerscore
@pure
*/
constant native ConvertPlayerScore          takes integer i returns playerscore



/**
Returns the playergameresult that corresponds to the given integer.
@param i The integer representation of the playergameresult
@pure
*/
constant native ConvertPlayerGameResult     takes integer i returns playergameresult



/**
Returns unitstate, first index is 0. 

It is used to define the constants representing unit state. Accepts any integer, the unitstate reference is always the same for a given integer.

**Example:** `constant unitstate UNIT_STATE_MAX_MANA = ConvertUnitState(3)`

@param i The integer representation of the unitstate

@note See: `GetUnitState`, `SetUnitState`
@pure
*/
constant native ConvertUnitState            takes integer i returns unitstate



/**
Returns the aidifficulty that corresponds to the given integer.
@param i The integer representation of the aidifficulty
@pure
*/
constant native ConvertAIDifficulty         takes integer i returns aidifficulty



/**
Returns the gameevent that corresponds to the given integer.
@param i The integer representation of the gameevent
@pure
*/
constant native ConvertGameEvent            takes integer i returns gameevent



/**
Returns the playerevent that corresponds to the given integer.
@param i The integer representation of the playerevent
@pure
*/
constant native ConvertPlayerEvent          takes integer i returns playerevent



/**
Returns the playerunitevent that corresponds to the given integer.
@param i The integer representation of the playerunitevent
@pure
*/
constant native ConvertPlayerUnitEvent      takes integer i returns playerunitevent



/**
Returns the widgetevent that corresponds to the given integer.
@param i The integer representation of the widgetevent
@pure
*/
constant native ConvertWidgetEvent          takes integer i returns widgetevent



/**
Returns the dialogevent that corresponds to the given integer.
@param i The integer representation of the dialogevent
@pure
*/
constant native ConvertDialogEvent          takes integer i returns dialogevent



/**
Returns the unitevent that corresponds to the given integer.
@param i The integer representation of the unitevent
@pure
*/
constant native ConvertUnitEvent            takes integer i returns unitevent



/**
Returns the limitop that corresponds to the given integer.
@param i The integer representation of the limitop
@pure
*/
constant native ConvertLimitOp              takes integer i returns limitop



/**
Returns the unittype that corresponds to the given integer.
@param i The integer representation of the unittype
@pure
*/
constant native ConvertUnitType             takes integer i returns unittype



/**
Returns the gamespeed that corresponds to the given integer.

It is used to define the constants representing gamespeed. First index is 0. Accepts any integer, the reference is always the same for a given integer.

@param i The integer representation of the gamespeed
@pure
*/
constant native ConvertGameSpeed            takes integer i returns gamespeed



/**
Returns the placement that corresponds to the given integer.
@param i The integer representation of the placement
@pure
*/
constant native ConvertPlacement            takes integer i returns placement



/**
Returns the startlocprio that corresponds to the given integer.
@param i The integer representation of the startlocprio
@pure
*/
constant native ConvertStartLocPrio         takes integer i returns startlocprio



/**
Returns the gamedifficulty that corresponds to the given integer.
@param i The integer representation of the gamedifficulty
@pure
*/
constant native ConvertGameDifficulty       takes integer i returns gamedifficulty



/**
Returns the gametype that corresponds to the given integer.
@param i The integer representation of the gametype
@pure
*/
constant native ConvertGameType             takes integer i returns gametype



/**
Returns the mapflag that corresponds to the given integer.
@param i The integer representation of the mapflag
@pure
*/
constant native ConvertMapFlag              takes integer i returns mapflag



/**
Returns the mapvisibility that corresponds to the given integer.
@param i The integer representation of the mapvisibility
@pure
*/
constant native ConvertMapVisibility        takes integer i returns mapvisibility



/**
Returns the mapsetting that corresponds to the given integer.
@param i The integer representation of the mapsetting
@pure
*/
constant native ConvertMapSetting           takes integer i returns mapsetting



/**
Returns the mapdensity that corresponds to the given integer.
@param i The integer representation of the mapdensity
@pure
*/
constant native ConvertMapDensity           takes integer i returns mapdensity



/**
Returns the mapcontrol that corresponds to the given integer.
@param i The integer representation of the mapcontrol
@pure
*/
constant native ConvertMapControl           takes integer i returns mapcontrol



/**
Returns the playercolor that corresponds to the given integer.
@param i The integer representation of the playercolor
@pure
*/
constant native ConvertPlayerColor          takes integer i returns playercolor



/**
Returns the playerslotstate that corresponds to the given integer.
@param i The integer representation of the playerslotstate
@pure
*/
constant native ConvertPlayerSlotState      takes integer i returns playerslotstate

/**
Returns the volumegroup that corresponds to the given integer.
@param i The integer representation of the volumegroup
@pure
*/
constant native ConvertVolumeGroup          takes integer i returns volumegroup


/**
Returns the camerafield that corresponds to the given integer.
@param i The integer representation of the camerafield
@pure
*/
constant native ConvertCameraField          takes integer i returns camerafield



/**
Returns the blendmode that corresponds to the given integer.
@param i The integer representation of the blendmode
@pure
*/
constant native ConvertBlendMode            takes integer i returns blendmode



/**
Returns the raritycontrol that corresponds to the given integer.
@param i The integer representation of the raritycontrol
@pure
*/
constant native ConvertRarityControl        takes integer i returns raritycontrol



/**
Returns the texmapflags that corresponds to the given integer.
@param i The integer representation of the texmapflags
@pure
*/
constant native ConvertTexMapFlags          takes integer i returns texmapflags





/**
Returns the effecttype that corresponds to the given integer.
@param i The integer representation of the effecttype
@pure
*/
constant native ConvertEffectType           takes integer i returns effecttype



/**
Returns the version that corresponds to the given integer.
@param i The integer representation of the version
@pure
*/
constant native ConvertVersion              takes integer i returns version



/**
Returns the itemtype that corresponds to the given integer.
@param i The integer representation of the itemtype
@pure
*/
constant native ConvertItemType             takes integer i returns itemtype




/**
Returns the damagetype that corresponds to the given integer.
@param i The integer representation of the damagetype
@pure
*/
constant native ConvertDamageType           takes integer i returns damagetype


/**
Returns the weapontype that corresponds to the given integer.
@param i The integer representation of the weapontype
@pure
*/
constant native ConvertWeaponType           takes integer i returns weapontype



/**
Returns the soundtype that corresponds to the given integer.
@param i The integer representation of the soundtype
@pure
*/
constant native ConvertSoundType            takes integer i returns soundtype


/**
Returns the pathingtype that corresponds to the given integer.
@param i The integer representation of the pathingtype
@pure
*/
constant native ConvertPathingType          takes integer i returns pathingtype


/**
Returns the mousebuttontype that corresponds to the given integer.
@param i The integer representation of the mousebuttontype
@pure
@patch 1.29
*/
constant native ConvertMouseButtonType      takes integer i returns mousebuttontype

/**
Returns the animtype that corresponds to the given integer.
@param i The integer representation of the animtype
@pure
@patch 1.30
*/
constant native ConvertAnimType             takes integer i returns animtype

/**
Returns the subanimtype that corresponds to the given integer.
@param i The integer representation of the subanimtype
@pure
@patch 1.30
*/
constant native ConvertSubAnimType          takes integer i returns subanimtype


/**
@note Can be used for extended typecasting.
<http://www.hiveworkshop.com/forums/j-280/t-232039/>
@pure
*/
constant native ConvertFogState             takes integer i returns fogstate


/**
@note Blizzard only defined attack-types 0 to 6 but there is a hidden one:
`ConvertAttackType(7)`.
<http://www.hiveworkshop.com/forums/t-269/h-227993/>
@pure
*/
constant native ConvertAttackType           takes integer i returns attacktype


/**
@pure
@patch 1.31
*/
constant native ConvertOriginFrameType      takes integer i returns originframetype
/**
@pure
@patch 1.31
*/
constant native ConvertFramePointType       takes integer i returns framepointtype
/**
@pure
@patch 1.31
*/
constant native ConvertTextAlignType        takes integer i returns textaligntype
/**
@pure
@patch 1.31
*/
constant native ConvertFrameEventType       takes integer i returns frameeventtype
/**
@pure
@patch 1.31
*/
constant native ConvertOsKeyType            takes integer i returns oskeytype
/**
@pure
@patch 1.31
*/
constant native ConvertAbilityIntegerField              takes integer i returns abilityintegerfield
/**
@pure
@patch 1.31
*/
constant native ConvertAbilityRealField                 takes integer i returns abilityrealfield
/**
@pure
@patch 1.31
*/
constant native ConvertAbilityBooleanField              takes integer i returns abilitybooleanfield
/**
@pure
@patch 1.31
*/
constant native ConvertAbilityStringField               takes integer i returns abilitystringfield
/**
@pure
@patch 1.31
*/
constant native ConvertAbilityIntegerLevelField         takes integer i returns abilityintegerlevelfield
/**
@pure
@patch 1.31
*/
constant native ConvertAbilityRealLevelField            takes integer i returns abilityreallevelfield
/**
@pure
@patch 1.31
*/
constant native ConvertAbilityBooleanLevelField         takes integer i returns abilitybooleanlevelfield
/**
@pure
@patch 1.31
*/
constant native ConvertAbilityStringLevelField          takes integer i returns abilitystringlevelfield
/**
@pure
@patch 1.31
*/
constant native ConvertAbilityIntegerLevelArrayField    takes integer i returns abilityintegerlevelarrayfield
/**
@pure
@patch 1.31
*/
constant native ConvertAbilityRealLevelArrayField       takes integer i returns abilityreallevelarrayfield
/**
@pure
@patch 1.31
*/
constant native ConvertAbilityBooleanLevelArrayField    takes integer i returns abilitybooleanlevelarrayfield
/**
@pure
@patch 1.31
*/
constant native ConvertAbilityStringLevelArrayField     takes integer i returns abilitystringlevelarrayfield
/**
@pure
@patch 1.31
*/
constant native ConvertUnitIntegerField                 takes integer i returns unitintegerfield
/**
@pure
@patch 1.31
*/
constant native ConvertUnitRealField                    takes integer i returns unitrealfield
/**
@pure
@patch 1.31
*/
constant native ConvertUnitBooleanField                 takes integer i returns unitbooleanfield
/**
@pure
@patch 1.31
*/
constant native ConvertUnitStringField                  takes integer i returns unitstringfield
/**
@pure
@patch 1.31
*/
constant native ConvertUnitWeaponIntegerField           takes integer i returns unitweaponintegerfield
/**
@pure
@patch 1.31
*/
constant native ConvertUnitWeaponRealField              takes integer i returns unitweaponrealfield
/**
@pure
@patch 1.31
*/
constant native ConvertUnitWeaponBooleanField           takes integer i returns unitweaponbooleanfield
/**
@pure
@patch 1.31
*/
constant native ConvertUnitWeaponStringField            takes integer i returns unitweaponstringfield
/**
@pure
@patch 1.31
*/
constant native ConvertItemIntegerField                 takes integer i returns itemintegerfield
/**
@pure
@patch 1.31
*/
constant native ConvertItemRealField                    takes integer i returns itemrealfield
/**
@pure
@patch 1.31
*/
constant native ConvertItemBooleanField                 takes integer i returns itembooleanfield
/**
@pure
@patch 1.31
*/
constant native ConvertItemStringField                  takes integer i returns itemstringfield
/**
@pure
@patch 1.31
*/
constant native ConvertMoveType                         takes integer i returns movetype
/**
@pure
@patch 1.31
*/
constant native ConvertTargetFlag                       takes integer i returns targetflag
/**
@pure
@patch 1.31
*/
constant native ConvertArmorType                        takes integer i returns armortype
/**
@pure
@patch 1.31
*/
constant native ConvertHeroAttribute                    takes integer i returns heroattribute
/**
@pure
@patch 1.31
*/
constant native ConvertDefenseType                      takes integer i returns defensetype
/**
@pure
@patch 1.31
*/
constant native ConvertRegenType                        takes integer i returns regentype
/**
@pure
@patch 1.31
*/
constant native ConvertUnitCategory                     takes integer i returns unitcategory
/**
@pure
@patch 1.31
*/
constant native ConvertPathingFlag                      takes integer i returns pathingflag

/**
Returns an internal ID for the unit order string.

**Example (Lua):**

	OrderId("humanbuild") == 851995 -- this order opens the human build menu

@note See: `OrderId2String`

@bug Do not use this in a global initialisation (map init) as it returns 0 there.
@bug
Orders: `humainbuild` / `orcbuild` / `nightelfbuild` / `undeadbuild` are [totally broken](https://www.hiveworkshop.com/threads/build-order-causing-all-player-builders-to-open-build-menu.339196/post-3529953), don't issue them.

@pure
*/
constant native OrderId                     takes string  orderIdString     returns integer

/**
Returns the human-readable unit order string.

**Example (Lua):**

	OrderId2String(851995) --> returns "humanbuild" (opens human build menu)

@note See: `OrderId`

@pure
@bug Always returns null after the game is loaded/if the game is a replay.
@bug Do not use this in a global initialisation (map init) as it returns null there.
*/
constant native OrderId2String              takes integer orderId           returns string
constant native UnitId                      takes string  unitIdString      returns integer

/**

**Example (Lua):** `UnitId2String( FourCC("hfoo") ) --> "footman" (internal name, not localized)`

@note See `GetObjectName` if you need to retrieve a unit's localized pretty name by the type ID.

@bug Always returns null after the game is loaded/if the game is a replay.
@bug Do not use this in a global initialisation (on map init) as it returns null there.
*/
constant native UnitId2String               takes integer unitId            returns string


/**
@bug Not working correctly
@pure
*/
constant native AbilityId                   takes string  abilityIdString   returns integer

/**
@bug Not working correctly
@pure
*/
constant native AbilityId2String            takes integer abilityId         returns string



/**
Returns localized value for field "name" for the given object type ID (unit, item, ability).
In WorldEdit this is "Text - Name".

**Example (Lua):** `GetObjectName( FourCC("hfoo") ) --> "Footman"`

@note See: `UnitId2String`

@pure
@async
@bug Do not use this in a global initialisation (on map init) as it crashes the game there.
*/
constant native GetObjectName               takes integer objectId          returns string

/**
Returns the maximum number of playable player slots regardless of map options.

* Classic: 12
* Reforged: 24

@note This is only affected by WorldEditor version specified in the map's war3map.w3i file. [Further reading](https://www.hiveworkshop.com/threads/success-hybrid-12-24-player-map-backwards-compatible-1-24-1-28-5-1-31.339722/)

@note See: `bj_MAX_PLAYERS`, `GetBJMaxPlayerSlots`

@patch 1.29
*/
constant native GetBJMaxPlayers             takes nothing returns integer

/**
Returns the zero-based ID of neutral victim player.

* Classic = (13?)
* Reforged: ID = 25

@note See: `bj_PLAYER_NEUTRAL_VICTIM`, `GetPlayerNeutralAggressive`, `GetBJPlayerNeutralExtra`, `GetPlayerNeutralPassive`

@patch 1.29
*/
constant native GetBJPlayerNeutralVictim    takes nothing returns integer

/**
Returns the zero-based ID of neutral extra player.

* Classic = (14?)
* Reforged: ID = 26

@note See: `bj_PLAYER_NEUTRAL_EXTRA`, `GetPlayerNeutralAggressive`, `GetPlayerNeutralPassive`, `GetBJPlayerNeutralVictim`

@patch 1.29
*/
constant native GetBJPlayerNeutralExtra     takes nothing returns integer

/**
Returns the maximum number of internal player slots regardless of map options.

* Classic: (16?)
* Reforged: 28

@note This is only affected by WorldEditor version specified in the map's war3map.w3i file. [Further reading](https://www.hiveworkshop.com/threads/success-hybrid-12-24-player-map-backwards-compatible-1-24-1-28-5-1-31.339722/)

@note See: `bj_MAX_PLAYER_SLOTS`, `GetBJMaxPlayers`

@patch 1.29
*/
constant native GetBJMaxPlayerSlots         takes nothing returns integer

/**
Returns the zero-based ID of neutral passive player.

* Classic = ?
* Reforged: ID = 27

See: `PLAYER_NEUTRAL_PASSIVE`, `GetPlayerNeutralAggressive`, `GetBJPlayerNeutralExtra`, `GetBJPlayerNeutralVictim`

@patch 1.29
*/
constant native GetPlayerNeutralPassive     takes nothing returns integer

/**
Returns the zero-based ID of neutral aggressive player.

* Classic = ?
* Reforged: ID = 24

See: `PLAYER_NEUTRAL_AGGRESSIVE`, `GetBJPlayerNeutralExtra`, `GetPlayerNeutralPassive`, `GetBJPlayerNeutralVictim`

@patch 1.29
*/
constant native GetPlayerNeutralAggressive  takes nothing returns integer


globals

//===================================================
// Game Constants    
//===================================================

    // pfff
    constant boolean            FALSE                           = false
    constant boolean            TRUE                            = true
    constant integer            JASS_MAX_ARRAY_SIZE             = 32768

/**
Stores the zero-based ID of neutral passive player.

@note See: `GetPlayerNeutralPassive`, `GetPlayerNeutralAggressive`, `GetBJPlayerNeutralExtra`, `GetBJPlayerNeutralVictim`
*/
    constant integer            PLAYER_NEUTRAL_PASSIVE          = GetPlayerNeutralPassive()

/**
Stores the zero-based ID of neutral aggressive player.

@note See: `GetPlayerNeutralAggressive`
*/
    constant integer            PLAYER_NEUTRAL_AGGRESSIVE       = GetPlayerNeutralAggressive()

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
    constant playercolor        PLAYER_COLOR_MAROON             = ConvertPlayerColor(12)
    constant playercolor        PLAYER_COLOR_NAVY               = ConvertPlayerColor(13)
    constant playercolor        PLAYER_COLOR_TURQUOISE          = ConvertPlayerColor(14)
    constant playercolor        PLAYER_COLOR_VIOLET             = ConvertPlayerColor(15)
    constant playercolor        PLAYER_COLOR_WHEAT              = ConvertPlayerColor(16)
    constant playercolor        PLAYER_COLOR_PEACH              = ConvertPlayerColor(17)
    constant playercolor        PLAYER_COLOR_MINT               = ConvertPlayerColor(18)
    constant playercolor        PLAYER_COLOR_LAVENDER           = ConvertPlayerColor(19)
    constant playercolor        PLAYER_COLOR_COAL               = ConvertPlayerColor(20)
    constant playercolor        PLAYER_COLOR_SNOW               = ConvertPlayerColor(21)
    constant playercolor        PLAYER_COLOR_EMERALD            = ConvertPlayerColor(22)
    constant playercolor        PLAYER_COLOR_PEANUT             = ConvertPlayerColor(23)

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

    constant mousebuttontype    MOUSE_BUTTON_TYPE_LEFT          = ConvertMouseButtonType(1)
    constant mousebuttontype    MOUSE_BUTTON_TYPE_MIDDLE        = ConvertMouseButtonType(2)
    constant mousebuttontype    MOUSE_BUTTON_TYPE_RIGHT         = ConvertMouseButtonType(3)

    constant animtype           ANIM_TYPE_BIRTH                 = ConvertAnimType(0)
    constant animtype           ANIM_TYPE_DEATH                 = ConvertAnimType(1)
    constant animtype           ANIM_TYPE_DECAY                 = ConvertAnimType(2)
    constant animtype           ANIM_TYPE_DISSIPATE             = ConvertAnimType(3)
    constant animtype           ANIM_TYPE_STAND                 = ConvertAnimType(4)
    constant animtype           ANIM_TYPE_WALK                  = ConvertAnimType(5)
    constant animtype           ANIM_TYPE_ATTACK                = ConvertAnimType(6)
    constant animtype           ANIM_TYPE_MORPH                 = ConvertAnimType(7)
    constant animtype           ANIM_TYPE_SLEEP                 = ConvertAnimType(8)
    constant animtype           ANIM_TYPE_SPELL                 = ConvertAnimType(9)
    constant animtype           ANIM_TYPE_PORTRAIT              = ConvertAnimType(10)

    constant subanimtype        SUBANIM_TYPE_ROOTED             = ConvertSubAnimType(11)
    constant subanimtype        SUBANIM_TYPE_ALTERNATE_EX       = ConvertSubAnimType(12)
    constant subanimtype        SUBANIM_TYPE_LOOPING            = ConvertSubAnimType(13)
    constant subanimtype        SUBANIM_TYPE_SLAM               = ConvertSubAnimType(14)
    constant subanimtype        SUBANIM_TYPE_THROW              = ConvertSubAnimType(15)
    constant subanimtype        SUBANIM_TYPE_SPIKED             = ConvertSubAnimType(16)
    constant subanimtype        SUBANIM_TYPE_FAST               = ConvertSubAnimType(17)
    constant subanimtype        SUBANIM_TYPE_SPIN               = ConvertSubAnimType(18)
    constant subanimtype        SUBANIM_TYPE_READY              = ConvertSubAnimType(19)
    constant subanimtype        SUBANIM_TYPE_CHANNEL            = ConvertSubAnimType(20)
    constant subanimtype        SUBANIM_TYPE_DEFEND             = ConvertSubAnimType(21)
    constant subanimtype        SUBANIM_TYPE_VICTORY            = ConvertSubAnimType(22)
    constant subanimtype        SUBANIM_TYPE_TURN               = ConvertSubAnimType(23)
    constant subanimtype        SUBANIM_TYPE_LEFT               = ConvertSubAnimType(24)
    constant subanimtype        SUBANIM_TYPE_RIGHT              = ConvertSubAnimType(25)
    constant subanimtype        SUBANIM_TYPE_FIRE               = ConvertSubAnimType(26)
    constant subanimtype        SUBANIM_TYPE_FLESH              = ConvertSubAnimType(27)
    constant subanimtype        SUBANIM_TYPE_HIT                = ConvertSubAnimType(28)
    constant subanimtype        SUBANIM_TYPE_WOUNDED            = ConvertSubAnimType(29)
    constant subanimtype        SUBANIM_TYPE_LIGHT              = ConvertSubAnimType(30)
    constant subanimtype        SUBANIM_TYPE_MODERATE           = ConvertSubAnimType(31)
    constant subanimtype        SUBANIM_TYPE_SEVERE             = ConvertSubAnimType(32)
    constant subanimtype        SUBANIM_TYPE_CRITICAL           = ConvertSubAnimType(33)
    constant subanimtype        SUBANIM_TYPE_COMPLETE           = ConvertSubAnimType(34)
    constant subanimtype        SUBANIM_TYPE_GOLD               = ConvertSubAnimType(35)
    constant subanimtype        SUBANIM_TYPE_LUMBER             = ConvertSubAnimType(36)
    constant subanimtype        SUBANIM_TYPE_WORK               = ConvertSubAnimType(37)
    constant subanimtype        SUBANIM_TYPE_TALK               = ConvertSubAnimType(38)
    constant subanimtype        SUBANIM_TYPE_FIRST              = ConvertSubAnimType(39)
    constant subanimtype        SUBANIM_TYPE_SECOND             = ConvertSubAnimType(40)
    constant subanimtype        SUBANIM_TYPE_THIRD              = ConvertSubAnimType(41)
    constant subanimtype        SUBANIM_TYPE_FOURTH             = ConvertSubAnimType(42)
    constant subanimtype        SUBANIM_TYPE_FIFTH              = ConvertSubAnimType(43)
    constant subanimtype        SUBANIM_TYPE_ONE                = ConvertSubAnimType(44)
    constant subanimtype        SUBANIM_TYPE_TWO                = ConvertSubAnimType(45)
    constant subanimtype        SUBANIM_TYPE_THREE              = ConvertSubAnimType(46)
    constant subanimtype        SUBANIM_TYPE_FOUR               = ConvertSubAnimType(47)
    constant subanimtype        SUBANIM_TYPE_FIVE               = ConvertSubAnimType(48)
    constant subanimtype        SUBANIM_TYPE_SMALL              = ConvertSubAnimType(49)
    constant subanimtype        SUBANIM_TYPE_MEDIUM             = ConvertSubAnimType(50)
    constant subanimtype        SUBANIM_TYPE_LARGE              = ConvertSubAnimType(51)
    constant subanimtype        SUBANIM_TYPE_UPGRADE            = ConvertSubAnimType(52)
    constant subanimtype        SUBANIM_TYPE_DRAIN              = ConvertSubAnimType(53)
    constant subanimtype        SUBANIM_TYPE_FILL               = ConvertSubAnimType(54)
    constant subanimtype        SUBANIM_TYPE_CHAINLIGHTNING     = ConvertSubAnimType(55)
    constant subanimtype        SUBANIM_TYPE_EATTREE            = ConvertSubAnimType(56)
    constant subanimtype        SUBANIM_TYPE_PUKE               = ConvertSubAnimType(57)
    constant subanimtype        SUBANIM_TYPE_FLAIL              = ConvertSubAnimType(58)
    constant subanimtype        SUBANIM_TYPE_OFF                = ConvertSubAnimType(59)
    constant subanimtype        SUBANIM_TYPE_SWIM               = ConvertSubAnimType(60)
    constant subanimtype        SUBANIM_TYPE_ENTANGLE           = ConvertSubAnimType(61)
    constant subanimtype        SUBANIM_TYPE_BERSERK            = ConvertSubAnimType(62)

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

/**
@note See `gamespeed` for explanation, values and mechanics.
*/
    constant gamespeed          MAP_SPEED_SLOWEST                   = ConvertGameSpeed(0)
/**
@note See `gamespeed` for explanation, values and mechanics.
*/
    constant gamespeed          MAP_SPEED_SLOW                      = ConvertGameSpeed(1)
/**
@note See `gamespeed` for explanation, values and mechanics.
*/
    constant gamespeed          MAP_SPEED_NORMAL                    = ConvertGameSpeed(2)
/**
@note See `gamespeed` for explanation, values and mechanics.
@bug Currently unused, resets to `MAP_SPEED_NORMAL`.
*/
    constant gamespeed          MAP_SPEED_FAST                      = ConvertGameSpeed(3)
/**
@note See `gamespeed` for explanation, values and mechanics.
@bug Currently unused, resets to `MAP_SPEED_NORMAL`.
*/
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
//Cinematic Sound Constants

/**
@patch 1.33
*/
    constant volumegroup        SOUND_VOLUMEGROUP_CINEMATIC_GENERAL         = ConvertVolumeGroup(8)
/**
@patch 1.33
*/
    constant volumegroup        SOUND_VOLUMEGROUP_CINEMATIC_AMBIENT         = ConvertVolumeGroup(9)
/**
@patch 1.33
*/
    constant volumegroup        SOUND_VOLUMEGROUP_CINEMATIC_MUSIC           = ConvertVolumeGroup(10)
/**
@patch 1.33
*/
    constant volumegroup        SOUND_VOLUMEGROUP_CINEMATIC_DIALOGUE        = ConvertVolumeGroup(11)
/**
@patch 1.33
*/
    constant volumegroup        SOUND_VOLUMEGROUP_CINEMATIC_SOUND_EFFECTS_1 = ConvertVolumeGroup(12)
/**
@patch 1.33
*/
    constant volumegroup        SOUND_VOLUMEGROUP_CINEMATIC_SOUND_EFFECTS_2 = ConvertVolumeGroup(13)
/**
@patch 1.33
*/
    constant volumegroup        SOUND_VOLUMEGROUP_CINEMATIC_SOUND_EFFECTS_3 = ConvertVolumeGroup(14)


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
/**
This event is fired when a build menu is opened (e.g. by human peasant).

**Example (Lua)**:

    trg_gameev = CreateTrigger()
    -- just print the object representing EventId
    TriggerAddAction(trg_gameev, function() print(GetTriggerEventId()) end)
    -- register for this event
    TriggerRegisterGameEvent(trg_gameev, EVENT_GAME_BUILD_SUBMENU)

*/
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
    constant playerunitevent EVENT_PLAYER_UNIT_DAMAGED                  = ConvertPlayerUnitEvent(308)
    constant playerunitevent EVENT_PLAYER_UNIT_DAMAGING                 = ConvertPlayerUnitEvent(315)
    
    //===================================================
    // For use with TriggerRegisterUnitEvent
    //===================================================

    constant unitevent EVENT_UNIT_DAMAGED                               = ConvertUnitEvent(52)
    constant unitevent EVENT_UNIT_DAMAGING                              = ConvertUnitEvent(314)
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

    constant gameevent          EVENT_GAME_LOADED                       = ConvertGameEvent(256)
    constant gameevent          EVENT_GAME_TOURNAMENT_FINISH_SOON       = ConvertGameEvent(257)
    constant gameevent          EVENT_GAME_TOURNAMENT_FINISH_NOW        = ConvertGameEvent(258)
    constant gameevent          EVENT_GAME_SAVE                         = ConvertGameEvent(259)
    constant gameevent          EVENT_GAME_CUSTOM_UI_FRAME              = ConvertGameEvent(310)

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
    constant playerevent        EVENT_PLAYER_MOUSE_DOWN                 = ConvertPlayerEvent(305)
    constant playerevent        EVENT_PLAYER_MOUSE_UP                   = ConvertPlayerEvent(306)
    constant playerevent        EVENT_PLAYER_MOUSE_MOVE                 = ConvertPlayerEvent(307)
    constant playerevent        EVENT_PLAYER_SYNC_DATA                  = ConvertPlayerEvent(309)
    constant playerevent        EVENT_PLAYER_KEY                        = ConvertPlayerEvent(311)
    constant playerevent        EVENT_PLAYER_KEY_DOWN                   = ConvertPlayerEvent(312)
    constant playerevent        EVENT_PLAYER_KEY_UP                     = ConvertPlayerEvent(313)

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
    constant playerunitevent    EVENT_PLAYER_UNIT_STACK_ITEM            = ConvertPlayerUnitEvent(319)


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
    constant unitevent          EVENT_UNIT_STACK_ITEM                   = ConvertUnitEvent(318)

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
    constant camerafield CAMERA_FIELD_NEARZ                 = ConvertCameraField(7)
    constant camerafield CAMERA_FIELD_LOCAL_PITCH           = ConvertCameraField(8)
    constant camerafield CAMERA_FIELD_LOCAL_YAW             = ConvertCameraField(9)
    constant camerafield CAMERA_FIELD_LOCAL_ROLL            = ConvertCameraField(10)

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

//===================================================
// Custom UI API constants
//===================================================

    constant originframetype        ORIGIN_FRAME_GAME_UI                 = ConvertOriginFrameType(0)
    constant originframetype        ORIGIN_FRAME_COMMAND_BUTTON          = ConvertOriginFrameType(1)
    constant originframetype        ORIGIN_FRAME_HERO_BAR                = ConvertOriginFrameType(2)
    constant originframetype        ORIGIN_FRAME_HERO_BUTTON             = ConvertOriginFrameType(3)
    constant originframetype        ORIGIN_FRAME_HERO_HP_BAR             = ConvertOriginFrameType(4)
    constant originframetype        ORIGIN_FRAME_HERO_MANA_BAR           = ConvertOriginFrameType(5)
    constant originframetype        ORIGIN_FRAME_HERO_BUTTON_INDICATOR   = ConvertOriginFrameType(6)
    constant originframetype        ORIGIN_FRAME_ITEM_BUTTON             = ConvertOriginFrameType(7)
    constant originframetype        ORIGIN_FRAME_MINIMAP                 = ConvertOriginFrameType(8)
    constant originframetype        ORIGIN_FRAME_MINIMAP_BUTTON          = ConvertOriginFrameType(9)
    constant originframetype        ORIGIN_FRAME_SYSTEM_BUTTON           = ConvertOriginFrameType(10)
    constant originframetype        ORIGIN_FRAME_TOOLTIP                 = ConvertOriginFrameType(11)
    constant originframetype        ORIGIN_FRAME_UBERTOOLTIP             = ConvertOriginFrameType(12)
    constant originframetype        ORIGIN_FRAME_CHAT_MSG                = ConvertOriginFrameType(13)
    constant originframetype        ORIGIN_FRAME_UNIT_MSG                = ConvertOriginFrameType(14)
    constant originframetype        ORIGIN_FRAME_TOP_MSG                 = ConvertOriginFrameType(15)
    constant originframetype        ORIGIN_FRAME_PORTRAIT                = ConvertOriginFrameType(16)
    constant originframetype        ORIGIN_FRAME_WORLD_FRAME             = ConvertOriginFrameType(17)
    constant originframetype        ORIGIN_FRAME_SIMPLE_UI_PARENT           = ConvertOriginFrameType(18)
    constant originframetype        ORIGIN_FRAME_PORTRAIT_HP_TEXT           = ConvertOriginFrameType(19)
    constant originframetype        ORIGIN_FRAME_PORTRAIT_MANA_TEXT         = ConvertOriginFrameType(20)
    constant originframetype        ORIGIN_FRAME_UNIT_PANEL_BUFF_BAR        = ConvertOriginFrameType(21)
    constant originframetype        ORIGIN_FRAME_UNIT_PANEL_BUFF_BAR_LABEL  = ConvertOriginFrameType(22)

    constant framepointtype         FRAMEPOINT_TOPLEFT                   = ConvertFramePointType(0)
    constant framepointtype         FRAMEPOINT_TOP                       = ConvertFramePointType(1)
    constant framepointtype         FRAMEPOINT_TOPRIGHT                  = ConvertFramePointType(2)
    constant framepointtype         FRAMEPOINT_LEFT                      = ConvertFramePointType(3)
    constant framepointtype         FRAMEPOINT_CENTER                    = ConvertFramePointType(4)
    constant framepointtype         FRAMEPOINT_RIGHT                     = ConvertFramePointType(5)
    constant framepointtype         FRAMEPOINT_BOTTOMLEFT                = ConvertFramePointType(6)
    constant framepointtype         FRAMEPOINT_BOTTOM                    = ConvertFramePointType(7)
    constant framepointtype         FRAMEPOINT_BOTTOMRIGHT               = ConvertFramePointType(8)

    constant textaligntype          TEXT_JUSTIFY_TOP                     = ConvertTextAlignType(0)
    constant textaligntype          TEXT_JUSTIFY_MIDDLE                  = ConvertTextAlignType(1)
    constant textaligntype          TEXT_JUSTIFY_BOTTOM                  = ConvertTextAlignType(2)
    constant textaligntype          TEXT_JUSTIFY_LEFT                    = ConvertTextAlignType(3)
    constant textaligntype          TEXT_JUSTIFY_CENTER                  = ConvertTextAlignType(4)
    constant textaligntype          TEXT_JUSTIFY_RIGHT                   = ConvertTextAlignType(5)

    constant frameeventtype         FRAMEEVENT_CONTROL_CLICK             = ConvertFrameEventType(1)
    constant frameeventtype         FRAMEEVENT_MOUSE_ENTER               = ConvertFrameEventType(2)
    constant frameeventtype         FRAMEEVENT_MOUSE_LEAVE               = ConvertFrameEventType(3)
    constant frameeventtype         FRAMEEVENT_MOUSE_UP                  = ConvertFrameEventType(4)
    constant frameeventtype         FRAMEEVENT_MOUSE_DOWN                = ConvertFrameEventType(5)
    constant frameeventtype         FRAMEEVENT_MOUSE_WHEEL               = ConvertFrameEventType(6)
    constant frameeventtype         FRAMEEVENT_CHECKBOX_CHECKED          = ConvertFrameEventType(7)
    constant frameeventtype         FRAMEEVENT_CHECKBOX_UNCHECKED        = ConvertFrameEventType(8)
    constant frameeventtype         FRAMEEVENT_EDITBOX_TEXT_CHANGED      = ConvertFrameEventType(9)
    constant frameeventtype         FRAMEEVENT_POPUPMENU_ITEM_CHANGED    = ConvertFrameEventType(10)
    constant frameeventtype         FRAMEEVENT_MOUSE_DOUBLECLICK         = ConvertFrameEventType(11)
    constant frameeventtype         FRAMEEVENT_SPRITE_ANIM_UPDATE        = ConvertFrameEventType(12)
    constant frameeventtype         FRAMEEVENT_SLIDER_VALUE_CHANGED      = ConvertFrameEventType(13)
    constant frameeventtype         FRAMEEVENT_DIALOG_CANCEL             = ConvertFrameEventType(14)
    constant frameeventtype         FRAMEEVENT_DIALOG_ACCEPT             = ConvertFrameEventType(15)
    constant frameeventtype         FRAMEEVENT_EDITBOX_ENTER             = ConvertFrameEventType(16)

//===================================================
// OS Key constants
//===================================================

    constant oskeytype              OSKEY_BACKSPACE                      = ConvertOsKeyType($08)
    constant oskeytype              OSKEY_TAB                            = ConvertOsKeyType($09)
    constant oskeytype              OSKEY_CLEAR                          = ConvertOsKeyType($0C)
    constant oskeytype              OSKEY_RETURN                         = ConvertOsKeyType($0D)
    constant oskeytype              OSKEY_SHIFT                          = ConvertOsKeyType($10)
    constant oskeytype              OSKEY_CONTROL                        = ConvertOsKeyType($11)
    constant oskeytype              OSKEY_ALT                            = ConvertOsKeyType($12)
    constant oskeytype              OSKEY_PAUSE                          = ConvertOsKeyType($13)
    constant oskeytype              OSKEY_CAPSLOCK                       = ConvertOsKeyType($14)
    constant oskeytype              OSKEY_KANA                           = ConvertOsKeyType($15)
    constant oskeytype              OSKEY_HANGUL                         = ConvertOsKeyType($15)
    constant oskeytype              OSKEY_JUNJA                          = ConvertOsKeyType($17)
    constant oskeytype              OSKEY_FINAL                          = ConvertOsKeyType($18)
    constant oskeytype              OSKEY_HANJA                          = ConvertOsKeyType($19)
    constant oskeytype              OSKEY_KANJI                          = ConvertOsKeyType($19)
    constant oskeytype              OSKEY_ESCAPE                         = ConvertOsKeyType($1B)
    constant oskeytype              OSKEY_CONVERT                        = ConvertOsKeyType($1C)
    constant oskeytype              OSKEY_NONCONVERT                     = ConvertOsKeyType($1D)
    constant oskeytype              OSKEY_ACCEPT                         = ConvertOsKeyType($1E)
    constant oskeytype              OSKEY_MODECHANGE                     = ConvertOsKeyType($1F)
    constant oskeytype              OSKEY_SPACE                          = ConvertOsKeyType($20)
    constant oskeytype              OSKEY_PAGEUP                         = ConvertOsKeyType($21)
    constant oskeytype              OSKEY_PAGEDOWN                       = ConvertOsKeyType($22)
    constant oskeytype              OSKEY_END                            = ConvertOsKeyType($23)
    constant oskeytype              OSKEY_HOME                           = ConvertOsKeyType($24)
    constant oskeytype              OSKEY_LEFT                           = ConvertOsKeyType($25)
    constant oskeytype              OSKEY_UP                             = ConvertOsKeyType($26)
    constant oskeytype              OSKEY_RIGHT                          = ConvertOsKeyType($27)
    constant oskeytype              OSKEY_DOWN                           = ConvertOsKeyType($28)
    constant oskeytype              OSKEY_SELECT                         = ConvertOsKeyType($29)
    constant oskeytype              OSKEY_PRINT                          = ConvertOsKeyType($2A)
    constant oskeytype              OSKEY_EXECUTE                        = ConvertOsKeyType($2B)
    constant oskeytype              OSKEY_PRINTSCREEN                    = ConvertOsKeyType($2C)
    constant oskeytype              OSKEY_INSERT                         = ConvertOsKeyType($2D)
    constant oskeytype              OSKEY_DELETE                         = ConvertOsKeyType($2E)
    constant oskeytype              OSKEY_HELP                           = ConvertOsKeyType($2F)
    constant oskeytype              OSKEY_0                              = ConvertOsKeyType($30)
    constant oskeytype              OSKEY_1                              = ConvertOsKeyType($31)
    constant oskeytype              OSKEY_2                              = ConvertOsKeyType($32)
    constant oskeytype              OSKEY_3                              = ConvertOsKeyType($33)
    constant oskeytype              OSKEY_4                              = ConvertOsKeyType($34)
    constant oskeytype              OSKEY_5                              = ConvertOsKeyType($35)
    constant oskeytype              OSKEY_6                              = ConvertOsKeyType($36)
    constant oskeytype              OSKEY_7                              = ConvertOsKeyType($37)
    constant oskeytype              OSKEY_8                              = ConvertOsKeyType($38)
    constant oskeytype              OSKEY_9                              = ConvertOsKeyType($39)
    constant oskeytype              OSKEY_A                              = ConvertOsKeyType($41)
    constant oskeytype              OSKEY_B                              = ConvertOsKeyType($42)
    constant oskeytype              OSKEY_C                              = ConvertOsKeyType($43)
    constant oskeytype              OSKEY_D                              = ConvertOsKeyType($44)
    constant oskeytype              OSKEY_E                              = ConvertOsKeyType($45)
    constant oskeytype              OSKEY_F                              = ConvertOsKeyType($46)
    constant oskeytype              OSKEY_G                              = ConvertOsKeyType($47)
    constant oskeytype              OSKEY_H                              = ConvertOsKeyType($48)
    constant oskeytype              OSKEY_I                              = ConvertOsKeyType($49)
    constant oskeytype              OSKEY_J                              = ConvertOsKeyType($4A)
    constant oskeytype              OSKEY_K                              = ConvertOsKeyType($4B)
    constant oskeytype              OSKEY_L                              = ConvertOsKeyType($4C)
    constant oskeytype              OSKEY_M                              = ConvertOsKeyType($4D)
    constant oskeytype              OSKEY_N                              = ConvertOsKeyType($4E)
    constant oskeytype              OSKEY_O                              = ConvertOsKeyType($4F)
    constant oskeytype              OSKEY_P                              = ConvertOsKeyType($50)
    constant oskeytype              OSKEY_Q                              = ConvertOsKeyType($51)
    constant oskeytype              OSKEY_R                              = ConvertOsKeyType($52)
    constant oskeytype              OSKEY_S                              = ConvertOsKeyType($53)
    constant oskeytype              OSKEY_T                              = ConvertOsKeyType($54)
    constant oskeytype              OSKEY_U                              = ConvertOsKeyType($55)
    constant oskeytype              OSKEY_V                              = ConvertOsKeyType($56)
    constant oskeytype              OSKEY_W                              = ConvertOsKeyType($57)
    constant oskeytype              OSKEY_X                              = ConvertOsKeyType($58)
    constant oskeytype              OSKEY_Y                              = ConvertOsKeyType($59)
    constant oskeytype              OSKEY_Z                              = ConvertOsKeyType($5A)
    constant oskeytype              OSKEY_LMETA                          = ConvertOsKeyType($5B)
    constant oskeytype              OSKEY_RMETA                          = ConvertOsKeyType($5C)
    constant oskeytype              OSKEY_APPS                           = ConvertOsKeyType($5D)
    constant oskeytype              OSKEY_SLEEP                          = ConvertOsKeyType($5F)
    constant oskeytype              OSKEY_NUMPAD0                        = ConvertOsKeyType($60)
    constant oskeytype              OSKEY_NUMPAD1                        = ConvertOsKeyType($61)
    constant oskeytype              OSKEY_NUMPAD2                        = ConvertOsKeyType($62)
    constant oskeytype              OSKEY_NUMPAD3                        = ConvertOsKeyType($63)
    constant oskeytype              OSKEY_NUMPAD4                        = ConvertOsKeyType($64)
    constant oskeytype              OSKEY_NUMPAD5                        = ConvertOsKeyType($65)
    constant oskeytype              OSKEY_NUMPAD6                        = ConvertOsKeyType($66)
    constant oskeytype              OSKEY_NUMPAD7                        = ConvertOsKeyType($67)
    constant oskeytype              OSKEY_NUMPAD8                        = ConvertOsKeyType($68)
    constant oskeytype              OSKEY_NUMPAD9                        = ConvertOsKeyType($69)
    constant oskeytype              OSKEY_MULTIPLY                       = ConvertOsKeyType($6A)
    constant oskeytype              OSKEY_ADD                            = ConvertOsKeyType($6B)
    constant oskeytype              OSKEY_SEPARATOR                      = ConvertOsKeyType($6C)
    constant oskeytype              OSKEY_SUBTRACT                       = ConvertOsKeyType($6D)
    constant oskeytype              OSKEY_DECIMAL                        = ConvertOsKeyType($6E)
    constant oskeytype              OSKEY_DIVIDE                         = ConvertOsKeyType($6F)
    constant oskeytype              OSKEY_F1                             = ConvertOsKeyType($70)
    constant oskeytype              OSKEY_F2                             = ConvertOsKeyType($71)
    constant oskeytype              OSKEY_F3                             = ConvertOsKeyType($72)
    constant oskeytype              OSKEY_F4                             = ConvertOsKeyType($73)
    constant oskeytype              OSKEY_F5                             = ConvertOsKeyType($74)
    constant oskeytype              OSKEY_F6                             = ConvertOsKeyType($75)
    constant oskeytype              OSKEY_F7                             = ConvertOsKeyType($76)
    constant oskeytype              OSKEY_F8                             = ConvertOsKeyType($77)
    constant oskeytype              OSKEY_F9                             = ConvertOsKeyType($78)
    constant oskeytype              OSKEY_F10                            = ConvertOsKeyType($79)
    constant oskeytype              OSKEY_F11                            = ConvertOsKeyType($7A)
    constant oskeytype              OSKEY_F12                            = ConvertOsKeyType($7B)
    constant oskeytype              OSKEY_F13                            = ConvertOsKeyType($7C)
    constant oskeytype              OSKEY_F14                            = ConvertOsKeyType($7D)
    constant oskeytype              OSKEY_F15                            = ConvertOsKeyType($7E)
    constant oskeytype              OSKEY_F16                            = ConvertOsKeyType($7F)
    constant oskeytype              OSKEY_F17                            = ConvertOsKeyType($80)
    constant oskeytype              OSKEY_F18                            = ConvertOsKeyType($81)
    constant oskeytype              OSKEY_F19                            = ConvertOsKeyType($82)
    constant oskeytype              OSKEY_F20                            = ConvertOsKeyType($83)
    constant oskeytype              OSKEY_F21                            = ConvertOsKeyType($84)
    constant oskeytype              OSKEY_F22                            = ConvertOsKeyType($85)
    constant oskeytype              OSKEY_F23                            = ConvertOsKeyType($86)
    constant oskeytype              OSKEY_F24                            = ConvertOsKeyType($87)
    constant oskeytype              OSKEY_NUMLOCK                        = ConvertOsKeyType($90)
    constant oskeytype              OSKEY_SCROLLLOCK                     = ConvertOsKeyType($91)
    constant oskeytype              OSKEY_OEM_NEC_EQUAL                  = ConvertOsKeyType($92)
    constant oskeytype              OSKEY_OEM_FJ_JISHO                   = ConvertOsKeyType($92)
    constant oskeytype              OSKEY_OEM_FJ_MASSHOU                 = ConvertOsKeyType($93)
    constant oskeytype              OSKEY_OEM_FJ_TOUROKU                 = ConvertOsKeyType($94)
    constant oskeytype              OSKEY_OEM_FJ_LOYA                    = ConvertOsKeyType($95)
    constant oskeytype              OSKEY_OEM_FJ_ROYA                    = ConvertOsKeyType($96)
    constant oskeytype              OSKEY_LSHIFT                         = ConvertOsKeyType($A0)
    constant oskeytype              OSKEY_RSHIFT                         = ConvertOsKeyType($A1)
    constant oskeytype              OSKEY_LCONTROL                       = ConvertOsKeyType($A2)
    constant oskeytype              OSKEY_RCONTROL                       = ConvertOsKeyType($A3)
    constant oskeytype              OSKEY_LALT                           = ConvertOsKeyType($A4)
    constant oskeytype              OSKEY_RALT                           = ConvertOsKeyType($A5)
    constant oskeytype              OSKEY_BROWSER_BACK                   = ConvertOsKeyType($A6)
    constant oskeytype              OSKEY_BROWSER_FORWARD                = ConvertOsKeyType($A7)
    constant oskeytype              OSKEY_BROWSER_REFRESH                = ConvertOsKeyType($A8)
    constant oskeytype              OSKEY_BROWSER_STOP                   = ConvertOsKeyType($A9)
    constant oskeytype              OSKEY_BROWSER_SEARCH                 = ConvertOsKeyType($AA)
    constant oskeytype              OSKEY_BROWSER_FAVORITES              = ConvertOsKeyType($AB)
    constant oskeytype              OSKEY_BROWSER_HOME                   = ConvertOsKeyType($AC)
    constant oskeytype              OSKEY_VOLUME_MUTE                    = ConvertOsKeyType($AD)
    constant oskeytype              OSKEY_VOLUME_DOWN                    = ConvertOsKeyType($AE)
    constant oskeytype              OSKEY_VOLUME_UP                      = ConvertOsKeyType($AF)
    constant oskeytype              OSKEY_MEDIA_NEXT_TRACK               = ConvertOsKeyType($B0)
    constant oskeytype              OSKEY_MEDIA_PREV_TRACK               = ConvertOsKeyType($B1)
    constant oskeytype              OSKEY_MEDIA_STOP                     = ConvertOsKeyType($B2)
    constant oskeytype              OSKEY_MEDIA_PLAY_PAUSE               = ConvertOsKeyType($B3)
    constant oskeytype              OSKEY_LAUNCH_MAIL                    = ConvertOsKeyType($B4)
    constant oskeytype              OSKEY_LAUNCH_MEDIA_SELECT            = ConvertOsKeyType($B5)
    constant oskeytype              OSKEY_LAUNCH_APP1                    = ConvertOsKeyType($B6)
    constant oskeytype              OSKEY_LAUNCH_APP2                    = ConvertOsKeyType($B7)
    constant oskeytype              OSKEY_OEM_1                          = ConvertOsKeyType($BA)
    constant oskeytype              OSKEY_OEM_PLUS                       = ConvertOsKeyType($BB)
    constant oskeytype              OSKEY_OEM_COMMA                      = ConvertOsKeyType($BC)
    constant oskeytype              OSKEY_OEM_MINUS                      = ConvertOsKeyType($BD)
    constant oskeytype              OSKEY_OEM_PERIOD                     = ConvertOsKeyType($BE)
    constant oskeytype              OSKEY_OEM_2                          = ConvertOsKeyType($BF)
    constant oskeytype              OSKEY_OEM_3                          = ConvertOsKeyType($C0)
    constant oskeytype              OSKEY_OEM_4                          = ConvertOsKeyType($DB)
    constant oskeytype              OSKEY_OEM_5                          = ConvertOsKeyType($DC)
    constant oskeytype              OSKEY_OEM_6                          = ConvertOsKeyType($DD)
    constant oskeytype              OSKEY_OEM_7                          = ConvertOsKeyType($DE)
    constant oskeytype              OSKEY_OEM_8                          = ConvertOsKeyType($DF)
    constant oskeytype              OSKEY_OEM_AX                         = ConvertOsKeyType($E1)
    constant oskeytype              OSKEY_OEM_102                        = ConvertOsKeyType($E2)
    constant oskeytype              OSKEY_ICO_HELP                       = ConvertOsKeyType($E3)
    constant oskeytype              OSKEY_ICO_00                         = ConvertOsKeyType($E4)
    constant oskeytype              OSKEY_PROCESSKEY                     = ConvertOsKeyType($E5)
    constant oskeytype              OSKEY_ICO_CLEAR                      = ConvertOsKeyType($E6)
    constant oskeytype              OSKEY_PACKET                         = ConvertOsKeyType($E7)
    constant oskeytype              OSKEY_OEM_RESET                      = ConvertOsKeyType($E9)
    constant oskeytype              OSKEY_OEM_JUMP                       = ConvertOsKeyType($EA)
    constant oskeytype              OSKEY_OEM_PA1                        = ConvertOsKeyType($EB)
    constant oskeytype              OSKEY_OEM_PA2                        = ConvertOsKeyType($EC)
    constant oskeytype              OSKEY_OEM_PA3                        = ConvertOsKeyType($ED)
    constant oskeytype              OSKEY_OEM_WSCTRL                     = ConvertOsKeyType($EE)
    constant oskeytype              OSKEY_OEM_CUSEL                      = ConvertOsKeyType($EF)
    constant oskeytype              OSKEY_OEM_ATTN                       = ConvertOsKeyType($F0)
    constant oskeytype              OSKEY_OEM_FINISH                     = ConvertOsKeyType($F1)
    constant oskeytype              OSKEY_OEM_COPY                       = ConvertOsKeyType($F2)
    constant oskeytype              OSKEY_OEM_AUTO                       = ConvertOsKeyType($F3)
    constant oskeytype              OSKEY_OEM_ENLW                       = ConvertOsKeyType($F4)
    constant oskeytype              OSKEY_OEM_BACKTAB                    = ConvertOsKeyType($F5)
    constant oskeytype              OSKEY_ATTN                           = ConvertOsKeyType($F6)
    constant oskeytype              OSKEY_CRSEL                          = ConvertOsKeyType($F7)
    constant oskeytype              OSKEY_EXSEL                          = ConvertOsKeyType($F8)
    constant oskeytype              OSKEY_EREOF                          = ConvertOsKeyType($F9)
    constant oskeytype              OSKEY_PLAY                           = ConvertOsKeyType($FA)
    constant oskeytype              OSKEY_ZOOM                           = ConvertOsKeyType($FB)
    constant oskeytype              OSKEY_NONAME                         = ConvertOsKeyType($FC)
    constant oskeytype              OSKEY_PA1                            = ConvertOsKeyType($FD)
    constant oskeytype              OSKEY_OEM_CLEAR                      = ConvertOsKeyType($FE)

//===================================================
// Instanced Object Operation API constants
//===================================================
    
    // Ability
    constant abilityintegerfield ABILITY_IF_BUTTON_POSITION_NORMAL_X        = ConvertAbilityIntegerField('abpx')
    constant abilityintegerfield ABILITY_IF_BUTTON_POSITION_NORMAL_Y        = ConvertAbilityIntegerField('abpy')
    constant abilityintegerfield ABILITY_IF_BUTTON_POSITION_ACTIVATED_X     = ConvertAbilityIntegerField('aubx')
    constant abilityintegerfield ABILITY_IF_BUTTON_POSITION_ACTIVATED_Y     = ConvertAbilityIntegerField('auby')
    constant abilityintegerfield ABILITY_IF_BUTTON_POSITION_RESEARCH_X      = ConvertAbilityIntegerField('arpx')
    constant abilityintegerfield ABILITY_IF_BUTTON_POSITION_RESEARCH_Y      = ConvertAbilityIntegerField('arpy')
    constant abilityintegerfield ABILITY_IF_MISSILE_SPEED                   = ConvertAbilityIntegerField('amsp')
    constant abilityintegerfield ABILITY_IF_TARGET_ATTACHMENTS              = ConvertAbilityIntegerField('atac')
    constant abilityintegerfield ABILITY_IF_CASTER_ATTACHMENTS              = ConvertAbilityIntegerField('acac')
    constant abilityintegerfield ABILITY_IF_PRIORITY                        = ConvertAbilityIntegerField('apri')
    constant abilityintegerfield ABILITY_IF_LEVELS                          = ConvertAbilityIntegerField('alev')
    constant abilityintegerfield ABILITY_IF_REQUIRED_LEVEL                  = ConvertAbilityIntegerField('arlv')
    constant abilityintegerfield ABILITY_IF_LEVEL_SKIP_REQUIREMENT          = ConvertAbilityIntegerField('alsk') 

    constant abilitybooleanfield ABILITY_BF_HERO_ABILITY                    = ConvertAbilityBooleanField('aher') // Get only
    constant abilitybooleanfield ABILITY_BF_ITEM_ABILITY                    = ConvertAbilityBooleanField('aite')
    constant abilitybooleanfield ABILITY_BF_CHECK_DEPENDENCIES              = ConvertAbilityBooleanField('achd')

    constant abilityrealfield ABILITY_RF_ARF_MISSILE_ARC                    = ConvertAbilityRealField('amac')

    constant abilitystringfield ABILITY_SF_NAME                             = ConvertAbilityStringField('anam') // Get Only
    constant abilitystringfield ABILITY_SF_ICON_ACTIVATED                   = ConvertAbilityStringField('auar')
    constant abilitystringfield ABILITY_SF_ICON_RESEARCH                    = ConvertAbilityStringField('arar')
    constant abilitystringfield ABILITY_SF_EFFECT_SOUND                     = ConvertAbilityStringField('aefs')
    constant abilitystringfield ABILITY_SF_EFFECT_SOUND_LOOPING             = ConvertAbilityStringField('aefl')

    constant abilityintegerlevelfield ABILITY_ILF_MANA_COST                         = ConvertAbilityIntegerLevelField('amcs')
    constant abilityintegerlevelfield ABILITY_ILF_NUMBER_OF_WAVES                   = ConvertAbilityIntegerLevelField('Hbz1')
    constant abilityintegerlevelfield ABILITY_ILF_NUMBER_OF_SHARDS                  = ConvertAbilityIntegerLevelField('Hbz3')
    constant abilityintegerlevelfield ABILITY_ILF_NUMBER_OF_UNITS_TELEPORTED        = ConvertAbilityIntegerLevelField('Hmt1')
    constant abilityintegerlevelfield ABILITY_ILF_SUMMONED_UNIT_COUNT_HWE2          = ConvertAbilityIntegerLevelField('Hwe2')
    constant abilityintegerlevelfield ABILITY_ILF_NUMBER_OF_IMAGES                  = ConvertAbilityIntegerLevelField('Omi1')
    constant abilityintegerlevelfield ABILITY_ILF_NUMBER_OF_CORPSES_RAISED_UAN1     = ConvertAbilityIntegerLevelField('Uan1')
    constant abilityintegerlevelfield ABILITY_ILF_MORPHING_FLAGS                    = ConvertAbilityIntegerLevelField('Eme2')
    constant abilityintegerlevelfield ABILITY_ILF_STRENGTH_BONUS_NRG5               = ConvertAbilityIntegerLevelField('Nrg5')
    constant abilityintegerlevelfield ABILITY_ILF_DEFENSE_BONUS_NRG6                = ConvertAbilityIntegerLevelField('Nrg6')
    constant abilityintegerlevelfield ABILITY_ILF_NUMBER_OF_TARGETS_HIT             = ConvertAbilityIntegerLevelField('Ocl2')
    constant abilityintegerlevelfield ABILITY_ILF_DETECTION_TYPE_OFS1               = ConvertAbilityIntegerLevelField('Ofs1')
    constant abilityintegerlevelfield ABILITY_ILF_NUMBER_OF_SUMMONED_UNITS_OSF2     = ConvertAbilityIntegerLevelField('Osf2')
    constant abilityintegerlevelfield ABILITY_ILF_NUMBER_OF_SUMMONED_UNITS_EFN1     = ConvertAbilityIntegerLevelField('Efn1')
    constant abilityintegerlevelfield ABILITY_ILF_NUMBER_OF_CORPSES_RAISED_HRE1     = ConvertAbilityIntegerLevelField('Hre1')
    constant abilityintegerlevelfield ABILITY_ILF_STACK_FLAGS                       = ConvertAbilityIntegerLevelField('Hca4')
    constant abilityintegerlevelfield ABILITY_ILF_MINIMUM_NUMBER_OF_UNITS           = ConvertAbilityIntegerLevelField('Ndp2')
    constant abilityintegerlevelfield ABILITY_ILF_MAXIMUM_NUMBER_OF_UNITS_NDP3      = ConvertAbilityIntegerLevelField('Ndp3')
    constant abilityintegerlevelfield ABILITY_ILF_NUMBER_OF_UNITS_CREATED_NRC2      = ConvertAbilityIntegerLevelField('Nrc2')
    constant abilityintegerlevelfield ABILITY_ILF_SHIELD_LIFE                       = ConvertAbilityIntegerLevelField('Ams3')
    constant abilityintegerlevelfield ABILITY_ILF_MANA_LOSS_AMS4                    = ConvertAbilityIntegerLevelField('Ams4')
    constant abilityintegerlevelfield ABILITY_ILF_GOLD_PER_INTERVAL_BGM1            = ConvertAbilityIntegerLevelField('Bgm1')
    constant abilityintegerlevelfield ABILITY_ILF_MAX_NUMBER_OF_MINERS              = ConvertAbilityIntegerLevelField('Bgm3')
    constant abilityintegerlevelfield ABILITY_ILF_CARGO_CAPACITY                    = ConvertAbilityIntegerLevelField('Car1')
    constant abilityintegerlevelfield ABILITY_ILF_MAXIMUM_CREEP_LEVEL_DEV3          = ConvertAbilityIntegerLevelField('Dev3')
    constant abilityintegerlevelfield ABILITY_ILF_MAX_CREEP_LEVEL_DEV1              = ConvertAbilityIntegerLevelField('Dev1')
    constant abilityintegerlevelfield ABILITY_ILF_GOLD_PER_INTERVAL_EGM1            = ConvertAbilityIntegerLevelField('Egm1')
    constant abilityintegerlevelfield ABILITY_ILF_DEFENSE_REDUCTION                 = ConvertAbilityIntegerLevelField('Fae1')
    constant abilityintegerlevelfield ABILITY_ILF_DETECTION_TYPE_FLA1               = ConvertAbilityIntegerLevelField('Fla1')
    constant abilityintegerlevelfield ABILITY_ILF_FLARE_COUNT                       = ConvertAbilityIntegerLevelField('Fla3')
    constant abilityintegerlevelfield ABILITY_ILF_MAX_GOLD                          = ConvertAbilityIntegerLevelField('Gld1')
    constant abilityintegerlevelfield ABILITY_ILF_MINING_CAPACITY                   = ConvertAbilityIntegerLevelField('Gld3')
    constant abilityintegerlevelfield ABILITY_ILF_MAXIMUM_NUMBER_OF_CORPSES_GYD1    = ConvertAbilityIntegerLevelField('Gyd1')
    constant abilityintegerlevelfield ABILITY_ILF_DAMAGE_TO_TREE                    = ConvertAbilityIntegerLevelField('Har1')
    constant abilityintegerlevelfield ABILITY_ILF_LUMBER_CAPACITY                   = ConvertAbilityIntegerLevelField('Har2')
    constant abilityintegerlevelfield ABILITY_ILF_GOLD_CAPACITY                     = ConvertAbilityIntegerLevelField('Har3')
    constant abilityintegerlevelfield ABILITY_ILF_DEFENSE_INCREASE_INF2             = ConvertAbilityIntegerLevelField('Inf2')
    constant abilityintegerlevelfield ABILITY_ILF_INTERACTION_TYPE                  = ConvertAbilityIntegerLevelField('Neu2')
    constant abilityintegerlevelfield ABILITY_ILF_GOLD_COST_NDT1                    = ConvertAbilityIntegerLevelField('Ndt1')
    constant abilityintegerlevelfield ABILITY_ILF_LUMBER_COST_NDT2                  = ConvertAbilityIntegerLevelField('Ndt2')
    constant abilityintegerlevelfield ABILITY_ILF_DETECTION_TYPE_NDT3               = ConvertAbilityIntegerLevelField('Ndt3')
    constant abilityintegerlevelfield ABILITY_ILF_STACKING_TYPE_POI4                = ConvertAbilityIntegerLevelField('Poi4')
    constant abilityintegerlevelfield ABILITY_ILF_STACKING_TYPE_POA5                = ConvertAbilityIntegerLevelField('Poa5')
    constant abilityintegerlevelfield ABILITY_ILF_MAXIMUM_CREEP_LEVEL_PLY1          = ConvertAbilityIntegerLevelField('Ply1')
    constant abilityintegerlevelfield ABILITY_ILF_MAXIMUM_CREEP_LEVEL_POS1          = ConvertAbilityIntegerLevelField('Pos1')
    constant abilityintegerlevelfield ABILITY_ILF_MOVEMENT_UPDATE_FREQUENCY_PRG1    = ConvertAbilityIntegerLevelField('Prg1')
    constant abilityintegerlevelfield ABILITY_ILF_ATTACK_UPDATE_FREQUENCY_PRG2      = ConvertAbilityIntegerLevelField('Prg2')
    constant abilityintegerlevelfield ABILITY_ILF_MANA_LOSS_PRG6                    = ConvertAbilityIntegerLevelField('Prg6')
    constant abilityintegerlevelfield ABILITY_ILF_UNITS_SUMMONED_TYPE_ONE           = ConvertAbilityIntegerLevelField('Rai1')
    constant abilityintegerlevelfield ABILITY_ILF_UNITS_SUMMONED_TYPE_TWO           = ConvertAbilityIntegerLevelField('Rai2')
    constant abilityintegerlevelfield ABILITY_ILF_MAX_UNITS_SUMMONED                = ConvertAbilityIntegerLevelField('Ucb5')
    constant abilityintegerlevelfield ABILITY_ILF_ALLOW_WHEN_FULL_REJ3              = ConvertAbilityIntegerLevelField('Rej3')
    constant abilityintegerlevelfield ABILITY_ILF_MAXIMUM_UNITS_CHARGED_TO_CASTER   = ConvertAbilityIntegerLevelField('Rpb5')
    constant abilityintegerlevelfield ABILITY_ILF_MAXIMUM_UNITS_AFFECTED            = ConvertAbilityIntegerLevelField('Rpb6')
    constant abilityintegerlevelfield ABILITY_ILF_DEFENSE_INCREASE_ROA2             = ConvertAbilityIntegerLevelField('Roa2')
    constant abilityintegerlevelfield ABILITY_ILF_MAX_UNITS_ROA7                    = ConvertAbilityIntegerLevelField('Roa7')
    constant abilityintegerlevelfield ABILITY_ILF_ROOTED_WEAPONS                    = ConvertAbilityIntegerLevelField('Roo1')
    constant abilityintegerlevelfield ABILITY_ILF_UPROOTED_WEAPONS                  = ConvertAbilityIntegerLevelField('Roo2')
    constant abilityintegerlevelfield ABILITY_ILF_UPROOTED_DEFENSE_TYPE             = ConvertAbilityIntegerLevelField('Roo4')
    constant abilityintegerlevelfield ABILITY_ILF_ACCUMULATION_STEP                 = ConvertAbilityIntegerLevelField('Sal2')
    constant abilityintegerlevelfield ABILITY_ILF_NUMBER_OF_OWLS                    = ConvertAbilityIntegerLevelField('Esn4')
    constant abilityintegerlevelfield ABILITY_ILF_STACKING_TYPE_SPO4                = ConvertAbilityIntegerLevelField('Spo4')
    constant abilityintegerlevelfield ABILITY_ILF_NUMBER_OF_UNITS                   = ConvertAbilityIntegerLevelField('Sod1')
    constant abilityintegerlevelfield ABILITY_ILF_SPIDER_CAPACITY                   = ConvertAbilityIntegerLevelField('Spa1')
    constant abilityintegerlevelfield ABILITY_ILF_INTERVALS_BEFORE_CHANGING_TREES   = ConvertAbilityIntegerLevelField('Wha2')
    constant abilityintegerlevelfield ABILITY_ILF_AGILITY_BONUS                     = ConvertAbilityIntegerLevelField('Iagi')
    constant abilityintegerlevelfield ABILITY_ILF_INTELLIGENCE_BONUS                = ConvertAbilityIntegerLevelField('Iint')
    constant abilityintegerlevelfield ABILITY_ILF_STRENGTH_BONUS_ISTR               = ConvertAbilityIntegerLevelField('Istr')
    constant abilityintegerlevelfield ABILITY_ILF_ATTACK_BONUS                      = ConvertAbilityIntegerLevelField('Iatt')
    constant abilityintegerlevelfield ABILITY_ILF_DEFENSE_BONUS_IDEF                = ConvertAbilityIntegerLevelField('Idef')
    constant abilityintegerlevelfield ABILITY_ILF_SUMMON_1_AMOUNT                   = ConvertAbilityIntegerLevelField('Isn1')
    constant abilityintegerlevelfield ABILITY_ILF_SUMMON_2_AMOUNT                   = ConvertAbilityIntegerLevelField('Isn2')
    constant abilityintegerlevelfield ABILITY_ILF_EXPERIENCE_GAINED                 = ConvertAbilityIntegerLevelField('Ixpg')
    constant abilityintegerlevelfield ABILITY_ILF_HIT_POINTS_GAINED_IHPG            = ConvertAbilityIntegerLevelField('Ihpg')
    constant abilityintegerlevelfield ABILITY_ILF_MANA_POINTS_GAINED_IMPG           = ConvertAbilityIntegerLevelField('Impg')
    constant abilityintegerlevelfield ABILITY_ILF_HIT_POINTS_GAINED_IHP2            = ConvertAbilityIntegerLevelField('Ihp2')
    constant abilityintegerlevelfield ABILITY_ILF_MANA_POINTS_GAINED_IMP2           = ConvertAbilityIntegerLevelField('Imp2')
    constant abilityintegerlevelfield ABILITY_ILF_DAMAGE_BONUS_DICE                 = ConvertAbilityIntegerLevelField('Idic')
    constant abilityintegerlevelfield ABILITY_ILF_ARMOR_PENALTY_IARP                = ConvertAbilityIntegerLevelField('Iarp')
    constant abilityintegerlevelfield ABILITY_ILF_ENABLED_ATTACK_INDEX_IOB5         = ConvertAbilityIntegerLevelField('Iob5')
    constant abilityintegerlevelfield ABILITY_ILF_LEVELS_GAINED                     = ConvertAbilityIntegerLevelField('Ilev')
    constant abilityintegerlevelfield ABILITY_ILF_MAX_LIFE_GAINED                   = ConvertAbilityIntegerLevelField('Ilif')
    constant abilityintegerlevelfield ABILITY_ILF_MAX_MANA_GAINED                   = ConvertAbilityIntegerLevelField('Iman')
    constant abilityintegerlevelfield ABILITY_ILF_GOLD_GIVEN                        = ConvertAbilityIntegerLevelField('Igol')
    constant abilityintegerlevelfield ABILITY_ILF_LUMBER_GIVEN                      = ConvertAbilityIntegerLevelField('Ilum')
    constant abilityintegerlevelfield ABILITY_ILF_DETECTION_TYPE_IFA1               = ConvertAbilityIntegerLevelField('Ifa1')
    constant abilityintegerlevelfield ABILITY_ILF_MAXIMUM_CREEP_LEVEL_ICRE          = ConvertAbilityIntegerLevelField('Icre')
    constant abilityintegerlevelfield ABILITY_ILF_MOVEMENT_SPEED_BONUS              = ConvertAbilityIntegerLevelField('Imvb')
    constant abilityintegerlevelfield ABILITY_ILF_HIT_POINTS_REGENERATED_PER_SECOND = ConvertAbilityIntegerLevelField('Ihpr')
    constant abilityintegerlevelfield ABILITY_ILF_SIGHT_RANGE_BONUS                 = ConvertAbilityIntegerLevelField('Isib')
    constant abilityintegerlevelfield ABILITY_ILF_DAMAGE_PER_DURATION               = ConvertAbilityIntegerLevelField('Icfd')
    constant abilityintegerlevelfield ABILITY_ILF_MANA_USED_PER_SECOND              = ConvertAbilityIntegerLevelField('Icfm')
    constant abilityintegerlevelfield ABILITY_ILF_EXTRA_MANA_REQUIRED               = ConvertAbilityIntegerLevelField('Icfx')
    constant abilityintegerlevelfield ABILITY_ILF_DETECTION_RADIUS_IDET             = ConvertAbilityIntegerLevelField('Idet')
    constant abilityintegerlevelfield ABILITY_ILF_MANA_LOSS_PER_UNIT_IDIM           = ConvertAbilityIntegerLevelField('Idim')
    constant abilityintegerlevelfield ABILITY_ILF_DAMAGE_TO_SUMMONED_UNITS_IDID     = ConvertAbilityIntegerLevelField('Idid')
    constant abilityintegerlevelfield ABILITY_ILF_MAXIMUM_NUMBER_OF_UNITS_IREC      = ConvertAbilityIntegerLevelField('Irec')
    constant abilityintegerlevelfield ABILITY_ILF_DELAY_AFTER_DEATH_SECONDS         = ConvertAbilityIntegerLevelField('Ircd')
    constant abilityintegerlevelfield ABILITY_ILF_RESTORED_LIFE                     = ConvertAbilityIntegerLevelField('irc2')
    constant abilityintegerlevelfield ABILITY_ILF_RESTORED_MANA__1_FOR_CURRENT      = ConvertAbilityIntegerLevelField('irc3')
    constant abilityintegerlevelfield ABILITY_ILF_HIT_POINTS_RESTORED               = ConvertAbilityIntegerLevelField('Ihps')
    constant abilityintegerlevelfield ABILITY_ILF_MANA_POINTS_RESTORED              = ConvertAbilityIntegerLevelField('Imps')
    constant abilityintegerlevelfield ABILITY_ILF_MAXIMUM_NUMBER_OF_UNITS_ITPM      = ConvertAbilityIntegerLevelField('Itpm')
    constant abilityintegerlevelfield ABILITY_ILF_NUMBER_OF_CORPSES_RAISED_CAD1     = ConvertAbilityIntegerLevelField('Cad1')
    constant abilityintegerlevelfield ABILITY_ILF_TERRAIN_DEFORMATION_DURATION_MS   = ConvertAbilityIntegerLevelField('Wrs3')
    constant abilityintegerlevelfield ABILITY_ILF_MAXIMUM_UNITS                     = ConvertAbilityIntegerLevelField('Uds1')
    constant abilityintegerlevelfield ABILITY_ILF_DETECTION_TYPE_DET1               = ConvertAbilityIntegerLevelField('Det1')
    constant abilityintegerlevelfield ABILITY_ILF_GOLD_COST_PER_STRUCTURE           = ConvertAbilityIntegerLevelField('Nsp1')
    constant abilityintegerlevelfield ABILITY_ILF_LUMBER_COST_PER_USE               = ConvertAbilityIntegerLevelField('Nsp2')
    constant abilityintegerlevelfield ABILITY_ILF_DETECTION_TYPE_NSP3               = ConvertAbilityIntegerLevelField('Nsp3')
    constant abilityintegerlevelfield ABILITY_ILF_NUMBER_OF_SWARM_UNITS             = ConvertAbilityIntegerLevelField('Uls1')
    constant abilityintegerlevelfield ABILITY_ILF_MAX_SWARM_UNITS_PER_TARGET        = ConvertAbilityIntegerLevelField('Uls3')
    constant abilityintegerlevelfield ABILITY_ILF_NUMBER_OF_SUMMONED_UNITS_NBA2     = ConvertAbilityIntegerLevelField('Nba2')
    constant abilityintegerlevelfield ABILITY_ILF_MAXIMUM_CREEP_LEVEL_NCH1          = ConvertAbilityIntegerLevelField('Nch1')
    constant abilityintegerlevelfield ABILITY_ILF_ATTACKS_PREVENTED                 = ConvertAbilityIntegerLevelField('Nsi1')
    constant abilityintegerlevelfield ABILITY_ILF_MAXIMUM_NUMBER_OF_TARGETS_EFK3    = ConvertAbilityIntegerLevelField('Efk3')
    constant abilityintegerlevelfield ABILITY_ILF_NUMBER_OF_SUMMONED_UNITS_ESV1     = ConvertAbilityIntegerLevelField('Esv1')
    constant abilityintegerlevelfield ABILITY_ILF_MAXIMUM_NUMBER_OF_CORPSES_EXH1    = ConvertAbilityIntegerLevelField('exh1')
    constant abilityintegerlevelfield ABILITY_ILF_ITEM_CAPACITY                     = ConvertAbilityIntegerLevelField('inv1')
    constant abilityintegerlevelfield ABILITY_ILF_MAXIMUM_NUMBER_OF_TARGETS_SPL2    = ConvertAbilityIntegerLevelField('spl2')
    constant abilityintegerlevelfield ABILITY_ILF_ALLOW_WHEN_FULL_IRL3              = ConvertAbilityIntegerLevelField('irl3')
    constant abilityintegerlevelfield ABILITY_ILF_MAXIMUM_DISPELLED_UNITS           = ConvertAbilityIntegerLevelField('idc3')
    constant abilityintegerlevelfield ABILITY_ILF_NUMBER_OF_LURES                   = ConvertAbilityIntegerLevelField('imo1')
    constant abilityintegerlevelfield ABILITY_ILF_NEW_TIME_OF_DAY_HOUR              = ConvertAbilityIntegerLevelField('ict1')
    constant abilityintegerlevelfield ABILITY_ILF_NEW_TIME_OF_DAY_MINUTE            = ConvertAbilityIntegerLevelField('ict2')
    constant abilityintegerlevelfield ABILITY_ILF_NUMBER_OF_UNITS_CREATED_MEC1      = ConvertAbilityIntegerLevelField('mec1')
    constant abilityintegerlevelfield ABILITY_ILF_MINIMUM_SPELLS                    = ConvertAbilityIntegerLevelField('spb3')
    constant abilityintegerlevelfield ABILITY_ILF_MAXIMUM_SPELLS                    = ConvertAbilityIntegerLevelField('spb4')
    constant abilityintegerlevelfield ABILITY_ILF_DISABLED_ATTACK_INDEX             = ConvertAbilityIntegerLevelField('gra3')
    constant abilityintegerlevelfield ABILITY_ILF_ENABLED_ATTACK_INDEX_GRA4         = ConvertAbilityIntegerLevelField('gra4')
    constant abilityintegerlevelfield ABILITY_ILF_MAXIMUM_ATTACKS                   = ConvertAbilityIntegerLevelField('gra5')
    constant abilityintegerlevelfield ABILITY_ILF_BUILDING_TYPES_ALLOWED_NPR1       = ConvertAbilityIntegerLevelField('Npr1')
    constant abilityintegerlevelfield ABILITY_ILF_BUILDING_TYPES_ALLOWED_NSA1       = ConvertAbilityIntegerLevelField('Nsa1')
    constant abilityintegerlevelfield ABILITY_ILF_ATTACK_MODIFICATION               = ConvertAbilityIntegerLevelField('Iaa1')
    constant abilityintegerlevelfield ABILITY_ILF_SUMMONED_UNIT_COUNT_NPA5          = ConvertAbilityIntegerLevelField('Npa5')
    constant abilityintegerlevelfield ABILITY_ILF_UPGRADE_LEVELS                    = ConvertAbilityIntegerLevelField('Igl1')
    constant abilityintegerlevelfield ABILITY_ILF_NUMBER_OF_SUMMONED_UNITS_NDO2     = ConvertAbilityIntegerLevelField('Ndo2')
    constant abilityintegerlevelfield ABILITY_ILF_BEASTS_PER_SECOND                 = ConvertAbilityIntegerLevelField('Nst1')
    constant abilityintegerlevelfield ABILITY_ILF_TARGET_TYPE                       = ConvertAbilityIntegerLevelField('Ncl2')
    constant abilityintegerlevelfield ABILITY_ILF_OPTIONS                           = ConvertAbilityIntegerLevelField('Ncl3')
    constant abilityintegerlevelfield ABILITY_ILF_ARMOR_PENALTY_NAB3                = ConvertAbilityIntegerLevelField('Nab3')
    constant abilityintegerlevelfield ABILITY_ILF_WAVE_COUNT_NHS6                   = ConvertAbilityIntegerLevelField('Nhs6')
    constant abilityintegerlevelfield ABILITY_ILF_MAX_CREEP_LEVEL_NTM3              = ConvertAbilityIntegerLevelField('Ntm3')
    constant abilityintegerlevelfield ABILITY_ILF_MISSILE_COUNT                     = ConvertAbilityIntegerLevelField('Ncs3')
    constant abilityintegerlevelfield ABILITY_ILF_SPLIT_ATTACK_COUNT                = ConvertAbilityIntegerLevelField('Nlm3')
    constant abilityintegerlevelfield ABILITY_ILF_GENERATION_COUNT                  = ConvertAbilityIntegerLevelField('Nlm6')
    constant abilityintegerlevelfield ABILITY_ILF_ROCK_RING_COUNT                   = ConvertAbilityIntegerLevelField('Nvc1')
    constant abilityintegerlevelfield ABILITY_ILF_WAVE_COUNT_NVC2                   = ConvertAbilityIntegerLevelField('Nvc2')
    constant abilityintegerlevelfield ABILITY_ILF_PREFER_HOSTILES_TAU1              = ConvertAbilityIntegerLevelField('Tau1')
    constant abilityintegerlevelfield ABILITY_ILF_PREFER_FRIENDLIES_TAU2            = ConvertAbilityIntegerLevelField('Tau2')
    constant abilityintegerlevelfield ABILITY_ILF_MAX_UNITS_TAU3                    = ConvertAbilityIntegerLevelField('Tau3')
    constant abilityintegerlevelfield ABILITY_ILF_NUMBER_OF_PULSES                  = ConvertAbilityIntegerLevelField('Tau4')
    constant abilityintegerlevelfield ABILITY_ILF_SUMMONED_UNIT_TYPE_HWE1           = ConvertAbilityIntegerLevelField('Hwe1')
    constant abilityintegerlevelfield ABILITY_ILF_SUMMONED_UNIT_UIN4                = ConvertAbilityIntegerLevelField('Uin4')
    constant abilityintegerlevelfield ABILITY_ILF_SUMMONED_UNIT_OSF1                = ConvertAbilityIntegerLevelField('Osf1')
    constant abilityintegerlevelfield ABILITY_ILF_SUMMONED_UNIT_TYPE_EFNU           = ConvertAbilityIntegerLevelField('Efnu')
    constant abilityintegerlevelfield ABILITY_ILF_SUMMONED_UNIT_TYPE_NBAU           = ConvertAbilityIntegerLevelField('Nbau')
    constant abilityintegerlevelfield ABILITY_ILF_SUMMONED_UNIT_TYPE_NTOU           = ConvertAbilityIntegerLevelField('Ntou')
    constant abilityintegerlevelfield ABILITY_ILF_SUMMONED_UNIT_TYPE_ESVU           = ConvertAbilityIntegerLevelField('Esvu')
    constant abilityintegerlevelfield ABILITY_ILF_SUMMONED_UNIT_TYPES               = ConvertAbilityIntegerLevelField('Nef1')
    constant abilityintegerlevelfield ABILITY_ILF_SUMMONED_UNIT_TYPE_NDOU           = ConvertAbilityIntegerLevelField('Ndou')
    constant abilityintegerlevelfield ABILITY_ILF_ALTERNATE_FORM_UNIT_EMEU          = ConvertAbilityIntegerLevelField('Emeu')
    constant abilityintegerlevelfield ABILITY_ILF_PLAGUE_WARD_UNIT_TYPE             = ConvertAbilityIntegerLevelField('Aplu')
    constant abilityintegerlevelfield ABILITY_ILF_ALLOWED_UNIT_TYPE_BTL1            = ConvertAbilityIntegerLevelField('Btl1')
    constant abilityintegerlevelfield ABILITY_ILF_NEW_UNIT_TYPE                     = ConvertAbilityIntegerLevelField('Cha1')
    constant abilityintegerlevelfield ABILITY_ILF_RESULTING_UNIT_TYPE_ENT1          = ConvertAbilityIntegerLevelField('ent1')
    constant abilityintegerlevelfield ABILITY_ILF_CORPSE_UNIT_TYPE                  = ConvertAbilityIntegerLevelField('Gydu')
    constant abilityintegerlevelfield ABILITY_ILF_ALLOWED_UNIT_TYPE_LOA1            = ConvertAbilityIntegerLevelField('Loa1')
    constant abilityintegerlevelfield ABILITY_ILF_UNIT_TYPE_FOR_LIMIT_CHECK         = ConvertAbilityIntegerLevelField('Raiu')
    constant abilityintegerlevelfield ABILITY_ILF_WARD_UNIT_TYPE_STAU               = ConvertAbilityIntegerLevelField('Stau')
    constant abilityintegerlevelfield ABILITY_ILF_EFFECT_ABILITY                    = ConvertAbilityIntegerLevelField('Iobu')
    constant abilityintegerlevelfield ABILITY_ILF_CONVERSION_UNIT                   = ConvertAbilityIntegerLevelField('Ndc2')
    constant abilityintegerlevelfield ABILITY_ILF_UNIT_TO_PRESERVE                  = ConvertAbilityIntegerLevelField('Nsl1')
    constant abilityintegerlevelfield ABILITY_ILF_UNIT_TYPE_ALLOWED                 = ConvertAbilityIntegerLevelField('Chl1')
    constant abilityintegerlevelfield ABILITY_ILF_SWARM_UNIT_TYPE                   = ConvertAbilityIntegerLevelField('Ulsu')
    constant abilityintegerlevelfield ABILITY_ILF_RESULTING_UNIT_TYPE_COAU          = ConvertAbilityIntegerLevelField('coau')
    constant abilityintegerlevelfield ABILITY_ILF_UNIT_TYPE_EXHU                    = ConvertAbilityIntegerLevelField('exhu')
    constant abilityintegerlevelfield ABILITY_ILF_WARD_UNIT_TYPE_HWDU               = ConvertAbilityIntegerLevelField('hwdu')
    constant abilityintegerlevelfield ABILITY_ILF_LURE_UNIT_TYPE                    = ConvertAbilityIntegerLevelField('imou')
    constant abilityintegerlevelfield ABILITY_ILF_UNIT_TYPE_IPMU                    = ConvertAbilityIntegerLevelField('ipmu')
    constant abilityintegerlevelfield ABILITY_ILF_FACTORY_UNIT_ID                   = ConvertAbilityIntegerLevelField('Nsyu')
    constant abilityintegerlevelfield ABILITY_ILF_SPAWN_UNIT_ID_NFYU                = ConvertAbilityIntegerLevelField('Nfyu')
    constant abilityintegerlevelfield ABILITY_ILF_DESTRUCTIBLE_ID                   = ConvertAbilityIntegerLevelField('Nvcu')
    constant abilityintegerlevelfield ABILITY_ILF_UPGRADE_TYPE                      = ConvertAbilityIntegerLevelField('Iglu')

    constant abilityreallevelfield ABILITY_RLF_CASTING_TIME                                      = ConvertAbilityRealLevelField('acas')
    constant abilityreallevelfield ABILITY_RLF_DURATION_NORMAL                                   = ConvertAbilityRealLevelField('adur')
    constant abilityreallevelfield ABILITY_RLF_DURATION_HERO                                     = ConvertAbilityRealLevelField('ahdu')
    constant abilityreallevelfield ABILITY_RLF_COOLDOWN                                          = ConvertAbilityRealLevelField('acdn')
    constant abilityreallevelfield ABILITY_RLF_AREA_OF_EFFECT                                    = ConvertAbilityRealLevelField('aare')
    constant abilityreallevelfield ABILITY_RLF_CAST_RANGE                                        = ConvertAbilityRealLevelField('aran')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_HBZ2                                       = ConvertAbilityRealLevelField('Hbz2')
    constant abilityreallevelfield ABILITY_RLF_BUILDING_REDUCTION_HBZ4                           = ConvertAbilityRealLevelField('Hbz4')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_PER_SECOND_HBZ5                            = ConvertAbilityRealLevelField('Hbz5')
    constant abilityreallevelfield ABILITY_RLF_MAXIMUM_DAMAGE_PER_WAVE                           = ConvertAbilityRealLevelField('Hbz6')
    constant abilityreallevelfield ABILITY_RLF_MANA_REGENERATION_INCREASE                        = ConvertAbilityRealLevelField('Hab1')
    constant abilityreallevelfield ABILITY_RLF_CASTING_DELAY                                     = ConvertAbilityRealLevelField('Hmt2')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_PER_SECOND_OWW1                            = ConvertAbilityRealLevelField('Oww1')
    constant abilityreallevelfield ABILITY_RLF_MAGIC_DAMAGE_REDUCTION_OWW2                       = ConvertAbilityRealLevelField('Oww2')
    constant abilityreallevelfield ABILITY_RLF_CHANCE_TO_CRITICAL_STRIKE                         = ConvertAbilityRealLevelField('Ocr1')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_MULTIPLIER_OCR2                            = ConvertAbilityRealLevelField('Ocr2')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_BONUS_OCR3                                 = ConvertAbilityRealLevelField('Ocr3')
    constant abilityreallevelfield ABILITY_RLF_CHANCE_TO_EVADE_OCR4                              = ConvertAbilityRealLevelField('Ocr4')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_DEALT_PERCENT_OMI2                         = ConvertAbilityRealLevelField('Omi2')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_TAKEN_PERCENT_OMI3                         = ConvertAbilityRealLevelField('Omi3')
    constant abilityreallevelfield ABILITY_RLF_ANIMATION_DELAY                                   = ConvertAbilityRealLevelField('Omi4')
    constant abilityreallevelfield ABILITY_RLF_TRANSITION_TIME                                   = ConvertAbilityRealLevelField('Owk1')
    constant abilityreallevelfield ABILITY_RLF_MOVEMENT_SPEED_INCREASE_PERCENT_OWK2              = ConvertAbilityRealLevelField('Owk2')
    constant abilityreallevelfield ABILITY_RLF_BACKSTAB_DAMAGE                                   = ConvertAbilityRealLevelField('Owk3')
    constant abilityreallevelfield ABILITY_RLF_AMOUNT_HEALED_DAMAGED_UDC1                        = ConvertAbilityRealLevelField('Udc1')
    constant abilityreallevelfield ABILITY_RLF_LIFE_CONVERTED_TO_MANA                            = ConvertAbilityRealLevelField('Udp1')
    constant abilityreallevelfield ABILITY_RLF_LIFE_CONVERTED_TO_LIFE                            = ConvertAbilityRealLevelField('Udp2')
    constant abilityreallevelfield ABILITY_RLF_MOVEMENT_SPEED_INCREASE_PERCENT_UAU1              = ConvertAbilityRealLevelField('Uau1')
    constant abilityreallevelfield ABILITY_RLF_LIFE_REGENERATION_INCREASE_PERCENT                = ConvertAbilityRealLevelField('Uau2')
    constant abilityreallevelfield ABILITY_RLF_CHANCE_TO_EVADE_EEV1                              = ConvertAbilityRealLevelField('Eev1')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_PER_INTERVAL                               = ConvertAbilityRealLevelField('Eim1')
    constant abilityreallevelfield ABILITY_RLF_MANA_DRAINED_PER_SECOND_EIM2                      = ConvertAbilityRealLevelField('Eim2')
    constant abilityreallevelfield ABILITY_RLF_BUFFER_MANA_REQUIRED                              = ConvertAbilityRealLevelField('Eim3')
    constant abilityreallevelfield ABILITY_RLF_MAX_MANA_DRAINED                                  = ConvertAbilityRealLevelField('Emb1')
    constant abilityreallevelfield ABILITY_RLF_BOLT_DELAY                                        = ConvertAbilityRealLevelField('Emb2')
    constant abilityreallevelfield ABILITY_RLF_BOLT_LIFETIME                                     = ConvertAbilityRealLevelField('Emb3')
    constant abilityreallevelfield ABILITY_RLF_ALTITUDE_ADJUSTMENT_DURATION                      = ConvertAbilityRealLevelField('Eme3')
    constant abilityreallevelfield ABILITY_RLF_LANDING_DELAY_TIME                                = ConvertAbilityRealLevelField('Eme4')
    constant abilityreallevelfield ABILITY_RLF_ALTERNATE_FORM_HIT_POINT_BONUS                    = ConvertAbilityRealLevelField('Eme5')
    constant abilityreallevelfield ABILITY_RLF_MOVE_SPEED_BONUS_INFO_PANEL_ONLY                  = ConvertAbilityRealLevelField('Ncr5')
    constant abilityreallevelfield ABILITY_RLF_ATTACK_SPEED_BONUS_INFO_PANEL_ONLY                = ConvertAbilityRealLevelField('Ncr6')
    constant abilityreallevelfield ABILITY_RLF_LIFE_REGENERATION_RATE_PER_SECOND                 = ConvertAbilityRealLevelField('ave5')
    constant abilityreallevelfield ABILITY_RLF_STUN_DURATION_USL1                                = ConvertAbilityRealLevelField('Usl1')
    constant abilityreallevelfield ABILITY_RLF_ATTACK_DAMAGE_STOLEN_PERCENT                      = ConvertAbilityRealLevelField('Uav1')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_UCS1                                       = ConvertAbilityRealLevelField('Ucs1')
    constant abilityreallevelfield ABILITY_RLF_MAX_DAMAGE_UCS2                                   = ConvertAbilityRealLevelField('Ucs2')
    constant abilityreallevelfield ABILITY_RLF_DISTANCE_UCS3                                     = ConvertAbilityRealLevelField('Ucs3')
    constant abilityreallevelfield ABILITY_RLF_FINAL_AREA_UCS4                                   = ConvertAbilityRealLevelField('Ucs4')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_UIN1                                       = ConvertAbilityRealLevelField('Uin1')
    constant abilityreallevelfield ABILITY_RLF_DURATION                                          = ConvertAbilityRealLevelField('Uin2')
    constant abilityreallevelfield ABILITY_RLF_IMPACT_DELAY                                      = ConvertAbilityRealLevelField('Uin3')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_PER_TARGET_OCL1                            = ConvertAbilityRealLevelField('Ocl1')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_REDUCTION_PER_TARGET                       = ConvertAbilityRealLevelField('Ocl3')
    constant abilityreallevelfield ABILITY_RLF_EFFECT_DELAY_OEQ1                                 = ConvertAbilityRealLevelField('Oeq1')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_PER_SECOND_TO_BUILDINGS                    = ConvertAbilityRealLevelField('Oeq2')
    constant abilityreallevelfield ABILITY_RLF_UNITS_SLOWED_PERCENT                              = ConvertAbilityRealLevelField('Oeq3')
    constant abilityreallevelfield ABILITY_RLF_FINAL_AREA_OEQ4                                   = ConvertAbilityRealLevelField('Oeq4')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_PER_SECOND_EER1                            = ConvertAbilityRealLevelField('Eer1')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_DEALT_TO_ATTACKERS                         = ConvertAbilityRealLevelField('Eah1')
    constant abilityreallevelfield ABILITY_RLF_LIFE_HEALED                                       = ConvertAbilityRealLevelField('Etq1')
    constant abilityreallevelfield ABILITY_RLF_HEAL_INTERVAL                                     = ConvertAbilityRealLevelField('Etq2')
    constant abilityreallevelfield ABILITY_RLF_BUILDING_REDUCTION_ETQ3                           = ConvertAbilityRealLevelField('Etq3')
    constant abilityreallevelfield ABILITY_RLF_INITIAL_IMMUNITY_DURATION                         = ConvertAbilityRealLevelField('Etq4')
    constant abilityreallevelfield ABILITY_RLF_MAX_LIFE_DRAINED_PER_SECOND_PERCENT               = ConvertAbilityRealLevelField('Udd1')
    constant abilityreallevelfield ABILITY_RLF_BUILDING_REDUCTION_UDD2                           = ConvertAbilityRealLevelField('Udd2')
    constant abilityreallevelfield ABILITY_RLF_ARMOR_DURATION                                    = ConvertAbilityRealLevelField('Ufa1')
    constant abilityreallevelfield ABILITY_RLF_ARMOR_BONUS_UFA2                                  = ConvertAbilityRealLevelField('Ufa2')
    constant abilityreallevelfield ABILITY_RLF_AREA_OF_EFFECT_DAMAGE                             = ConvertAbilityRealLevelField('Ufn1')
    constant abilityreallevelfield ABILITY_RLF_SPECIFIC_TARGET_DAMAGE_UFN2                       = ConvertAbilityRealLevelField('Ufn2')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_BONUS_HFA1                                 = ConvertAbilityRealLevelField('Hfa1')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_DEALT_ESF1                                 = ConvertAbilityRealLevelField('Esf1')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_INTERVAL_ESF2                              = ConvertAbilityRealLevelField('Esf2')
    constant abilityreallevelfield ABILITY_RLF_BUILDING_REDUCTION_ESF3                           = ConvertAbilityRealLevelField('Esf3')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_BONUS_PERCENT                              = ConvertAbilityRealLevelField('Ear1')
    constant abilityreallevelfield ABILITY_RLF_DEFENSE_BONUS_HAV1                                = ConvertAbilityRealLevelField('Hav1')
    constant abilityreallevelfield ABILITY_RLF_HIT_POINT_BONUS                                   = ConvertAbilityRealLevelField('Hav2')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_BONUS_HAV3                                 = ConvertAbilityRealLevelField('Hav3')
    constant abilityreallevelfield ABILITY_RLF_MAGIC_DAMAGE_REDUCTION_HAV4                       = ConvertAbilityRealLevelField('Hav4')
    constant abilityreallevelfield ABILITY_RLF_CHANCE_TO_BASH                                    = ConvertAbilityRealLevelField('Hbh1')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_MULTIPLIER_HBH2                            = ConvertAbilityRealLevelField('Hbh2')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_BONUS_HBH3                                 = ConvertAbilityRealLevelField('Hbh3')
    constant abilityreallevelfield ABILITY_RLF_CHANCE_TO_MISS_HBH4                               = ConvertAbilityRealLevelField('Hbh4')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_HTB1                                       = ConvertAbilityRealLevelField('Htb1')
    constant abilityreallevelfield ABILITY_RLF_AOE_DAMAGE                                        = ConvertAbilityRealLevelField('Htc1')
    constant abilityreallevelfield ABILITY_RLF_SPECIFIC_TARGET_DAMAGE_HTC2                       = ConvertAbilityRealLevelField('Htc2')
    constant abilityreallevelfield ABILITY_RLF_MOVEMENT_SPEED_REDUCTION_PERCENT_HTC3             = ConvertAbilityRealLevelField('Htc3')
    constant abilityreallevelfield ABILITY_RLF_ATTACK_SPEED_REDUCTION_PERCENT_HTC4               = ConvertAbilityRealLevelField('Htc4')
    constant abilityreallevelfield ABILITY_RLF_ARMOR_BONUS_HAD1                                  = ConvertAbilityRealLevelField('Had1')
    constant abilityreallevelfield ABILITY_RLF_AMOUNT_HEALED_DAMAGED_HHB1                        = ConvertAbilityRealLevelField('Hhb1')
    constant abilityreallevelfield ABILITY_RLF_EXTRA_DAMAGE_HCA1                                 = ConvertAbilityRealLevelField('Hca1')
    constant abilityreallevelfield ABILITY_RLF_MOVEMENT_SPEED_FACTOR_HCA2                        = ConvertAbilityRealLevelField('Hca2')
    constant abilityreallevelfield ABILITY_RLF_ATTACK_SPEED_FACTOR_HCA3                          = ConvertAbilityRealLevelField('Hca3')
    constant abilityreallevelfield ABILITY_RLF_MOVEMENT_SPEED_INCREASE_PERCENT_OAE1              = ConvertAbilityRealLevelField('Oae1')
    constant abilityreallevelfield ABILITY_RLF_ATTACK_SPEED_INCREASE_PERCENT_OAE2                = ConvertAbilityRealLevelField('Oae2')
    constant abilityreallevelfield ABILITY_RLF_REINCARNATION_DELAY                               = ConvertAbilityRealLevelField('Ore1')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_OSH1                                       = ConvertAbilityRealLevelField('Osh1')
    constant abilityreallevelfield ABILITY_RLF_MAXIMUM_DAMAGE_OSH2                               = ConvertAbilityRealLevelField('Osh2')
    constant abilityreallevelfield ABILITY_RLF_DISTANCE_OSH3                                     = ConvertAbilityRealLevelField('Osh3')
    constant abilityreallevelfield ABILITY_RLF_FINAL_AREA_OSH4                                   = ConvertAbilityRealLevelField('Osh4')
    constant abilityreallevelfield ABILITY_RLF_GRAPHIC_DELAY_NFD1                                = ConvertAbilityRealLevelField('Nfd1')
    constant abilityreallevelfield ABILITY_RLF_GRAPHIC_DURATION_NFD2                             = ConvertAbilityRealLevelField('Nfd2')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_NFD3                                       = ConvertAbilityRealLevelField('Nfd3')
    constant abilityreallevelfield ABILITY_RLF_SUMMONED_UNIT_DAMAGE_AMS1                         = ConvertAbilityRealLevelField('Ams1')
    constant abilityreallevelfield ABILITY_RLF_MAGIC_DAMAGE_REDUCTION_AMS2                       = ConvertAbilityRealLevelField('Ams2')
    constant abilityreallevelfield ABILITY_RLF_AURA_DURATION                                     = ConvertAbilityRealLevelField('Apl1')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_PER_SECOND_APL2                            = ConvertAbilityRealLevelField('Apl2')
    constant abilityreallevelfield ABILITY_RLF_DURATION_OF_PLAGUE_WARD                           = ConvertAbilityRealLevelField('Apl3')
    constant abilityreallevelfield ABILITY_RLF_AMOUNT_OF_HIT_POINTS_REGENERATED                  = ConvertAbilityRealLevelField('Oar1')
    constant abilityreallevelfield ABILITY_RLF_ATTACK_DAMAGE_INCREASE_AKB1                       = ConvertAbilityRealLevelField('Akb1')
    constant abilityreallevelfield ABILITY_RLF_MANA_LOSS_ADM1                                    = ConvertAbilityRealLevelField('Adm1')
    constant abilityreallevelfield ABILITY_RLF_SUMMONED_UNIT_DAMAGE_ADM2                         = ConvertAbilityRealLevelField('Adm2')
    constant abilityreallevelfield ABILITY_RLF_EXPANSION_AMOUNT                                  = ConvertAbilityRealLevelField('Bli1')
    constant abilityreallevelfield ABILITY_RLF_INTERVAL_DURATION_BGM2                            = ConvertAbilityRealLevelField('Bgm2')
    constant abilityreallevelfield ABILITY_RLF_RADIUS_OF_MINING_RING                             = ConvertAbilityRealLevelField('Bgm4')
    constant abilityreallevelfield ABILITY_RLF_ATTACK_SPEED_INCREASE_PERCENT_BLO1                = ConvertAbilityRealLevelField('Blo1')
    constant abilityreallevelfield ABILITY_RLF_MOVEMENT_SPEED_INCREASE_PERCENT_BLO2              = ConvertAbilityRealLevelField('Blo2')
    constant abilityreallevelfield ABILITY_RLF_SCALING_FACTOR                                    = ConvertAbilityRealLevelField('Blo3')
    constant abilityreallevelfield ABILITY_RLF_HIT_POINTS_PER_SECOND_CAN1                        = ConvertAbilityRealLevelField('Can1')
    constant abilityreallevelfield ABILITY_RLF_MAX_HIT_POINTS                                    = ConvertAbilityRealLevelField('Can2')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_PER_SECOND_DEV2                            = ConvertAbilityRealLevelField('Dev2')
    constant abilityreallevelfield ABILITY_RLF_MOVEMENT_UPDATE_FREQUENCY_CHD1                    = ConvertAbilityRealLevelField('Chd1')
    constant abilityreallevelfield ABILITY_RLF_ATTACK_UPDATE_FREQUENCY_CHD2                      = ConvertAbilityRealLevelField('Chd2')
    constant abilityreallevelfield ABILITY_RLF_SUMMONED_UNIT_DAMAGE_CHD3                         = ConvertAbilityRealLevelField('Chd3')
    constant abilityreallevelfield ABILITY_RLF_MOVEMENT_SPEED_REDUCTION_PERCENT_CRI1             = ConvertAbilityRealLevelField('Cri1')
    constant abilityreallevelfield ABILITY_RLF_ATTACK_SPEED_REDUCTION_PERCENT_CRI2               = ConvertAbilityRealLevelField('Cri2')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_REDUCTION_CRI3                             = ConvertAbilityRealLevelField('Cri3')
    constant abilityreallevelfield ABILITY_RLF_CHANCE_TO_MISS_CRS                                = ConvertAbilityRealLevelField('Crs1')
    constant abilityreallevelfield ABILITY_RLF_FULL_DAMAGE_RADIUS_DDA1                           = ConvertAbilityRealLevelField('Dda1')
    constant abilityreallevelfield ABILITY_RLF_FULL_DAMAGE_AMOUNT_DDA2                           = ConvertAbilityRealLevelField('Dda2')
    constant abilityreallevelfield ABILITY_RLF_PARTIAL_DAMAGE_RADIUS                             = ConvertAbilityRealLevelField('Dda3')
    constant abilityreallevelfield ABILITY_RLF_PARTIAL_DAMAGE_AMOUNT                             = ConvertAbilityRealLevelField('Dda4')
    constant abilityreallevelfield ABILITY_RLF_BUILDING_DAMAGE_FACTOR_SDS1                       = ConvertAbilityRealLevelField('Sds1')
    constant abilityreallevelfield ABILITY_RLF_MAX_DAMAGE_UCO5                                   = ConvertAbilityRealLevelField('Uco5')
    constant abilityreallevelfield ABILITY_RLF_MOVE_SPEED_BONUS_UCO6                             = ConvertAbilityRealLevelField('Uco6')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_TAKEN_PERCENT_DEF1                         = ConvertAbilityRealLevelField('Def1')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_DEALT_PERCENT_DEF2                         = ConvertAbilityRealLevelField('Def2')
    constant abilityreallevelfield ABILITY_RLF_MOVEMENT_SPEED_FACTOR_DEF3                        = ConvertAbilityRealLevelField('Def3')
    constant abilityreallevelfield ABILITY_RLF_ATTACK_SPEED_FACTOR_DEF4                          = ConvertAbilityRealLevelField('Def4')
    constant abilityreallevelfield ABILITY_RLF_MAGIC_DAMAGE_REDUCTION_DEF5                       = ConvertAbilityRealLevelField('Def5')
    constant abilityreallevelfield ABILITY_RLF_CHANCE_TO_DEFLECT                                 = ConvertAbilityRealLevelField('Def6')
    constant abilityreallevelfield ABILITY_RLF_DEFLECT_DAMAGE_TAKEN_PIERCING                     = ConvertAbilityRealLevelField('Def7')
    constant abilityreallevelfield ABILITY_RLF_DEFLECT_DAMAGE_TAKEN_SPELLS                       = ConvertAbilityRealLevelField('Def8')
    constant abilityreallevelfield ABILITY_RLF_RIP_DELAY                                         = ConvertAbilityRealLevelField('Eat1')
    constant abilityreallevelfield ABILITY_RLF_EAT_DELAY                                         = ConvertAbilityRealLevelField('Eat2')
    constant abilityreallevelfield ABILITY_RLF_HIT_POINTS_GAINED_EAT3                            = ConvertAbilityRealLevelField('Eat3')
    constant abilityreallevelfield ABILITY_RLF_AIR_UNIT_LOWER_DURATION                           = ConvertAbilityRealLevelField('Ens1')
    constant abilityreallevelfield ABILITY_RLF_AIR_UNIT_HEIGHT                                   = ConvertAbilityRealLevelField('Ens2')
    constant abilityreallevelfield ABILITY_RLF_MELEE_ATTACK_RANGE                                = ConvertAbilityRealLevelField('Ens3')
    constant abilityreallevelfield ABILITY_RLF_INTERVAL_DURATION_EGM2                            = ConvertAbilityRealLevelField('Egm2')
    constant abilityreallevelfield ABILITY_RLF_EFFECT_DELAY_FLA2                                 = ConvertAbilityRealLevelField('Fla2')
    constant abilityreallevelfield ABILITY_RLF_MINING_DURATION                                   = ConvertAbilityRealLevelField('Gld2')
    constant abilityreallevelfield ABILITY_RLF_RADIUS_OF_GRAVESTONES                             = ConvertAbilityRealLevelField('Gyd2')
    constant abilityreallevelfield ABILITY_RLF_RADIUS_OF_CORPSES                                 = ConvertAbilityRealLevelField('Gyd3')
    constant abilityreallevelfield ABILITY_RLF_HIT_POINTS_GAINED_HEA1                            = ConvertAbilityRealLevelField('Hea1')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_INCREASE_PERCENT_INF1                      = ConvertAbilityRealLevelField('Inf1')
    constant abilityreallevelfield ABILITY_RLF_AUTOCAST_RANGE                                    = ConvertAbilityRealLevelField('Inf3')
    constant abilityreallevelfield ABILITY_RLF_LIFE_REGEN_RATE                                   = ConvertAbilityRealLevelField('Inf4')
    constant abilityreallevelfield ABILITY_RLF_GRAPHIC_DELAY_LIT1                                = ConvertAbilityRealLevelField('Lit1')
    constant abilityreallevelfield ABILITY_RLF_GRAPHIC_DURATION_LIT2                             = ConvertAbilityRealLevelField('Lit2')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_PER_SECOND_LSH1                            = ConvertAbilityRealLevelField('Lsh1')
    constant abilityreallevelfield ABILITY_RLF_MANA_GAINED                                       = ConvertAbilityRealLevelField('Mbt1')
    constant abilityreallevelfield ABILITY_RLF_HIT_POINTS_GAINED_MBT2                            = ConvertAbilityRealLevelField('Mbt2')
    constant abilityreallevelfield ABILITY_RLF_AUTOCAST_REQUIREMENT                              = ConvertAbilityRealLevelField('Mbt3')
    constant abilityreallevelfield ABILITY_RLF_WATER_HEIGHT                                      = ConvertAbilityRealLevelField('Mbt4')
    constant abilityreallevelfield ABILITY_RLF_ACTIVATION_DELAY_MIN1                             = ConvertAbilityRealLevelField('Min1')
    constant abilityreallevelfield ABILITY_RLF_INVISIBILITY_TRANSITION_TIME                      = ConvertAbilityRealLevelField('Min2')
    constant abilityreallevelfield ABILITY_RLF_ACTIVATION_RADIUS                                 = ConvertAbilityRealLevelField('Neu1')
    constant abilityreallevelfield ABILITY_RLF_AMOUNT_REGENERATED                                = ConvertAbilityRealLevelField('Arm1')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_PER_SECOND_POI1                            = ConvertAbilityRealLevelField('Poi1')
    constant abilityreallevelfield ABILITY_RLF_ATTACK_SPEED_FACTOR_POI2                          = ConvertAbilityRealLevelField('Poi2')
    constant abilityreallevelfield ABILITY_RLF_MOVEMENT_SPEED_FACTOR_POI3                        = ConvertAbilityRealLevelField('Poi3')
    constant abilityreallevelfield ABILITY_RLF_EXTRA_DAMAGE_POA1                                 = ConvertAbilityRealLevelField('Poa1')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_PER_SECOND_POA2                            = ConvertAbilityRealLevelField('Poa2')
    constant abilityreallevelfield ABILITY_RLF_ATTACK_SPEED_FACTOR_POA3                          = ConvertAbilityRealLevelField('Poa3')
    constant abilityreallevelfield ABILITY_RLF_MOVEMENT_SPEED_FACTOR_POA4                        = ConvertAbilityRealLevelField('Poa4')   
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_AMPLIFICATION                              = ConvertAbilityRealLevelField('Pos2')
    constant abilityreallevelfield ABILITY_RLF_CHANCE_TO_STOMP_PERCENT                           = ConvertAbilityRealLevelField('War1')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_DEALT_WAR2                                 = ConvertAbilityRealLevelField('War2')
    constant abilityreallevelfield ABILITY_RLF_FULL_DAMAGE_RADIUS_WAR3                           = ConvertAbilityRealLevelField('War3')
    constant abilityreallevelfield ABILITY_RLF_HALF_DAMAGE_RADIUS_WAR4                           = ConvertAbilityRealLevelField('War4')
    constant abilityreallevelfield ABILITY_RLF_SUMMONED_UNIT_DAMAGE_PRG3                         = ConvertAbilityRealLevelField('Prg3')
    constant abilityreallevelfield ABILITY_RLF_UNIT_PAUSE_DURATION                               = ConvertAbilityRealLevelField('Prg4')
    constant abilityreallevelfield ABILITY_RLF_HERO_PAUSE_DURATION                               = ConvertAbilityRealLevelField('Prg5')
    constant abilityreallevelfield ABILITY_RLF_HIT_POINTS_GAINED_REJ1                            = ConvertAbilityRealLevelField('Rej1')
    constant abilityreallevelfield ABILITY_RLF_MANA_POINTS_GAINED_REJ2                           = ConvertAbilityRealLevelField('Rej2')
    constant abilityreallevelfield ABILITY_RLF_MINIMUM_LIFE_REQUIRED                             = ConvertAbilityRealLevelField('Rpb3')
    constant abilityreallevelfield ABILITY_RLF_MINIMUM_MANA_REQUIRED                             = ConvertAbilityRealLevelField('Rpb4')
    constant abilityreallevelfield ABILITY_RLF_REPAIR_COST_RATIO                                 = ConvertAbilityRealLevelField('Rep1')
    constant abilityreallevelfield ABILITY_RLF_REPAIR_TIME_RATIO                                 = ConvertAbilityRealLevelField('Rep2')
    constant abilityreallevelfield ABILITY_RLF_POWERBUILD_COST                                   = ConvertAbilityRealLevelField('Rep3')
    constant abilityreallevelfield ABILITY_RLF_POWERBUILD_RATE                                   = ConvertAbilityRealLevelField('Rep4')
    constant abilityreallevelfield ABILITY_RLF_NAVAL_RANGE_BONUS                                 = ConvertAbilityRealLevelField('Rep5')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_INCREASE_PERCENT_ROA1                      = ConvertAbilityRealLevelField('Roa1')
    constant abilityreallevelfield ABILITY_RLF_LIFE_REGENERATION_RATE                            = ConvertAbilityRealLevelField('Roa3')
    constant abilityreallevelfield ABILITY_RLF_MANA_REGEN                                        = ConvertAbilityRealLevelField('Roa4')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_INCREASE                                   = ConvertAbilityRealLevelField('Nbr1')
    constant abilityreallevelfield ABILITY_RLF_SALVAGE_COST_RATIO                                = ConvertAbilityRealLevelField('Sal1')
    constant abilityreallevelfield ABILITY_RLF_IN_FLIGHT_SIGHT_RADIUS                            = ConvertAbilityRealLevelField('Esn1')
    constant abilityreallevelfield ABILITY_RLF_HOVERING_SIGHT_RADIUS                             = ConvertAbilityRealLevelField('Esn2')
    constant abilityreallevelfield ABILITY_RLF_HOVERING_HEIGHT                                   = ConvertAbilityRealLevelField('Esn3')
    constant abilityreallevelfield ABILITY_RLF_DURATION_OF_OWLS                                  = ConvertAbilityRealLevelField('Esn5')
    constant abilityreallevelfield ABILITY_RLF_FADE_DURATION                                     = ConvertAbilityRealLevelField('Shm1')
    constant abilityreallevelfield ABILITY_RLF_DAY_NIGHT_DURATION                                = ConvertAbilityRealLevelField('Shm2')
    constant abilityreallevelfield ABILITY_RLF_ACTION_DURATION                                   = ConvertAbilityRealLevelField('Shm3')
    constant abilityreallevelfield ABILITY_RLF_MOVEMENT_SPEED_FACTOR_SLO1                        = ConvertAbilityRealLevelField('Slo1')
    constant abilityreallevelfield ABILITY_RLF_ATTACK_SPEED_FACTOR_SLO2                          = ConvertAbilityRealLevelField('Slo2')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_PER_SECOND_SPO1                            = ConvertAbilityRealLevelField('Spo1')
    constant abilityreallevelfield ABILITY_RLF_MOVEMENT_SPEED_FACTOR_SPO2                        = ConvertAbilityRealLevelField('Spo2')
    constant abilityreallevelfield ABILITY_RLF_ATTACK_SPEED_FACTOR_SPO3                          = ConvertAbilityRealLevelField('Spo3')
    constant abilityreallevelfield ABILITY_RLF_ACTIVATION_DELAY_STA1                             = ConvertAbilityRealLevelField('Sta1')
    constant abilityreallevelfield ABILITY_RLF_DETECTION_RADIUS_STA2                             = ConvertAbilityRealLevelField('Sta2')
    constant abilityreallevelfield ABILITY_RLF_DETONATION_RADIUS                                 = ConvertAbilityRealLevelField('Sta3')
    constant abilityreallevelfield ABILITY_RLF_STUN_DURATION_STA4                                = ConvertAbilityRealLevelField('Sta4')
    constant abilityreallevelfield ABILITY_RLF_ATTACK_SPEED_BONUS_PERCENT                        = ConvertAbilityRealLevelField('Uhf1')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_PER_SECOND_UHF2                            = ConvertAbilityRealLevelField('Uhf2')
    constant abilityreallevelfield ABILITY_RLF_LUMBER_PER_INTERVAL                               = ConvertAbilityRealLevelField('Wha1')
    constant abilityreallevelfield ABILITY_RLF_ART_ATTACHMENT_HEIGHT                             = ConvertAbilityRealLevelField('Wha3')
    constant abilityreallevelfield ABILITY_RLF_TELEPORT_AREA_WIDTH                               = ConvertAbilityRealLevelField('Wrp1')
    constant abilityreallevelfield ABILITY_RLF_TELEPORT_AREA_HEIGHT                              = ConvertAbilityRealLevelField('Wrp2')
    constant abilityreallevelfield ABILITY_RLF_LIFE_STOLEN_PER_ATTACK                            = ConvertAbilityRealLevelField('Ivam')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_BONUS_IDAM                                 = ConvertAbilityRealLevelField('Idam')
    constant abilityreallevelfield ABILITY_RLF_CHANCE_TO_HIT_UNITS_PERCENT                       = ConvertAbilityRealLevelField('Iob2')
    constant abilityreallevelfield ABILITY_RLF_CHANCE_TO_HIT_HEROS_PERCENT                       = ConvertAbilityRealLevelField('Iob3')
    constant abilityreallevelfield ABILITY_RLF_CHANCE_TO_HIT_SUMMONS_PERCENT                     = ConvertAbilityRealLevelField('Iob4')
    constant abilityreallevelfield ABILITY_RLF_DELAY_FOR_TARGET_EFFECT                           = ConvertAbilityRealLevelField('Idel')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_DEALT_PERCENT_OF_NORMAL                    = ConvertAbilityRealLevelField('Iild')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_RECEIVED_MULTIPLIER                        = ConvertAbilityRealLevelField('Iilw')
    constant abilityreallevelfield ABILITY_RLF_MANA_REGENERATION_BONUS_AS_FRACTION_OF_NORMAL     = ConvertAbilityRealLevelField('Imrp')
    constant abilityreallevelfield ABILITY_RLF_MOVEMENT_SPEED_INCREASE_ISPI                      = ConvertAbilityRealLevelField('Ispi')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_PER_SECOND_IDPS                            = ConvertAbilityRealLevelField('Idps')
    constant abilityreallevelfield ABILITY_RLF_ATTACK_DAMAGE_INCREASE_CAC1                       = ConvertAbilityRealLevelField('Cac1')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_PER_SECOND_COR1                            = ConvertAbilityRealLevelField('Cor1')
    constant abilityreallevelfield ABILITY_RLF_ATTACK_SPEED_INCREASE_ISX1                        = ConvertAbilityRealLevelField('Isx1')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_WRS1                                       = ConvertAbilityRealLevelField('Wrs1')
    constant abilityreallevelfield ABILITY_RLF_TERRAIN_DEFORMATION_AMPLITUDE                     = ConvertAbilityRealLevelField('Wrs2')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_CTC1                                       = ConvertAbilityRealLevelField('Ctc1')
    constant abilityreallevelfield ABILITY_RLF_EXTRA_DAMAGE_TO_TARGET                            = ConvertAbilityRealLevelField('Ctc2')
    constant abilityreallevelfield ABILITY_RLF_MOVEMENT_SPEED_REDUCTION_CTC3                     = ConvertAbilityRealLevelField('Ctc3')
    constant abilityreallevelfield ABILITY_RLF_ATTACK_SPEED_REDUCTION_CTC4                       = ConvertAbilityRealLevelField('Ctc4')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_CTB1                                       = ConvertAbilityRealLevelField('Ctb1')
    constant abilityreallevelfield ABILITY_RLF_CASTING_DELAY_SECONDS                             = ConvertAbilityRealLevelField('Uds2')
    constant abilityreallevelfield ABILITY_RLF_MANA_LOSS_PER_UNIT_DTN1                           = ConvertAbilityRealLevelField('Dtn1')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_TO_SUMMONED_UNITS_DTN2                     = ConvertAbilityRealLevelField('Dtn2')
    constant abilityreallevelfield ABILITY_RLF_TRANSITION_TIME_SECONDS                           = ConvertAbilityRealLevelField('Ivs1')
    constant abilityreallevelfield ABILITY_RLF_MANA_DRAINED_PER_SECOND_NMR1                      = ConvertAbilityRealLevelField('Nmr1')
    constant abilityreallevelfield ABILITY_RLF_CHANCE_TO_REDUCE_DAMAGE_PERCENT                   = ConvertAbilityRealLevelField('Ssk1')
    constant abilityreallevelfield ABILITY_RLF_MINIMUM_DAMAGE                                    = ConvertAbilityRealLevelField('Ssk2')
    constant abilityreallevelfield ABILITY_RLF_IGNORED_DAMAGE                                    = ConvertAbilityRealLevelField('Ssk3')
    constant abilityreallevelfield ABILITY_RLF_FULL_DAMAGE_DEALT                                 = ConvertAbilityRealLevelField('Hfs1')
    constant abilityreallevelfield ABILITY_RLF_FULL_DAMAGE_INTERVAL                              = ConvertAbilityRealLevelField('Hfs2')
    constant abilityreallevelfield ABILITY_RLF_HALF_DAMAGE_DEALT                                 = ConvertAbilityRealLevelField('Hfs3')
    constant abilityreallevelfield ABILITY_RLF_HALF_DAMAGE_INTERVAL                              = ConvertAbilityRealLevelField('Hfs4')
    constant abilityreallevelfield ABILITY_RLF_BUILDING_REDUCTION_HFS5                           = ConvertAbilityRealLevelField('Hfs5')
    constant abilityreallevelfield ABILITY_RLF_MAXIMUM_DAMAGE_HFS6                               = ConvertAbilityRealLevelField('Hfs6')
    constant abilityreallevelfield ABILITY_RLF_MANA_PER_HIT_POINT                                = ConvertAbilityRealLevelField('Nms1')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_ABSORBED_PERCENT                           = ConvertAbilityRealLevelField('Nms2')
    constant abilityreallevelfield ABILITY_RLF_WAVE_DISTANCE                                     = ConvertAbilityRealLevelField('Uim1')
    constant abilityreallevelfield ABILITY_RLF_WAVE_TIME_SECONDS                                 = ConvertAbilityRealLevelField('Uim2')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_DEALT_UIM3                                 = ConvertAbilityRealLevelField('Uim3')
    constant abilityreallevelfield ABILITY_RLF_AIR_TIME_SECONDS_UIM4                             = ConvertAbilityRealLevelField('Uim4')
    constant abilityreallevelfield ABILITY_RLF_UNIT_RELEASE_INTERVAL_SECONDS                     = ConvertAbilityRealLevelField('Uls2')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_RETURN_FACTOR                              = ConvertAbilityRealLevelField('Uls4')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_RETURN_THRESHOLD                           = ConvertAbilityRealLevelField('Uls5')
    constant abilityreallevelfield ABILITY_RLF_RETURNED_DAMAGE_FACTOR                            = ConvertAbilityRealLevelField('Uts1')
    constant abilityreallevelfield ABILITY_RLF_RECEIVED_DAMAGE_FACTOR                            = ConvertAbilityRealLevelField('Uts2')
    constant abilityreallevelfield ABILITY_RLF_DEFENSE_BONUS_UTS3                                = ConvertAbilityRealLevelField('Uts3')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_BONUS_NBA1                                 = ConvertAbilityRealLevelField('Nba1')
    constant abilityreallevelfield ABILITY_RLF_SUMMONED_UNIT_DURATION_SECONDS_NBA3               = ConvertAbilityRealLevelField('Nba3')
    constant abilityreallevelfield ABILITY_RLF_MANA_PER_SUMMONED_HITPOINT                        = ConvertAbilityRealLevelField('Cmg2')
    constant abilityreallevelfield ABILITY_RLF_CHARGE_FOR_CURRENT_LIFE                           = ConvertAbilityRealLevelField('Cmg3')
    constant abilityreallevelfield ABILITY_RLF_HIT_POINTS_DRAINED                                = ConvertAbilityRealLevelField('Ndr1')
    constant abilityreallevelfield ABILITY_RLF_MANA_POINTS_DRAINED                               = ConvertAbilityRealLevelField('Ndr2')
    constant abilityreallevelfield ABILITY_RLF_DRAIN_INTERVAL_SECONDS                            = ConvertAbilityRealLevelField('Ndr3')
    constant abilityreallevelfield ABILITY_RLF_LIFE_TRANSFERRED_PER_SECOND                       = ConvertAbilityRealLevelField('Ndr4')
    constant abilityreallevelfield ABILITY_RLF_MANA_TRANSFERRED_PER_SECOND                       = ConvertAbilityRealLevelField('Ndr5')
    constant abilityreallevelfield ABILITY_RLF_BONUS_LIFE_FACTOR                                 = ConvertAbilityRealLevelField('Ndr6')
    constant abilityreallevelfield ABILITY_RLF_BONUS_LIFE_DECAY                                  = ConvertAbilityRealLevelField('Ndr7')
    constant abilityreallevelfield ABILITY_RLF_BONUS_MANA_FACTOR                                 = ConvertAbilityRealLevelField('Ndr8')
    constant abilityreallevelfield ABILITY_RLF_BONUS_MANA_DECAY                                  = ConvertAbilityRealLevelField('Ndr9')
    constant abilityreallevelfield ABILITY_RLF_CHANCE_TO_MISS_PERCENT                            = ConvertAbilityRealLevelField('Nsi2')
    constant abilityreallevelfield ABILITY_RLF_MOVEMENT_SPEED_MODIFIER                           = ConvertAbilityRealLevelField('Nsi3')
    constant abilityreallevelfield ABILITY_RLF_ATTACK_SPEED_MODIFIER                             = ConvertAbilityRealLevelField('Nsi4')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_PER_SECOND_TDG1                            = ConvertAbilityRealLevelField('Tdg1')
    constant abilityreallevelfield ABILITY_RLF_MEDIUM_DAMAGE_RADIUS_TDG2                         = ConvertAbilityRealLevelField('Tdg2')
    constant abilityreallevelfield ABILITY_RLF_MEDIUM_DAMAGE_PER_SECOND                          = ConvertAbilityRealLevelField('Tdg3')
    constant abilityreallevelfield ABILITY_RLF_SMALL_DAMAGE_RADIUS_TDG4                          = ConvertAbilityRealLevelField('Tdg4')
    constant abilityreallevelfield ABILITY_RLF_SMALL_DAMAGE_PER_SECOND                           = ConvertAbilityRealLevelField('Tdg5')
    constant abilityreallevelfield ABILITY_RLF_AIR_TIME_SECONDS_TSP1                             = ConvertAbilityRealLevelField('Tsp1')
    constant abilityreallevelfield ABILITY_RLF_MINIMUM_HIT_INTERVAL_SECONDS                      = ConvertAbilityRealLevelField('Tsp2')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_PER_SECOND_NBF5                            = ConvertAbilityRealLevelField('Nbf5')
    constant abilityreallevelfield ABILITY_RLF_MAXIMUM_RANGE                                     = ConvertAbilityRealLevelField('Ebl1')
    constant abilityreallevelfield ABILITY_RLF_MINIMUM_RANGE                                     = ConvertAbilityRealLevelField('Ebl2')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_PER_TARGET_EFK1                            = ConvertAbilityRealLevelField('Efk1')
    constant abilityreallevelfield ABILITY_RLF_MAXIMUM_TOTAL_DAMAGE                              = ConvertAbilityRealLevelField('Efk2')
    constant abilityreallevelfield ABILITY_RLF_MAXIMUM_SPEED_ADJUSTMENT                          = ConvertAbilityRealLevelField('Efk4')
    constant abilityreallevelfield ABILITY_RLF_DECAYING_DAMAGE                                   = ConvertAbilityRealLevelField('Esh1')
    constant abilityreallevelfield ABILITY_RLF_MOVEMENT_SPEED_FACTOR_ESH2                        = ConvertAbilityRealLevelField('Esh2')
    constant abilityreallevelfield ABILITY_RLF_ATTACK_SPEED_FACTOR_ESH3                          = ConvertAbilityRealLevelField('Esh3')
    constant abilityreallevelfield ABILITY_RLF_DECAY_POWER                                       = ConvertAbilityRealLevelField('Esh4')
    constant abilityreallevelfield ABILITY_RLF_INITIAL_DAMAGE_ESH5                               = ConvertAbilityRealLevelField('Esh5')
    constant abilityreallevelfield ABILITY_RLF_MAXIMUM_LIFE_ABSORBED                             = ConvertAbilityRealLevelField('abs1')
    constant abilityreallevelfield ABILITY_RLF_MAXIMUM_MANA_ABSORBED                             = ConvertAbilityRealLevelField('abs2')
    constant abilityreallevelfield ABILITY_RLF_MOVEMENT_SPEED_INCREASE_BSK1                      = ConvertAbilityRealLevelField('bsk1')
    constant abilityreallevelfield ABILITY_RLF_ATTACK_SPEED_INCREASE_BSK2                        = ConvertAbilityRealLevelField('bsk2')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_TAKEN_INCREASE                             = ConvertAbilityRealLevelField('bsk3')
    constant abilityreallevelfield ABILITY_RLF_LIFE_PER_UNIT                                     = ConvertAbilityRealLevelField('dvm1')
    constant abilityreallevelfield ABILITY_RLF_MANA_PER_UNIT                                     = ConvertAbilityRealLevelField('dvm2')
    constant abilityreallevelfield ABILITY_RLF_LIFE_PER_BUFF                                     = ConvertAbilityRealLevelField('dvm3')
    constant abilityreallevelfield ABILITY_RLF_MANA_PER_BUFF                                     = ConvertAbilityRealLevelField('dvm4')
    constant abilityreallevelfield ABILITY_RLF_SUMMONED_UNIT_DAMAGE_DVM5                         = ConvertAbilityRealLevelField('dvm5')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_BONUS_FAK1                                 = ConvertAbilityRealLevelField('fak1')
    constant abilityreallevelfield ABILITY_RLF_MEDIUM_DAMAGE_FACTOR_FAK2                         = ConvertAbilityRealLevelField('fak2')
    constant abilityreallevelfield ABILITY_RLF_SMALL_DAMAGE_FACTOR_FAK3                          = ConvertAbilityRealLevelField('fak3')
    constant abilityreallevelfield ABILITY_RLF_FULL_DAMAGE_RADIUS_FAK4                           = ConvertAbilityRealLevelField('fak4')
    constant abilityreallevelfield ABILITY_RLF_HALF_DAMAGE_RADIUS_FAK5                           = ConvertAbilityRealLevelField('fak5')
    constant abilityreallevelfield ABILITY_RLF_EXTRA_DAMAGE_PER_SECOND                           = ConvertAbilityRealLevelField('liq1')
    constant abilityreallevelfield ABILITY_RLF_MOVEMENT_SPEED_REDUCTION_LIQ2                     = ConvertAbilityRealLevelField('liq2')
    constant abilityreallevelfield ABILITY_RLF_ATTACK_SPEED_REDUCTION_LIQ3                       = ConvertAbilityRealLevelField('liq3')
    constant abilityreallevelfield ABILITY_RLF_MAGIC_DAMAGE_FACTOR                               = ConvertAbilityRealLevelField('mim1')
    constant abilityreallevelfield ABILITY_RLF_UNIT_DAMAGE_PER_MANA_POINT                        = ConvertAbilityRealLevelField('mfl1')
    constant abilityreallevelfield ABILITY_RLF_HERO_DAMAGE_PER_MANA_POINT                        = ConvertAbilityRealLevelField('mfl2')
    constant abilityreallevelfield ABILITY_RLF_UNIT_MAXIMUM_DAMAGE                               = ConvertAbilityRealLevelField('mfl3')
    constant abilityreallevelfield ABILITY_RLF_HERO_MAXIMUM_DAMAGE                               = ConvertAbilityRealLevelField('mfl4')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_COOLDOWN                                   = ConvertAbilityRealLevelField('mfl5')
    constant abilityreallevelfield ABILITY_RLF_DISTRIBUTED_DAMAGE_FACTOR_SPL1                    = ConvertAbilityRealLevelField('spl1')
    constant abilityreallevelfield ABILITY_RLF_LIFE_REGENERATED                                  = ConvertAbilityRealLevelField('irl1')
    constant abilityreallevelfield ABILITY_RLF_MANA_REGENERATED                                  = ConvertAbilityRealLevelField('irl2')
    constant abilityreallevelfield ABILITY_RLF_MANA_LOSS_PER_UNIT_IDC1                           = ConvertAbilityRealLevelField('idc1')
    constant abilityreallevelfield ABILITY_RLF_SUMMONED_UNIT_DAMAGE_IDC2                         = ConvertAbilityRealLevelField('idc2')
    constant abilityreallevelfield ABILITY_RLF_ACTIVATION_DELAY_IMO2                             = ConvertAbilityRealLevelField('imo2')
    constant abilityreallevelfield ABILITY_RLF_LURE_INTERVAL_SECONDS                             = ConvertAbilityRealLevelField('imo3')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_BONUS_ISR1                                 = ConvertAbilityRealLevelField('isr1')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_REDUCTION_ISR2                             = ConvertAbilityRealLevelField('isr2')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_BONUS_IPV1                                 = ConvertAbilityRealLevelField('ipv1')
    constant abilityreallevelfield ABILITY_RLF_LIFE_STEAL_AMOUNT                                 = ConvertAbilityRealLevelField('ipv2')
    constant abilityreallevelfield ABILITY_RLF_LIFE_RESTORED_FACTOR                              = ConvertAbilityRealLevelField('ast1')
    constant abilityreallevelfield ABILITY_RLF_MANA_RESTORED_FACTOR                              = ConvertAbilityRealLevelField('ast2')
    constant abilityreallevelfield ABILITY_RLF_ATTACH_DELAY                                      = ConvertAbilityRealLevelField('gra1')
    constant abilityreallevelfield ABILITY_RLF_REMOVE_DELAY                                      = ConvertAbilityRealLevelField('gra2')
    constant abilityreallevelfield ABILITY_RLF_HERO_REGENERATION_DELAY                           = ConvertAbilityRealLevelField('Nsa2')
    constant abilityreallevelfield ABILITY_RLF_UNIT_REGENERATION_DELAY                           = ConvertAbilityRealLevelField('Nsa3')
    constant abilityreallevelfield ABILITY_RLF_MAGIC_DAMAGE_REDUCTION_NSA4                       = ConvertAbilityRealLevelField('Nsa4')
    constant abilityreallevelfield ABILITY_RLF_HIT_POINTS_PER_SECOND_NSA5                        = ConvertAbilityRealLevelField('Nsa5')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_TO_SUMMONED_UNITS_IXS1                     = ConvertAbilityRealLevelField('Ixs1')
    constant abilityreallevelfield ABILITY_RLF_MAGIC_DAMAGE_REDUCTION_IXS2                       = ConvertAbilityRealLevelField('Ixs2')
    constant abilityreallevelfield ABILITY_RLF_SUMMONED_UNIT_DURATION                            = ConvertAbilityRealLevelField('Npa6')
    constant abilityreallevelfield ABILITY_RLF_SHIELD_COOLDOWN_TIME                              = ConvertAbilityRealLevelField('Nse1')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_PER_SECOND_NDO1                            = ConvertAbilityRealLevelField('Ndo1')
    constant abilityreallevelfield ABILITY_RLF_SUMMONED_UNIT_DURATION_SECONDS_NDO3               = ConvertAbilityRealLevelField('Ndo3')
    constant abilityreallevelfield ABILITY_RLF_MEDIUM_DAMAGE_RADIUS_FLK1                         = ConvertAbilityRealLevelField('flk1')
    constant abilityreallevelfield ABILITY_RLF_SMALL_DAMAGE_RADIUS_FLK2                          = ConvertAbilityRealLevelField('flk2')
    constant abilityreallevelfield ABILITY_RLF_FULL_DAMAGE_AMOUNT_FLK3                           = ConvertAbilityRealLevelField('flk3')
    constant abilityreallevelfield ABILITY_RLF_MEDIUM_DAMAGE_AMOUNT                              = ConvertAbilityRealLevelField('flk4')
    constant abilityreallevelfield ABILITY_RLF_SMALL_DAMAGE_AMOUNT                               = ConvertAbilityRealLevelField('flk5')
    constant abilityreallevelfield ABILITY_RLF_MOVEMENT_SPEED_REDUCTION_PERCENT_HBN1             = ConvertAbilityRealLevelField('Hbn1')
    constant abilityreallevelfield ABILITY_RLF_ATTACK_SPEED_REDUCTION_PERCENT_HBN2               = ConvertAbilityRealLevelField('Hbn2')
    constant abilityreallevelfield ABILITY_RLF_MAX_MANA_DRAINED_UNITS                            = ConvertAbilityRealLevelField('fbk1')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_RATIO_UNITS_PERCENT                        = ConvertAbilityRealLevelField('fbk2')
    constant abilityreallevelfield ABILITY_RLF_MAX_MANA_DRAINED_HEROS                            = ConvertAbilityRealLevelField('fbk3')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_RATIO_HEROS_PERCENT                        = ConvertAbilityRealLevelField('fbk4')
    constant abilityreallevelfield ABILITY_RLF_SUMMONED_DAMAGE                                   = ConvertAbilityRealLevelField('fbk5')
    constant abilityreallevelfield ABILITY_RLF_DISTRIBUTED_DAMAGE_FACTOR_NCA1                    = ConvertAbilityRealLevelField('nca1')
    constant abilityreallevelfield ABILITY_RLF_INITIAL_DAMAGE_PXF1                               = ConvertAbilityRealLevelField('pxf1')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_PER_SECOND_PXF2                            = ConvertAbilityRealLevelField('pxf2')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_PER_SECOND_MLS1                            = ConvertAbilityRealLevelField('mls1')
    constant abilityreallevelfield ABILITY_RLF_BEAST_COLLISION_RADIUS                            = ConvertAbilityRealLevelField('Nst2')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_AMOUNT_NST3                                = ConvertAbilityRealLevelField('Nst3')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_RADIUS                                     = ConvertAbilityRealLevelField('Nst4')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_DELAY                                      = ConvertAbilityRealLevelField('Nst5')
    constant abilityreallevelfield ABILITY_RLF_FOLLOW_THROUGH_TIME                               = ConvertAbilityRealLevelField('Ncl1')
    constant abilityreallevelfield ABILITY_RLF_ART_DURATION                                      = ConvertAbilityRealLevelField('Ncl4')
    constant abilityreallevelfield ABILITY_RLF_MOVEMENT_SPEED_REDUCTION_PERCENT_NAB1             = ConvertAbilityRealLevelField('Nab1')
    constant abilityreallevelfield ABILITY_RLF_ATTACK_SPEED_REDUCTION_PERCENT_NAB2               = ConvertAbilityRealLevelField('Nab2')
    constant abilityreallevelfield ABILITY_RLF_PRIMARY_DAMAGE                                    = ConvertAbilityRealLevelField('Nab4')
    constant abilityreallevelfield ABILITY_RLF_SECONDARY_DAMAGE                                  = ConvertAbilityRealLevelField('Nab5')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_INTERVAL_NAB6                              = ConvertAbilityRealLevelField('Nab6')
    constant abilityreallevelfield ABILITY_RLF_GOLD_COST_FACTOR                                  = ConvertAbilityRealLevelField('Ntm1')
    constant abilityreallevelfield ABILITY_RLF_LUMBER_COST_FACTOR                                = ConvertAbilityRealLevelField('Ntm2')
    constant abilityreallevelfield ABILITY_RLF_MOVE_SPEED_BONUS_NEG1                             = ConvertAbilityRealLevelField('Neg1')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_BONUS_NEG2                                 = ConvertAbilityRealLevelField('Neg2')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_AMOUNT_NCS1                                = ConvertAbilityRealLevelField('Ncs1')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_INTERVAL_NCS2                              = ConvertAbilityRealLevelField('Ncs2')
    constant abilityreallevelfield ABILITY_RLF_MAX_DAMAGE_NCS4                                   = ConvertAbilityRealLevelField('Ncs4')
    constant abilityreallevelfield ABILITY_RLF_BUILDING_DAMAGE_FACTOR_NCS5                       = ConvertAbilityRealLevelField('Ncs5')
    constant abilityreallevelfield ABILITY_RLF_EFFECT_DURATION                                   = ConvertAbilityRealLevelField('Ncs6')
    constant abilityreallevelfield ABILITY_RLF_SPAWN_INTERVAL_NSY1                               = ConvertAbilityRealLevelField('Nsy1')
    constant abilityreallevelfield ABILITY_RLF_SPAWN_UNIT_DURATION                               = ConvertAbilityRealLevelField('Nsy3')
    constant abilityreallevelfield ABILITY_RLF_SPAWN_UNIT_OFFSET                                 = ConvertAbilityRealLevelField('Nsy4')
    constant abilityreallevelfield ABILITY_RLF_LEASH_RANGE_NSY5                                  = ConvertAbilityRealLevelField('Nsy5')
    constant abilityreallevelfield ABILITY_RLF_SPAWN_INTERVAL_NFY1                               = ConvertAbilityRealLevelField('Nfy1')
    constant abilityreallevelfield ABILITY_RLF_LEASH_RANGE_NFY2                                  = ConvertAbilityRealLevelField('Nfy2')
    constant abilityreallevelfield ABILITY_RLF_CHANCE_TO_DEMOLISH                                = ConvertAbilityRealLevelField('Nde1')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_MULTIPLIER_BUILDINGS                       = ConvertAbilityRealLevelField('Nde2')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_MULTIPLIER_UNITS                           = ConvertAbilityRealLevelField('Nde3')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_MULTIPLIER_HEROES                          = ConvertAbilityRealLevelField('Nde4')
    constant abilityreallevelfield ABILITY_RLF_BONUS_DAMAGE_MULTIPLIER                           = ConvertAbilityRealLevelField('Nic1')
    constant abilityreallevelfield ABILITY_RLF_DEATH_DAMAGE_FULL_AMOUNT                          = ConvertAbilityRealLevelField('Nic2')
    constant abilityreallevelfield ABILITY_RLF_DEATH_DAMAGE_FULL_AREA                            = ConvertAbilityRealLevelField('Nic3')
    constant abilityreallevelfield ABILITY_RLF_DEATH_DAMAGE_HALF_AMOUNT                          = ConvertAbilityRealLevelField('Nic4')
    constant abilityreallevelfield ABILITY_RLF_DEATH_DAMAGE_HALF_AREA                            = ConvertAbilityRealLevelField('Nic5')
    constant abilityreallevelfield ABILITY_RLF_DEATH_DAMAGE_DELAY                                = ConvertAbilityRealLevelField('Nic6')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_AMOUNT_NSO1                                = ConvertAbilityRealLevelField('Nso1')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_PERIOD                                     = ConvertAbilityRealLevelField('Nso2')
    constant abilityreallevelfield ABILITY_RLF_DAMAGE_PENALTY                                    = ConvertAbilityRealLevelField('Nso3')
    constant abilityreallevelfield ABILITY_RLF_MOVEMENT_SPEED_REDUCTION_PERCENT_NSO4             = ConvertAbilityRealLevelField('Nso4')
    constant abilityreallevelfield ABILITY_RLF_ATTACK_SPEED_REDUCTION_PERCENT_NSO5               = ConvertAbilityRealLevelField('Nso5')
    constant abilityreallevelfield ABILITY_RLF_SPLIT_DELAY                                       = ConvertAbilityRealLevelField('Nlm2')
    constant abilityreallevelfield ABILITY_RLF_MAX_HITPOINT_FACTOR                               = ConvertAbilityRealLevelField('Nlm4')
    constant abilityreallevelfield ABILITY_RLF_LIFE_DURATION_SPLIT_BONUS                         = ConvertAbilityRealLevelField('Nlm5')
    constant abilityreallevelfield ABILITY_RLF_WAVE_INTERVAL                                     = ConvertAbilityRealLevelField('Nvc3')
    constant abilityreallevelfield ABILITY_RLF_BUILDING_DAMAGE_FACTOR_NVC4                       = ConvertAbilityRealLevelField('Nvc4')
    constant abilityreallevelfield ABILITY_RLF_FULL_DAMAGE_AMOUNT_NVC5                           = ConvertAbilityRealLevelField('Nvc5')
    constant abilityreallevelfield ABILITY_RLF_HALF_DAMAGE_FACTOR                                = ConvertAbilityRealLevelField('Nvc6')
    constant abilityreallevelfield ABILITY_RLF_INTERVAL_BETWEEN_PULSES                           = ConvertAbilityRealLevelField('Tau5')

    constant abilitybooleanlevelfield ABILITY_BLF_PERCENT_BONUS_HAB2            = ConvertAbilityBooleanLevelField('Hab2')
    constant abilitybooleanlevelfield ABILITY_BLF_USE_TELEPORT_CLUSTERING_HMT3  = ConvertAbilityBooleanLevelField('Hmt3')
    constant abilitybooleanlevelfield ABILITY_BLF_NEVER_MISS_OCR5               = ConvertAbilityBooleanLevelField('Ocr5')
    constant abilitybooleanlevelfield ABILITY_BLF_EXCLUDE_ITEM_DAMAGE           = ConvertAbilityBooleanLevelField('Ocr6')
    constant abilitybooleanlevelfield ABILITY_BLF_BACKSTAB_DAMAGE               = ConvertAbilityBooleanLevelField('Owk4')
    constant abilitybooleanlevelfield ABILITY_BLF_INHERIT_UPGRADES_UAN3         = ConvertAbilityBooleanLevelField('Uan3')
    constant abilitybooleanlevelfield ABILITY_BLF_MANA_CONVERSION_AS_PERCENT    = ConvertAbilityBooleanLevelField('Udp3')
    constant abilitybooleanlevelfield ABILITY_BLF_LIFE_CONVERSION_AS_PERCENT    = ConvertAbilityBooleanLevelField('Udp4')
    constant abilitybooleanlevelfield ABILITY_BLF_LEAVE_TARGET_ALIVE            = ConvertAbilityBooleanLevelField('Udp5')
    constant abilitybooleanlevelfield ABILITY_BLF_PERCENT_BONUS_UAU3            = ConvertAbilityBooleanLevelField('Uau3')
    constant abilitybooleanlevelfield ABILITY_BLF_DAMAGE_IS_PERCENT_RECEIVED    = ConvertAbilityBooleanLevelField('Eah2')
    constant abilitybooleanlevelfield ABILITY_BLF_MELEE_BONUS                   = ConvertAbilityBooleanLevelField('Ear2')
    constant abilitybooleanlevelfield ABILITY_BLF_RANGED_BONUS                  = ConvertAbilityBooleanLevelField('Ear3')
    constant abilitybooleanlevelfield ABILITY_BLF_FLAT_BONUS                    = ConvertAbilityBooleanLevelField('Ear4')
    constant abilitybooleanlevelfield ABILITY_BLF_NEVER_MISS_HBH5               = ConvertAbilityBooleanLevelField('Hbh5')
    constant abilitybooleanlevelfield ABILITY_BLF_PERCENT_BONUS_HAD2            = ConvertAbilityBooleanLevelField('Had2')
    constant abilitybooleanlevelfield ABILITY_BLF_CAN_DEACTIVATE                = ConvertAbilityBooleanLevelField('Hds1')
    constant abilitybooleanlevelfield ABILITY_BLF_RAISED_UNITS_ARE_INVULNERABLE = ConvertAbilityBooleanLevelField('Hre2')
    constant abilitybooleanlevelfield ABILITY_BLF_PERCENTAGE_OAR2               = ConvertAbilityBooleanLevelField('Oar2')
    constant abilitybooleanlevelfield ABILITY_BLF_SUMMON_BUSY_UNITS             = ConvertAbilityBooleanLevelField('Btl2')
    constant abilitybooleanlevelfield ABILITY_BLF_CREATES_BLIGHT                = ConvertAbilityBooleanLevelField('Bli2')
    constant abilitybooleanlevelfield ABILITY_BLF_EXPLODES_ON_DEATH             = ConvertAbilityBooleanLevelField('Sds6')
    constant abilitybooleanlevelfield ABILITY_BLF_ALWAYS_AUTOCAST_FAE2          = ConvertAbilityBooleanLevelField('Fae2')
    constant abilitybooleanlevelfield ABILITY_BLF_REGENERATE_ONLY_AT_NIGHT      = ConvertAbilityBooleanLevelField('Mbt5')
    constant abilitybooleanlevelfield ABILITY_BLF_SHOW_SELECT_UNIT_BUTTON       = ConvertAbilityBooleanLevelField('Neu3')
    constant abilitybooleanlevelfield ABILITY_BLF_SHOW_UNIT_INDICATOR           = ConvertAbilityBooleanLevelField('Neu4')
    constant abilitybooleanlevelfield ABILITY_BLF_CHARGE_OWNING_PLAYER          = ConvertAbilityBooleanLevelField('Ans6')
    constant abilitybooleanlevelfield ABILITY_BLF_PERCENTAGE_ARM2               = ConvertAbilityBooleanLevelField('Arm2')
    constant abilitybooleanlevelfield ABILITY_BLF_TARGET_IS_INVULNERABLE        = ConvertAbilityBooleanLevelField('Pos3')
    constant abilitybooleanlevelfield ABILITY_BLF_TARGET_IS_MAGIC_IMMUNE        = ConvertAbilityBooleanLevelField('Pos4')
    constant abilitybooleanlevelfield ABILITY_BLF_KILL_ON_CASTER_DEATH          = ConvertAbilityBooleanLevelField('Ucb6')
    constant abilitybooleanlevelfield ABILITY_BLF_NO_TARGET_REQUIRED_REJ4       = ConvertAbilityBooleanLevelField('Rej4')
    constant abilitybooleanlevelfield ABILITY_BLF_ACCEPTS_GOLD                  = ConvertAbilityBooleanLevelField('Rtn1')
    constant abilitybooleanlevelfield ABILITY_BLF_ACCEPTS_LUMBER                = ConvertAbilityBooleanLevelField('Rtn2')
    constant abilitybooleanlevelfield ABILITY_BLF_PREFER_HOSTILES_ROA5          = ConvertAbilityBooleanLevelField('Roa5')
    constant abilitybooleanlevelfield ABILITY_BLF_PREFER_FRIENDLIES_ROA6        = ConvertAbilityBooleanLevelField('Roa6')
    constant abilitybooleanlevelfield ABILITY_BLF_ROOTED_TURNING                = ConvertAbilityBooleanLevelField('Roo3')
    constant abilitybooleanlevelfield ABILITY_BLF_ALWAYS_AUTOCAST_SLO3          = ConvertAbilityBooleanLevelField('Slo3')
    constant abilitybooleanlevelfield ABILITY_BLF_HIDE_BUTTON                   = ConvertAbilityBooleanLevelField('Ihid')
    constant abilitybooleanlevelfield ABILITY_BLF_USE_TELEPORT_CLUSTERING_ITP2  = ConvertAbilityBooleanLevelField('Itp2')
    constant abilitybooleanlevelfield ABILITY_BLF_IMMUNE_TO_MORPH_EFFECTS       = ConvertAbilityBooleanLevelField('Eth1')
    constant abilitybooleanlevelfield ABILITY_BLF_DOES_NOT_BLOCK_BUILDINGS      = ConvertAbilityBooleanLevelField('Eth2')
    constant abilitybooleanlevelfield ABILITY_BLF_AUTO_ACQUIRE_ATTACK_TARGETS   = ConvertAbilityBooleanLevelField('Gho1')
    constant abilitybooleanlevelfield ABILITY_BLF_IMMUNE_TO_MORPH_EFFECTS_GHO2  = ConvertAbilityBooleanLevelField('Gho2')
    constant abilitybooleanlevelfield ABILITY_BLF_DO_NOT_BLOCK_BUILDINGS        = ConvertAbilityBooleanLevelField('Gho3')
    constant abilitybooleanlevelfield ABILITY_BLF_INCLUDE_RANGED_DAMAGE         = ConvertAbilityBooleanLevelField('Ssk4')
    constant abilitybooleanlevelfield ABILITY_BLF_INCLUDE_MELEE_DAMAGE          = ConvertAbilityBooleanLevelField('Ssk5')
    constant abilitybooleanlevelfield ABILITY_BLF_MOVE_TO_PARTNER               = ConvertAbilityBooleanLevelField('coa2')
    constant abilitybooleanlevelfield ABILITY_BLF_CAN_BE_DISPELLED              = ConvertAbilityBooleanLevelField('cyc1')
    constant abilitybooleanlevelfield ABILITY_BLF_IGNORE_FRIENDLY_BUFFS         = ConvertAbilityBooleanLevelField('dvm6')
    constant abilitybooleanlevelfield ABILITY_BLF_DROP_ITEMS_ON_DEATH           = ConvertAbilityBooleanLevelField('inv2')
    constant abilitybooleanlevelfield ABILITY_BLF_CAN_USE_ITEMS                 = ConvertAbilityBooleanLevelField('inv3')
    constant abilitybooleanlevelfield ABILITY_BLF_CAN_GET_ITEMS                 = ConvertAbilityBooleanLevelField('inv4')
    constant abilitybooleanlevelfield ABILITY_BLF_CAN_DROP_ITEMS                = ConvertAbilityBooleanLevelField('inv5')
    constant abilitybooleanlevelfield ABILITY_BLF_REPAIRS_ALLOWED               = ConvertAbilityBooleanLevelField('liq4')
    constant abilitybooleanlevelfield ABILITY_BLF_CASTER_ONLY_SPLASH            = ConvertAbilityBooleanLevelField('mfl6')
    constant abilitybooleanlevelfield ABILITY_BLF_NO_TARGET_REQUIRED_IRL4       = ConvertAbilityBooleanLevelField('irl4')
    constant abilitybooleanlevelfield ABILITY_BLF_DISPEL_ON_ATTACK              = ConvertAbilityBooleanLevelField('irl5')
    constant abilitybooleanlevelfield ABILITY_BLF_AMOUNT_IS_RAW_VALUE           = ConvertAbilityBooleanLevelField('ipv3')
    constant abilitybooleanlevelfield ABILITY_BLF_SHARED_SPELL_COOLDOWN         = ConvertAbilityBooleanLevelField('spb2')
    constant abilitybooleanlevelfield ABILITY_BLF_SLEEP_ONCE                    = ConvertAbilityBooleanLevelField('sla1')
    constant abilitybooleanlevelfield ABILITY_BLF_ALLOW_ON_ANY_PLAYER_SLOT      = ConvertAbilityBooleanLevelField('sla2')
    constant abilitybooleanlevelfield ABILITY_BLF_DISABLE_OTHER_ABILITIES       = ConvertAbilityBooleanLevelField('Ncl5')
    constant abilitybooleanlevelfield ABILITY_BLF_ALLOW_BOUNTY                  = ConvertAbilityBooleanLevelField('Ntm4')

    constant abilitystringlevelfield ABILITY_SLF_ICON_NORMAL                    = ConvertAbilityStringLevelField('aart')
    constant abilitystringlevelfield ABILITY_SLF_CASTER                         = ConvertAbilityStringLevelField('acat')
    constant abilitystringlevelfield ABILITY_SLF_TARGET                         = ConvertAbilityStringLevelField('atat')
    constant abilitystringlevelfield ABILITY_SLF_SPECIAL                        = ConvertAbilityStringLevelField('asat')
    constant abilitystringlevelfield ABILITY_SLF_EFFECT                         = ConvertAbilityStringLevelField('aeat')
    constant abilitystringlevelfield ABILITY_SLF_AREA_EFFECT                    = ConvertAbilityStringLevelField('aaea')
    constant abilitystringlevelfield ABILITY_SLF_LIGHTNING_EFFECTS              = ConvertAbilityStringLevelField('alig')
    constant abilitystringlevelfield ABILITY_SLF_MISSILE_ART                    = ConvertAbilityStringLevelField('amat')
    constant abilitystringlevelfield ABILITY_SLF_TOOLTIP_LEARN                  = ConvertAbilityStringLevelField('aret')
    constant abilitystringlevelfield ABILITY_SLF_TOOLTIP_LEARN_EXTENDED         = ConvertAbilityStringLevelField('arut')
    constant abilitystringlevelfield ABILITY_SLF_TOOLTIP_NORMAL                 = ConvertAbilityStringLevelField('atp1')
    constant abilitystringlevelfield ABILITY_SLF_TOOLTIP_TURN_OFF               = ConvertAbilityStringLevelField('aut1')
    constant abilitystringlevelfield ABILITY_SLF_TOOLTIP_NORMAL_EXTENDED        = ConvertAbilityStringLevelField('aub1')
    constant abilitystringlevelfield ABILITY_SLF_TOOLTIP_TURN_OFF_EXTENDED      = ConvertAbilityStringLevelField('auu1')
    constant abilitystringlevelfield ABILITY_SLF_NORMAL_FORM_UNIT_EME1          = ConvertAbilityStringLevelField('Eme1')
    constant abilitystringlevelfield ABILITY_SLF_SPAWNED_UNITS                  = ConvertAbilityStringLevelField('Ndp1')
    constant abilitystringlevelfield ABILITY_SLF_ABILITY_FOR_UNIT_CREATION      = ConvertAbilityStringLevelField('Nrc1')
    constant abilitystringlevelfield ABILITY_SLF_NORMAL_FORM_UNIT_MIL1          = ConvertAbilityStringLevelField('Mil1')
    constant abilitystringlevelfield ABILITY_SLF_ALTERNATE_FORM_UNIT_MIL2       = ConvertAbilityStringLevelField('Mil2')
    constant abilitystringlevelfield ABILITY_SLF_BASE_ORDER_ID_ANS5             = ConvertAbilityStringLevelField('Ans5')
    constant abilitystringlevelfield ABILITY_SLF_MORPH_UNITS_GROUND             = ConvertAbilityStringLevelField('Ply2')
    constant abilitystringlevelfield ABILITY_SLF_MORPH_UNITS_AIR                = ConvertAbilityStringLevelField('Ply3')
    constant abilitystringlevelfield ABILITY_SLF_MORPH_UNITS_AMPHIBIOUS         = ConvertAbilityStringLevelField('Ply4')
    constant abilitystringlevelfield ABILITY_SLF_MORPH_UNITS_WATER              = ConvertAbilityStringLevelField('Ply5')
    constant abilitystringlevelfield ABILITY_SLF_UNIT_TYPE_ONE                  = ConvertAbilityStringLevelField('Rai3')
    constant abilitystringlevelfield ABILITY_SLF_UNIT_TYPE_TWO                  = ConvertAbilityStringLevelField('Rai4')
    constant abilitystringlevelfield ABILITY_SLF_UNIT_TYPE_SOD2                 = ConvertAbilityStringLevelField('Sod2')
    constant abilitystringlevelfield ABILITY_SLF_SUMMON_1_UNIT_TYPE             = ConvertAbilityStringLevelField('Ist1')
    constant abilitystringlevelfield ABILITY_SLF_SUMMON_2_UNIT_TYPE             = ConvertAbilityStringLevelField('Ist2')
    constant abilitystringlevelfield ABILITY_SLF_RACE_TO_CONVERT                = ConvertAbilityStringLevelField('Ndc1')
    constant abilitystringlevelfield ABILITY_SLF_PARTNER_UNIT_TYPE              = ConvertAbilityStringLevelField('coa1')
    constant abilitystringlevelfield ABILITY_SLF_PARTNER_UNIT_TYPE_ONE          = ConvertAbilityStringLevelField('dcp1')
    constant abilitystringlevelfield ABILITY_SLF_PARTNER_UNIT_TYPE_TWO          = ConvertAbilityStringLevelField('dcp2')
    constant abilitystringlevelfield ABILITY_SLF_REQUIRED_UNIT_TYPE             = ConvertAbilityStringLevelField('tpi1')
    constant abilitystringlevelfield ABILITY_SLF_CONVERTED_UNIT_TYPE            = ConvertAbilityStringLevelField('tpi2')
    constant abilitystringlevelfield ABILITY_SLF_SPELL_LIST                     = ConvertAbilityStringLevelField('spb1')
    constant abilitystringlevelfield ABILITY_SLF_BASE_ORDER_ID_SPB5             = ConvertAbilityStringLevelField('spb5')
    constant abilitystringlevelfield ABILITY_SLF_BASE_ORDER_ID_NCL6             = ConvertAbilityStringLevelField('Ncl6')
    constant abilitystringlevelfield ABILITY_SLF_ABILITY_UPGRADE_1              = ConvertAbilityStringLevelField('Neg3')
    constant abilitystringlevelfield ABILITY_SLF_ABILITY_UPGRADE_2              = ConvertAbilityStringLevelField('Neg4')
    constant abilitystringlevelfield ABILITY_SLF_ABILITY_UPGRADE_3              = ConvertAbilityStringLevelField('Neg5')
    constant abilitystringlevelfield ABILITY_SLF_ABILITY_UPGRADE_4              = ConvertAbilityStringLevelField('Neg6')
    constant abilitystringlevelfield ABILITY_SLF_SPAWN_UNIT_ID_NSY2             = ConvertAbilityStringLevelField('Nsy2')

    // Item
    constant itemintegerfield ITEM_IF_LEVEL                 = ConvertItemIntegerField('ilev')
    constant itemintegerfield ITEM_IF_NUMBER_OF_CHARGES     = ConvertItemIntegerField('iuse')
    constant itemintegerfield ITEM_IF_COOLDOWN_GROUP        = ConvertItemIntegerField('icid')
    constant itemintegerfield ITEM_IF_MAX_HIT_POINTS        = ConvertItemIntegerField('ihtp')
    constant itemintegerfield ITEM_IF_HIT_POINTS            = ConvertItemIntegerField('ihpc')
    constant itemintegerfield ITEM_IF_PRIORITY              = ConvertItemIntegerField('ipri')
    constant itemintegerfield ITEM_IF_ARMOR_TYPE            = ConvertItemIntegerField('iarm')
    constant itemintegerfield ITEM_IF_TINTING_COLOR_RED     = ConvertItemIntegerField('iclr')
    constant itemintegerfield ITEM_IF_TINTING_COLOR_GREEN   = ConvertItemIntegerField('iclg')
    constant itemintegerfield ITEM_IF_TINTING_COLOR_BLUE    = ConvertItemIntegerField('iclb')
    constant itemintegerfield ITEM_IF_TINTING_COLOR_ALPHA   = ConvertItemIntegerField('ical')

    constant itemrealfield ITEM_RF_SCALING_VALUE            = ConvertItemRealField('isca')

    constant itembooleanfield ITEM_BF_DROPPED_WHEN_CARRIER_DIES         = ConvertItemBooleanField('idrp')
    constant itembooleanfield ITEM_BF_CAN_BE_DROPPED                    = ConvertItemBooleanField('idro')
    constant itembooleanfield ITEM_BF_PERISHABLE                        = ConvertItemBooleanField('iper')
    constant itembooleanfield ITEM_BF_INCLUDE_AS_RANDOM_CHOICE          = ConvertItemBooleanField('iprn')
    constant itembooleanfield ITEM_BF_USE_AUTOMATICALLY_WHEN_ACQUIRED   = ConvertItemBooleanField('ipow')
    constant itembooleanfield ITEM_BF_CAN_BE_SOLD_TO_MERCHANTS          = ConvertItemBooleanField('ipaw')
    constant itembooleanfield ITEM_BF_ACTIVELY_USED                     = ConvertItemBooleanField('iusa')

    constant itemstringfield ITEM_SF_MODEL_USED                         = ConvertItemStringField('ifil')

    // Unit
    constant unitintegerfield UNIT_IF_DEFENSE_TYPE                          = ConvertUnitIntegerField('udty')
    constant unitintegerfield UNIT_IF_ARMOR_TYPE                            = ConvertUnitIntegerField('uarm')
    constant unitintegerfield UNIT_IF_LOOPING_FADE_IN_RATE                  = ConvertUnitIntegerField('ulfi')
    constant unitintegerfield UNIT_IF_LOOPING_FADE_OUT_RATE                 = ConvertUnitIntegerField('ulfo')
    constant unitintegerfield UNIT_IF_AGILITY                               = ConvertUnitIntegerField('uagc')
    constant unitintegerfield UNIT_IF_INTELLIGENCE                          = ConvertUnitIntegerField('uinc')
    constant unitintegerfield UNIT_IF_STRENGTH                              = ConvertUnitIntegerField('ustc')
    constant unitintegerfield UNIT_IF_AGILITY_PERMANENT                     = ConvertUnitIntegerField('uagm')
    constant unitintegerfield UNIT_IF_INTELLIGENCE_PERMANENT                = ConvertUnitIntegerField('uinm')
    constant unitintegerfield UNIT_IF_STRENGTH_PERMANENT                    = ConvertUnitIntegerField('ustm')
    constant unitintegerfield UNIT_IF_AGILITY_WITH_BONUS                    = ConvertUnitIntegerField('uagb')
    constant unitintegerfield UNIT_IF_INTELLIGENCE_WITH_BONUS               = ConvertUnitIntegerField('uinb')
    constant unitintegerfield UNIT_IF_STRENGTH_WITH_BONUS                   = ConvertUnitIntegerField('ustb')
    constant unitintegerfield UNIT_IF_GOLD_BOUNTY_AWARDED_NUMBER_OF_DICE    = ConvertUnitIntegerField('ubdi')
    constant unitintegerfield UNIT_IF_GOLD_BOUNTY_AWARDED_BASE              = ConvertUnitIntegerField('ubba')
    constant unitintegerfield UNIT_IF_GOLD_BOUNTY_AWARDED_SIDES_PER_DIE     = ConvertUnitIntegerField('ubsi')
    constant unitintegerfield UNIT_IF_LUMBER_BOUNTY_AWARDED_NUMBER_OF_DICE  = ConvertUnitIntegerField('ulbd')
    constant unitintegerfield UNIT_IF_LUMBER_BOUNTY_AWARDED_BASE            = ConvertUnitIntegerField('ulba')
    constant unitintegerfield UNIT_IF_LUMBER_BOUNTY_AWARDED_SIDES_PER_DIE   = ConvertUnitIntegerField('ulbs')
    constant unitintegerfield UNIT_IF_LEVEL                                 = ConvertUnitIntegerField('ulev')
    constant unitintegerfield UNIT_IF_FORMATION_RANK                        = ConvertUnitIntegerField('ufor')
    constant unitintegerfield UNIT_IF_ORIENTATION_INTERPOLATION             = ConvertUnitIntegerField('uori')
    constant unitintegerfield UNIT_IF_ELEVATION_SAMPLE_POINTS               = ConvertUnitIntegerField('uept')
    constant unitintegerfield UNIT_IF_TINTING_COLOR_RED                     = ConvertUnitIntegerField('uclr')
    constant unitintegerfield UNIT_IF_TINTING_COLOR_GREEN                   = ConvertUnitIntegerField('uclg')
    constant unitintegerfield UNIT_IF_TINTING_COLOR_BLUE                    = ConvertUnitIntegerField('uclb')
    constant unitintegerfield UNIT_IF_TINTING_COLOR_ALPHA                   = ConvertUnitIntegerField('ucal')
    constant unitintegerfield UNIT_IF_MOVE_TYPE                             = ConvertUnitIntegerField('umvt')
    constant unitintegerfield UNIT_IF_TARGETED_AS                           = ConvertUnitIntegerField('utar')
    constant unitintegerfield UNIT_IF_UNIT_CLASSIFICATION                   = ConvertUnitIntegerField('utyp')
    constant unitintegerfield UNIT_IF_HIT_POINTS_REGENERATION_TYPE          = ConvertUnitIntegerField('uhrt')
    constant unitintegerfield UNIT_IF_PLACEMENT_PREVENTED_BY                = ConvertUnitIntegerField('upar')
    constant unitintegerfield UNIT_IF_PRIMARY_ATTRIBUTE                     = ConvertUnitIntegerField('upra')

    constant unitrealfield UNIT_RF_STRENGTH_PER_LEVEL                       = ConvertUnitRealField('ustp')
    constant unitrealfield UNIT_RF_AGILITY_PER_LEVEL                        = ConvertUnitRealField('uagp')
    constant unitrealfield UNIT_RF_INTELLIGENCE_PER_LEVEL                   = ConvertUnitRealField('uinp')
    constant unitrealfield UNIT_RF_HIT_POINTS_REGENERATION_RATE             = ConvertUnitRealField('uhpr')
    constant unitrealfield UNIT_RF_MANA_REGENERATION                        = ConvertUnitRealField('umpr')
    constant unitrealfield UNIT_RF_DEATH_TIME                               = ConvertUnitRealField('udtm')
    constant unitrealfield UNIT_RF_FLY_HEIGHT                               = ConvertUnitRealField('ufyh')
    constant unitrealfield UNIT_RF_TURN_RATE                                = ConvertUnitRealField('umvr')
    constant unitrealfield UNIT_RF_ELEVATION_SAMPLE_RADIUS                  = ConvertUnitRealField('uerd')
    constant unitrealfield UNIT_RF_FOG_OF_WAR_SAMPLE_RADIUS                 = ConvertUnitRealField('ufrd')
    constant unitrealfield UNIT_RF_MAXIMUM_PITCH_ANGLE_DEGREES              = ConvertUnitRealField('umxp')
    constant unitrealfield UNIT_RF_MAXIMUM_ROLL_ANGLE_DEGREES               = ConvertUnitRealField('umxr')
    constant unitrealfield UNIT_RF_SCALING_VALUE                            = ConvertUnitRealField('usca')
    constant unitrealfield UNIT_RF_ANIMATION_RUN_SPEED                      = ConvertUnitRealField('urun')
    constant unitrealfield UNIT_RF_SELECTION_SCALE                          = ConvertUnitRealField('ussc')
    constant unitrealfield UNIT_RF_SELECTION_CIRCLE_HEIGHT                  = ConvertUnitRealField('uslz')
    constant unitrealfield UNIT_RF_SHADOW_IMAGE_HEIGHT                      = ConvertUnitRealField('ushh')
    constant unitrealfield UNIT_RF_SHADOW_IMAGE_WIDTH                       = ConvertUnitRealField('ushw')
    constant unitrealfield UNIT_RF_SHADOW_IMAGE_CENTER_X                    = ConvertUnitRealField('ushx')
    constant unitrealfield UNIT_RF_SHADOW_IMAGE_CENTER_Y                    = ConvertUnitRealField('ushy')
    constant unitrealfield UNIT_RF_ANIMATION_WALK_SPEED                     = ConvertUnitRealField('uwal')
    constant unitrealfield UNIT_RF_DEFENSE                                  = ConvertUnitRealField('udfc')
    constant unitrealfield UNIT_RF_SIGHT_RADIUS                             = ConvertUnitRealField('usir')
    constant unitrealfield UNIT_RF_PRIORITY                                 = ConvertUnitRealField('upri')
    constant unitrealfield UNIT_RF_SPEED                                    = ConvertUnitRealField('umvc')
    constant unitrealfield UNIT_RF_OCCLUDER_HEIGHT                          = ConvertUnitRealField('uocc')
    constant unitrealfield UNIT_RF_HP                                       = ConvertUnitRealField('uhpc')
    constant unitrealfield UNIT_RF_MANA                                     = ConvertUnitRealField('umpc')
    constant unitrealfield UNIT_RF_ACQUISITION_RANGE                        = ConvertUnitRealField('uacq')
    constant unitrealfield UNIT_RF_CAST_BACK_SWING                          = ConvertUnitRealField('ucbs')
    constant unitrealfield UNIT_RF_CAST_POINT                               = ConvertUnitRealField('ucpt')
    constant unitrealfield UNIT_RF_MINIMUM_ATTACK_RANGE                     = ConvertUnitRealField('uamn')

    constant unitbooleanfield UNIT_BF_RAISABLE                              = ConvertUnitBooleanField('urai')
    constant unitbooleanfield UNIT_BF_DECAYABLE                             = ConvertUnitBooleanField('udec')
    constant unitbooleanfield UNIT_BF_IS_A_BUILDING                         = ConvertUnitBooleanField('ubdg')
    constant unitbooleanfield UNIT_BF_USE_EXTENDED_LINE_OF_SIGHT            = ConvertUnitBooleanField('ulos')
    constant unitbooleanfield UNIT_BF_NEUTRAL_BUILDING_SHOWS_MINIMAP_ICON   = ConvertUnitBooleanField('unbm')
    constant unitbooleanfield UNIT_BF_HERO_HIDE_HERO_INTERFACE_ICON         = ConvertUnitBooleanField('uhhb')
    constant unitbooleanfield UNIT_BF_HERO_HIDE_HERO_MINIMAP_DISPLAY        = ConvertUnitBooleanField('uhhm')
    constant unitbooleanfield UNIT_BF_HERO_HIDE_HERO_DEATH_MESSAGE          = ConvertUnitBooleanField('uhhd')
    constant unitbooleanfield UNIT_BF_HIDE_MINIMAP_DISPLAY                  = ConvertUnitBooleanField('uhom')
    constant unitbooleanfield UNIT_BF_SCALE_PROJECTILES                     = ConvertUnitBooleanField('uscb')
    constant unitbooleanfield UNIT_BF_SELECTION_CIRCLE_ON_WATER             = ConvertUnitBooleanField('usew')
    constant unitbooleanfield UNIT_BF_HAS_WATER_SHADOW                      = ConvertUnitBooleanField('ushr')

    constant unitstringfield UNIT_SF_NAME                   = ConvertUnitStringField('unam')
    constant unitstringfield UNIT_SF_PROPER_NAMES           = ConvertUnitStringField('upro')
    constant unitstringfield UNIT_SF_GROUND_TEXTURE         = ConvertUnitStringField('uubs')
    constant unitstringfield UNIT_SF_SHADOW_IMAGE_UNIT      = ConvertUnitStringField('ushu')

    // Unit Weapon
    constant unitweaponintegerfield UNIT_WEAPON_IF_ATTACK_DAMAGE_NUMBER_OF_DICE     = ConvertUnitWeaponIntegerField('ua1d')
    constant unitweaponintegerfield UNIT_WEAPON_IF_ATTACK_DAMAGE_BASE               = ConvertUnitWeaponIntegerField('ua1b')
    constant unitweaponintegerfield UNIT_WEAPON_IF_ATTACK_DAMAGE_SIDES_PER_DIE      = ConvertUnitWeaponIntegerField('ua1s')
    constant unitweaponintegerfield UNIT_WEAPON_IF_ATTACK_MAXIMUM_NUMBER_OF_TARGETS = ConvertUnitWeaponIntegerField('utc1')
    constant unitweaponintegerfield UNIT_WEAPON_IF_ATTACK_ATTACK_TYPE               = ConvertUnitWeaponIntegerField('ua1t')
    constant unitweaponintegerfield UNIT_WEAPON_IF_ATTACK_WEAPON_SOUND              = ConvertUnitWeaponIntegerField('ucs1')
    constant unitweaponintegerfield UNIT_WEAPON_IF_ATTACK_AREA_OF_EFFECT_TARGETS    = ConvertUnitWeaponIntegerField('ua1p')
    constant unitweaponintegerfield UNIT_WEAPON_IF_ATTACK_TARGETS_ALLOWED           = ConvertUnitWeaponIntegerField('ua1g')

    constant unitweaponrealfield UNIT_WEAPON_RF_ATTACK_BACKSWING_POINT              = ConvertUnitWeaponRealField('ubs1')
    constant unitweaponrealfield UNIT_WEAPON_RF_ATTACK_DAMAGE_POINT                 = ConvertUnitWeaponRealField('udp1')
    constant unitweaponrealfield UNIT_WEAPON_RF_ATTACK_BASE_COOLDOWN                = ConvertUnitWeaponRealField('ua1c')
    constant unitweaponrealfield UNIT_WEAPON_RF_ATTACK_DAMAGE_LOSS_FACTOR           = ConvertUnitWeaponRealField('udl1')
    constant unitweaponrealfield UNIT_WEAPON_RF_ATTACK_DAMAGE_FACTOR_MEDIUM         = ConvertUnitWeaponRealField('uhd1')
    constant unitweaponrealfield UNIT_WEAPON_RF_ATTACK_DAMAGE_FACTOR_SMALL          = ConvertUnitWeaponRealField('uqd1')
    constant unitweaponrealfield UNIT_WEAPON_RF_ATTACK_DAMAGE_SPILL_DISTANCE        = ConvertUnitWeaponRealField('usd1')
    constant unitweaponrealfield UNIT_WEAPON_RF_ATTACK_DAMAGE_SPILL_RADIUS          = ConvertUnitWeaponRealField('usr1')
    constant unitweaponrealfield UNIT_WEAPON_RF_ATTACK_PROJECTILE_SPEED             = ConvertUnitWeaponRealField('ua1z')
    constant unitweaponrealfield UNIT_WEAPON_RF_ATTACK_PROJECTILE_ARC               = ConvertUnitWeaponRealField('uma1')
    constant unitweaponrealfield UNIT_WEAPON_RF_ATTACK_AREA_OF_EFFECT_FULL_DAMAGE   = ConvertUnitWeaponRealField('ua1f')
    constant unitweaponrealfield UNIT_WEAPON_RF_ATTACK_AREA_OF_EFFECT_MEDIUM_DAMAGE = ConvertUnitWeaponRealField('ua1h')
    constant unitweaponrealfield UNIT_WEAPON_RF_ATTACK_AREA_OF_EFFECT_SMALL_DAMAGE  = ConvertUnitWeaponRealField('ua1q')
    constant unitweaponrealfield UNIT_WEAPON_RF_ATTACK_RANGE                        = ConvertUnitWeaponRealField('ua1r')

    constant unitweaponbooleanfield UNIT_WEAPON_BF_ATTACK_SHOW_UI                   = ConvertUnitWeaponBooleanField('uwu1')
    constant unitweaponbooleanfield UNIT_WEAPON_BF_ATTACKS_ENABLED                  = ConvertUnitWeaponBooleanField('uaen')
    constant unitweaponbooleanfield UNIT_WEAPON_BF_ATTACK_PROJECTILE_HOMING_ENABLED = ConvertUnitWeaponBooleanField('umh1')
    
    constant unitweaponstringfield UNIT_WEAPON_SF_ATTACK_PROJECTILE_ART             = ConvertUnitWeaponStringField('ua1m')

    // Move Type
    constant movetype       MOVE_TYPE_UNKNOWN               = ConvertMoveType(0)
    constant movetype       MOVE_TYPE_FOOT                  = ConvertMoveType(1)
    constant movetype       MOVE_TYPE_FLY                   = ConvertMoveType(2)
    constant movetype       MOVE_TYPE_HORSE                 = ConvertMoveType(4)
    constant movetype       MOVE_TYPE_HOVER                 = ConvertMoveType(8)
    constant movetype       MOVE_TYPE_FLOAT                 = ConvertMoveType(16)
    constant movetype       MOVE_TYPE_AMPHIBIOUS            = ConvertMoveType(32)
    constant movetype       MOVE_TYPE_UNBUILDABLE           = ConvertMoveType(64)
    
    // Target Flag
    constant targetflag     TARGET_FLAG_NONE                = ConvertTargetFlag(1)
    constant targetflag     TARGET_FLAG_GROUND              = ConvertTargetFlag(2)
    constant targetflag     TARGET_FLAG_AIR                 = ConvertTargetFlag(4)
    constant targetflag     TARGET_FLAG_STRUCTURE           = ConvertTargetFlag(8)
    constant targetflag     TARGET_FLAG_WARD                = ConvertTargetFlag(16)
    constant targetflag     TARGET_FLAG_ITEM                = ConvertTargetFlag(32)
    constant targetflag     TARGET_FLAG_TREE                = ConvertTargetFlag(64)
    constant targetflag     TARGET_FLAG_WALL                = ConvertTargetFlag(128)
    constant targetflag     TARGET_FLAG_DEBRIS              = ConvertTargetFlag(256)
    constant targetflag     TARGET_FLAG_DECORATION          = ConvertTargetFlag(512)
    constant targetflag     TARGET_FLAG_BRIDGE              = ConvertTargetFlag(1024)

    // defense type
    constant defensetype    DEFENSE_TYPE_LIGHT              = ConvertDefenseType(0)
    constant defensetype    DEFENSE_TYPE_MEDIUM             = ConvertDefenseType(1)
    constant defensetype    DEFENSE_TYPE_LARGE              = ConvertDefenseType(2)
    constant defensetype    DEFENSE_TYPE_FORT               = ConvertDefenseType(3)
    constant defensetype    DEFENSE_TYPE_NORMAL             = ConvertDefenseType(4)
    constant defensetype    DEFENSE_TYPE_HERO               = ConvertDefenseType(5)
    constant defensetype    DEFENSE_TYPE_DIVINE             = ConvertDefenseType(6)
    constant defensetype    DEFENSE_TYPE_NONE               = ConvertDefenseType(7)

    // Hero Attribute
    constant heroattribute  HERO_ATTRIBUTE_STR              = ConvertHeroAttribute(1)
    constant heroattribute  HERO_ATTRIBUTE_INT              = ConvertHeroAttribute(2)
    constant heroattribute  HERO_ATTRIBUTE_AGI              = ConvertHeroAttribute(3)

    // Armor Type
    constant armortype      ARMOR_TYPE_WHOKNOWS             = ConvertArmorType(0)
    constant armortype      ARMOR_TYPE_FLESH                = ConvertArmorType(1)
    constant armortype      ARMOR_TYPE_METAL                = ConvertArmorType(2)
    constant armortype      ARMOR_TYPE_WOOD                 = ConvertArmorType(3)
    constant armortype      ARMOR_TYPE_ETHREAL              = ConvertArmorType(4)
    constant armortype      ARMOR_TYPE_STONE                = ConvertArmorType(5)

    // Regeneration Type
    constant regentype      REGENERATION_TYPE_NONE          = ConvertRegenType(0)
    constant regentype      REGENERATION_TYPE_ALWAYS        = ConvertRegenType(1)
    constant regentype      REGENERATION_TYPE_BLIGHT        = ConvertRegenType(2)
    constant regentype      REGENERATION_TYPE_DAY           = ConvertRegenType(3)
    constant regentype      REGENERATION_TYPE_NIGHT         = ConvertRegenType(4)

    // Unit Category
    constant unitcategory   UNIT_CATEGORY_GIANT             = ConvertUnitCategory(1)
    constant unitcategory   UNIT_CATEGORY_UNDEAD            = ConvertUnitCategory(2)
    constant unitcategory   UNIT_CATEGORY_SUMMONED          = ConvertUnitCategory(4)
    constant unitcategory   UNIT_CATEGORY_MECHANICAL        = ConvertUnitCategory(8)
    constant unitcategory   UNIT_CATEGORY_PEON              = ConvertUnitCategory(16)
    constant unitcategory   UNIT_CATEGORY_SAPPER            = ConvertUnitCategory(32)
    constant unitcategory   UNIT_CATEGORY_TOWNHALL          = ConvertUnitCategory(64)
    constant unitcategory   UNIT_CATEGORY_ANCIENT           = ConvertUnitCategory(128)
    constant unitcategory   UNIT_CATEGORY_NEUTRAL           = ConvertUnitCategory(256)
    constant unitcategory   UNIT_CATEGORY_WARD              = ConvertUnitCategory(512)
    constant unitcategory   UNIT_CATEGORY_STANDON           = ConvertUnitCategory(1024)
    constant unitcategory   UNIT_CATEGORY_TAUREN            = ConvertUnitCategory(2048)

    // Pathing Flag
    constant pathingflag    PATHING_FLAG_UNWALKABLE             = ConvertPathingFlag(2)
    constant pathingflag    PATHING_FLAG_UNFLYABLE              = ConvertPathingFlag(4)
    constant pathingflag    PATHING_FLAG_UNBUILDABLE            = ConvertPathingFlag(8)
    constant pathingflag    PATHING_FLAG_UNPEONHARVEST          = ConvertPathingFlag(16)
    constant pathingflag    PATHING_FLAG_BLIGHTED               = ConvertPathingFlag(32)
    constant pathingflag    PATHING_FLAG_UNFLOATABLE            = ConvertPathingFlag(64)
    constant pathingflag    PATHING_FLAG_UNAMPHIBIOUS           = ConvertPathingFlag(128)
    constant pathingflag    PATHING_FLAG_UNITEMPLACABLE         = ConvertPathingFlag(256)




endglobals

