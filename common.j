//============================================================================
// Native types. All native functions take extended handle types when
// possible to help prevent passing bad values to native functions
//

/**

@patch 1.24b

*/
type agent			    extends     handle  // all reference counted objects

/**
Currently useless, although triggers return an event reference when you register
a new trigger-event, there are no useful functions.
You cannot destroy an event object either (technically a leak).

The only functions that take event are: `SaveTriggerEventHandle` and
`SaveTriggerEventHandleBJ`.

*/
type event              extends     agent  // a reference to an event registration
type player             extends     agent  // a single player reference

/**
A widget is an "interactive game object" with HP, possibly an inventory etc.

Types `unit`, `destructable`, `item` extend from widget.
Widget is the parent type and unit etc. are the descendant types (children).

Therefore all API functions that accept `widget` as a type, will also work with
any of the children types.

However if doesn't work the other way around, then you need to explicitly cast the type by pushing it through a hashtable, this is called "downcasting":

1. Put the widget object in a hashtable
2. Retrieve it as unit/destructable/item - needed since 1.24b,
[source](https://web.archive.org/web/20100118203210/http://wiki.thehelper.net/wc3/jass/common.j/Widget_API)

**Example (Lua)**:

```{.lua}
hasht = InitHashtable() -- for type-casting
SaveWidgetHandle(hasht, 1, 1, widgetHandle) -- put as widget
itemHandle = LoadItemHandle(hasht, 1, 1) -- retrieve as item
```

See `TriggerRegisterDeathEvent` for a full practical example.

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

/**
Currently useless, there are no functions that take `widgetevent`.

*/
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

/**
Represents different fog of war types.

- `FOG_OF_WAR_MASKED` (1): Black mask, an unexplored map area.
    - If "Masked areas are partially visible" is enabled in
Map Properties, unexplored areas are shown in dark grey.
You can see the terrain, but no units.
    - If disabled, unexplored areas are black and not visible.
- `FOG_OF_WAR_FOGGED` (2): Haze, a previously explored
map area that is currently not visible.
    - You can see the terrain, but no units.
- `FOG_OF_WAR_VISIBLE` (4): A fully visible map area.
- Other (non-existent) fog types do nothing.

*/
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

/**
Trackables can register both click and hover events by players. But they don't
provide a way to get the triggering player. In fact the only (as of writing)
functions that have the trackable type in their signature are:

* `CreateTrackable`
* `TriggerRegisterTrackableHitEvent`
* `TriggerRegisterTrackableTrackEvent`
* `GetTriggeringTrackable`
* `SaveTrackableHandle`
* `LoadTrackableHandle`

To create a trackable which can distinguish the triggering player we simply
create a trackable for each player but with a *locally* different path:

```
function CreateTrackableForPlayer takes player p, string path, real x, real y, real facing returns trackable
    if GetLocalPlayer() != p then
        set path = ""
    endif
    return CreateTrackable(path, x, y, facing)
endfunction
```

Now using something like `hashtable`s or [lua tables](https://www.lua.org/pil/2.5.html)
we can attach the correct player to the trackable handle and retrieve it by
accessing `GetTriggeringTrackable`. You can use the same technique to attach
other information like the trackables position, facing, etc.

@note See `CreateTrackable` for a way to create a trackable with a non-zero
z-coordinate.


*/
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
type commandbuttoneffect            extends handle



/**
Returns the race that corresponds to the given integer.
@param i The integer representation of the race.

@pure 

*/
constant native ConvertRace                 takes integer i returns race

/**
Returns the alliancetype that corresponds to the given integer.
@param i The integer representation of the alliancetype.

@pure 

*/
constant native ConvertAllianceType         takes integer i returns alliancetype

/**
Returns the racepreference that corresponds to the given integer.
@param i The integer representation of the racepreference.

@pure 

*/
constant native ConvertRacePref             takes integer i returns racepreference

/**
Returns the igamestate that corresponds to the given integer.
@param i The integer representation of the igamestate.

@pure 

*/
constant native ConvertIGameState           takes integer i returns igamestate

/**
Returns the fgamestate that corresponds to the given integer.
@param i The integer representation of the fgamestate.

@pure 

*/
constant native ConvertFGameState           takes integer i returns fgamestate

/**
Returns the playerstate that corresponds to the given integer.
@param i The integer representation of the playerstate.

@pure 

*/
constant native ConvertPlayerState          takes integer i returns playerstate

/**
Returns the playerscore that corresponds to the given integer.
@param i The integer representation of the playerscore.

@pure 

*/
constant native ConvertPlayerScore          takes integer i returns playerscore

/**
Returns the playergameresult that corresponds to the given integer.
@param i The integer representation of the playergameresult.

@pure 

*/
constant native ConvertPlayerGameResult     takes integer i returns playergameresult

/**
Returns unitstate, first index is 0. 

It is used to define the constants representing unit state. Accepts any integer, the unitstate reference is always the same for a given integer.

**Example:** `constant unitstate UNIT_STATE_MAX_MANA = ConvertUnitState(3)`

@param i The integer representation of the unitstate.


@note See: `GetUnitState`, `SetUnitState`.
@pure 

*/
constant native ConvertUnitState            takes integer i returns unitstate

/**
Returns the aidifficulty that corresponds to the given integer.
@param i The integer representation of the aidifficulty.

@pure 

*/
constant native ConvertAIDifficulty         takes integer i returns aidifficulty

/**
Returns the gameevent that corresponds to the given integer.
@param i The integer representation of the gameevent.

@pure 

*/
constant native ConvertGameEvent            takes integer i returns gameevent

/**
Returns the playerevent that corresponds to the given integer.
@param i The integer representation of the playerevent.

@pure 

*/
constant native ConvertPlayerEvent          takes integer i returns playerevent

/**
Returns the playerunitevent that corresponds to the given integer.
@param i The integer representation of the playerunitevent.

@pure 

*/
constant native ConvertPlayerUnitEvent      takes integer i returns playerunitevent

/**
Returns the widgetevent that corresponds to the given integer.
@param i The integer representation of the widgetevent.

@pure 

*/
constant native ConvertWidgetEvent          takes integer i returns widgetevent

/**
Returns the dialogevent that corresponds to the given integer.
@param i The integer representation of the dialogevent.

@pure 

*/
constant native ConvertDialogEvent          takes integer i returns dialogevent

/**
Returns the unitevent that corresponds to the given integer.
@param i The integer representation of the unitevent.

@pure 

*/
constant native ConvertUnitEvent            takes integer i returns unitevent

/**
Returns the limitop that corresponds to the given integer.
@param i The integer representation of the limitop.

@pure 

*/
constant native ConvertLimitOp              takes integer i returns limitop

/**
Returns the unittype that corresponds to the given integer.
@param i The integer representation of the unittype.

@pure 

*/
constant native ConvertUnitType             takes integer i returns unittype

/**
Returns the gamespeed that corresponds to the given integer.

It is used to define the constants representing gamespeed. First index is 0. Accepts any integer, the reference is always the same for a given integer.

@param i The integer representation of the gamespeed.

@pure 

*/
constant native ConvertGameSpeed            takes integer i returns gamespeed

/**
Returns the placement that corresponds to the given integer.
@param i The integer representation of the placement.

@pure 

*/
constant native ConvertPlacement            takes integer i returns placement

/**
Returns the startlocprio that corresponds to the given integer.
@param i The integer representation of the startlocprio.

@pure 

*/
constant native ConvertStartLocPrio         takes integer i returns startlocprio

/**
Returns the gamedifficulty that corresponds to the given integer.
@param i The integer representation of the gamedifficulty.

@pure 

*/
constant native ConvertGameDifficulty       takes integer i returns gamedifficulty

/**
Returns the gametype that corresponds to the given integer.
@param i The integer representation of the gametype.

@pure 

*/
constant native ConvertGameType             takes integer i returns gametype

/**
Returns the mapflag that corresponds to the given integer.
@param i The integer representation of the mapflag.

@pure 

*/
constant native ConvertMapFlag              takes integer i returns mapflag

/**
Returns the mapvisibility that corresponds to the given integer.
@param i The integer representation of the mapvisibility.

@pure 

*/
constant native ConvertMapVisibility        takes integer i returns mapvisibility

/**
Returns the mapsetting that corresponds to the given integer.
@param i The integer representation of the mapsetting.

@pure 

*/
constant native ConvertMapSetting           takes integer i returns mapsetting

/**
Returns the mapdensity that corresponds to the given integer.
@param i The integer representation of the mapdensity.

@pure 

*/
constant native ConvertMapDensity           takes integer i returns mapdensity

/**
Returns the mapcontrol that corresponds to the given integer.
@param i The integer representation of the mapcontrol.

@pure 

*/
constant native ConvertMapControl           takes integer i returns mapcontrol

/**
Returns the playercolor that corresponds to the given integer.
@param i The integer representation of the playercolor.

@pure 

*/
constant native ConvertPlayerColor          takes integer i returns playercolor

/**
Returns the playerslotstate that corresponds to the given integer.
@param i The integer representation of the playerslotstate.

@pure 

*/
constant native ConvertPlayerSlotState      takes integer i returns playerslotstate

/**
Returns the volumegroup that corresponds to the given integer.
@param i The integer representation of the volumegroup.

@pure 

*/
constant native ConvertVolumeGroup          takes integer i returns volumegroup

/**
Returns the camerafield that corresponds to the given integer.
@param i The integer representation of the camerafield.

@pure 

*/
constant native ConvertCameraField          takes integer i returns camerafield

/**
Returns the blendmode that corresponds to the given integer.
@param i The integer representation of the blendmode.

@pure 

*/
constant native ConvertBlendMode            takes integer i returns blendmode

/**
Returns the raritycontrol that corresponds to the given integer.
@param i The integer representation of the raritycontrol.

@pure 

*/
constant native ConvertRarityControl        takes integer i returns raritycontrol

/**
Returns the texmapflags that corresponds to the given integer.
@param i The integer representation of the texmapflags.

@pure 

*/
constant native ConvertTexMapFlags          takes integer i returns texmapflags

/**
Converts a bitmask in integer i to a fog of war type. See: `fogstate`.


@note Can be used for extended typecasting.
<http://www.hiveworkshop.com/forums/j-280/t-232039/>
@pure 

*/
constant native ConvertFogState             takes integer i returns fogstate

/**
Returns the effecttype that corresponds to the given integer.
@param i The integer representation of the effecttype.

@pure 

*/
constant native ConvertEffectType           takes integer i returns effecttype

/**
Returns the version that corresponds to the given integer.
@param i The integer representation of the version.

@pure 

*/
constant native ConvertVersion              takes integer i returns version

/**
Returns the itemtype that corresponds to the given integer.
@param i The integer representation of the itemtype.

@pure 

*/
constant native ConvertItemType             takes integer i returns itemtype

/**


@note Blizzard only defined attack-types 0 to 6 but there is a hidden one:
`ConvertAttackType(7)`.
<http://www.hiveworkshop.com/forums/t-269/h-227993/>
@pure 

*/
constant native ConvertAttackType           takes integer i returns attacktype

/**
Returns the damagetype that corresponds to the given integer.
@param i The integer representation of the damagetype.

@pure 

*/
constant native ConvertDamageType           takes integer i returns damagetype

/**
Returns the weapontype that corresponds to the given integer.
@param i The integer representation of the weapontype.

@pure 

*/
constant native ConvertWeaponType           takes integer i returns weapontype

/**
Returns the soundtype that corresponds to the given integer.
@param i The integer representation of the soundtype.

@pure 

*/
constant native ConvertSoundType            takes integer i returns soundtype

/**
Returns the pathingtype that corresponds to the given integer.
@param i The integer representation of the pathingtype.

@pure 

*/
constant native ConvertPathingType          takes integer i returns pathingtype

/**
Returns the mousebuttontype that corresponds to the given integer.
@param i The integer representation of the mousebuttontype.

@pure 
@patch 1.29

*/
constant native ConvertMouseButtonType      takes integer i returns mousebuttontype

/**
Returns the animtype that corresponds to the given integer.
@param i The integer representation of the animtype.

@pure 
@patch 1.30

*/
constant native ConvertAnimType             takes integer i returns animtype

/**
Returns the subanimtype that corresponds to the given integer.
@param i The integer representation of the subanimtype.

@pure 
@patch 1.30

*/
constant native ConvertSubAnimType          takes integer i returns subanimtype

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

```{.lua}
OrderId("humanbuild") == 851995 -- this order opens the human build menu
```


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

```{.lua}
OrderId2String(851995) --> returns "humanbuild" (opens human build menu)
```


@note See: `OrderId`

@pure 
@bug Always returns null after the game is loaded/if the game is a replay.
@bug Do not use this in a global initialisation (map init) as it returns null there.

*/
constant native OrderId2String              takes integer orderId           returns string
constant native UnitId                      takes string  unitIdString      returns integer

/**
**Example (Lua):** `UnitId2String( FourCC("hfoo") ) --> "footman" (internal name, not localized)`{.lua}


@note See `GetObjectName` if you need to retrieve a unit's localized pretty name by the type ID.

@bug Always returns null after the game is loaded/if the game is a replay.
@bug Do not use this in a global initialisation (on map init) as it returns null there.

*/
constant native UnitId2String               takes integer unitId            returns string

// Not currently working correctly...

/**


@bug Not working correctly.
@pure 

*/
constant native AbilityId                   takes string  abilityIdString   returns integer

/**


@bug Not working correctly.
@pure 

*/
constant native AbilityId2String            takes integer abilityId         returns string

// Looks up the "name" field for any object (unit, item, ability)

/**
Returns localized value for field "name" for the given object type ID (unit, item, ability).
In WorldEdit this is "Text - Name".

**Example (Lua):** `GetObjectName( FourCC("hfoo") ) --> "Footman"`{.lua}


@note See: `UnitId2String`.

@pure 
@async 
@bug Do not use this in a global initialisation (on map init) as it crashes the game there.

*/
constant native GetObjectName               takes integer objectId          returns string


/**
Returns the maximum number of playable player slots regardless of map options.

* Classic: 12 (hardcoded as `bj_MAX_PLAYERS`)
* Reforged: 24

@note This is only affected by WorldEditor version (>=6060) specified in the map's war3map.w3i file.
[Further reading](https://www.hiveworkshop.com/threads/success-hybrid-12-24-player-map-backwards-compatible-1-24-1-28-5-1-31.339722/).

@note See: `bj_MAX_PLAYERS`, `GetBJMaxPlayerSlots`.

@patch 1.29.0.8803
*/
constant native GetBJMaxPlayers             takes nothing returns integer

/**
Returns the zero-based ID of neutral victim player.

* Classic: 13 (hardcoded as `bj_PLAYER_NEUTRAL_VICTIM`)
* Reforged: 25

@note This is only affected by WorldEditor version (>=6060) specified in the map's war3map.w3i file.
[Further reading](https://www.hiveworkshop.com/threads/success-hybrid-12-24-player-map-backwards-compatible-1-24-1-28-5-1-31.339722/).


@note See: `bj_PLAYER_NEUTRAL_VICTIM`, `GetPlayerNeutralAggressive`, `GetBJPlayerNeutralExtra`, `GetPlayerNeutralPassive`.

@patch 1.29.0.8803
*/
constant native GetBJPlayerNeutralVictim    takes nothing returns integer

/**
Returns the zero-based ID of neutral extra player.

* Classic: 14 (hardcoded as `bj_PLAYER_NEUTRAL_EXTRA`)
* Reforged: 26

@note This is only affected by WorldEditor version (>=6060) specified in the map's war3map.w3i file.
[Further reading](https://www.hiveworkshop.com/threads/success-hybrid-12-24-player-map-backwards-compatible-1-24-1-28-5-1-31.339722/).


@note See: `bj_PLAYER_NEUTRAL_EXTRA`, `GetPlayerNeutralAggressive`, `GetPlayerNeutralPassive`, `GetBJPlayerNeutralVictim`.

@patch 1.29.0.8803
*/
constant native GetBJPlayerNeutralExtra     takes nothing returns integer

/**
Returns the maximum number of internal player slots regardless of map options.

* Classic: 16 (hardcoded as `bj_MAX_PLAYER_SLOTS`)
* Reforged: 28

@note This is only affected by WorldEditor version (>=6060) specified in the map's war3map.w3i file.
[Further reading](https://www.hiveworkshop.com/threads/success-hybrid-12-24-player-map-backwards-compatible-1-24-1-28-5-1-31.339722/).

@note See: `bj_MAX_PLAYER_SLOTS`, `GetBJMaxPlayers`.

@patch 1.29.0.8803
*/
constant native GetBJMaxPlayerSlots         takes nothing returns integer

/**
Returns the zero-based ID of neutral passive player.

* Classic: 15 (hardcoded as `PLAYER_NEUTRAL_PASSIVE`)
* Reforged: 27

@note This is only affected by WorldEditor version (>=6060) specified in the map's war3map.w3i file.
[Further reading](https://www.hiveworkshop.com/threads/success-hybrid-12-24-player-map-backwards-compatible-1-24-1-28-5-1-31.339722/).

See: `PLAYER_NEUTRAL_PASSIVE`, `GetPlayerNeutralAggressive`, `GetBJPlayerNeutralExtra`, `GetBJPlayerNeutralVictim`.

@patch 1.29.0.8803
*/
constant native GetPlayerNeutralPassive     takes nothing returns integer

/**
Returns the zero-based ID of neutral aggressive player.

* Classic: 12 (hardcoded as `PLAYER_NEUTRAL_AGGRESSIVE`)
* Reforged: 24

@note This is only affected by WorldEditor version (>=6060) specified in the map's war3map.w3i file.
[Further reading](https://www.hiveworkshop.com/threads/success-hybrid-12-24-player-map-backwards-compatible-1-24-1-28-5-1-31.339722/).

See: `PLAYER_NEUTRAL_AGGRESSIVE`, `GetBJPlayerNeutralExtra`, `GetPlayerNeutralPassive`, `GetBJPlayerNeutralVictim`.

@patch 1.29.0.8803
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

@note See: `GetPlayerNeutralPassive`, `GetPlayerNeutralAggressive`, `GetBJPlayerNeutralExtra`, `GetBJPlayerNeutralVictim`.

*/
    constant integer            PLAYER_NEUTRAL_PASSIVE          = GetPlayerNeutralPassive()

/**
Stores the zero-based ID of neutral aggressive player.

@note See: `GetPlayerNeutralAggressive`.

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

```{.lua}
trg_gameev = CreateTrigger()
-- just print the object representing EventId
TriggerAddAction(trg_gameev, function() print(GetTriggerEventId()) end)
-- register for this event
TriggerRegisterGameEvent(trg_gameev, EVENT_GAME_BUILD_SUBMENU)
```


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

/**

@bug Do not use this with `TriggerRegisterPlayerEvent` as `GetEventPlayerChatString`
will return `""`. Use `TriggerRegisterPlayerChatEvent` instead.


*/
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


/**
Currently useless, there are no functions that take `widgetevent`.

@note It was probably intended to be used in a similar way like
`TriggerRegisterUnitEvent` and `TriggerRegisterFilterUnitEvent` that take
a `unitevent`. It would have allowed to register an event for
a specific widget (unit/item/destructable).

@note See: `TriggerRegisterDeathEvent`.

*/
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


/**
See `fogstate` for an explanation.

*/
    constant fogstate       FOG_OF_WAR_MASKED               = ConvertFogState(1)

/**
See `fogstate` for an explanation.

*/
    constant fogstate       FOG_OF_WAR_FOGGED               = ConvertFogState(2)

/**
See `fogstate` for an explanation.

*/
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

    constant originframetype        ORIGIN_FRAME_GAME_UI                    = ConvertOriginFrameType(0)
    constant originframetype        ORIGIN_FRAME_COMMAND_BUTTON             = ConvertOriginFrameType(1)
    constant originframetype        ORIGIN_FRAME_HERO_BAR                   = ConvertOriginFrameType(2)
    constant originframetype        ORIGIN_FRAME_HERO_BUTTON                = ConvertOriginFrameType(3)
    constant originframetype        ORIGIN_FRAME_HERO_HP_BAR                = ConvertOriginFrameType(4)
    constant originframetype        ORIGIN_FRAME_HERO_MANA_BAR              = ConvertOriginFrameType(5)
    constant originframetype        ORIGIN_FRAME_HERO_BUTTON_INDICATOR      = ConvertOriginFrameType(6)
    constant originframetype        ORIGIN_FRAME_ITEM_BUTTON                = ConvertOriginFrameType(7)
    constant originframetype        ORIGIN_FRAME_MINIMAP                    = ConvertOriginFrameType(8)
    constant originframetype        ORIGIN_FRAME_MINIMAP_BUTTON             = ConvertOriginFrameType(9)
    constant originframetype        ORIGIN_FRAME_SYSTEM_BUTTON              = ConvertOriginFrameType(10)
    constant originframetype        ORIGIN_FRAME_TOOLTIP                    = ConvertOriginFrameType(11)
    constant originframetype        ORIGIN_FRAME_UBERTOOLTIP                = ConvertOriginFrameType(12)
    constant originframetype        ORIGIN_FRAME_CHAT_MSG                   = ConvertOriginFrameType(13)
    constant originframetype        ORIGIN_FRAME_UNIT_MSG                   = ConvertOriginFrameType(14)
    constant originframetype        ORIGIN_FRAME_TOP_MSG                    = ConvertOriginFrameType(15)
    constant originframetype        ORIGIN_FRAME_PORTRAIT                   = ConvertOriginFrameType(16)
    constant originframetype        ORIGIN_FRAME_WORLD_FRAME                = ConvertOriginFrameType(17)
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

//============================================================================
// MathAPI

/**
Converts degrees into radians. This is similar to multiplying the degree value by pi / 2.

@param degrees The degree input.


@note This is slightly more accurate than multiplying the degree value
by `bj_PI / 2`. `bj_PI` has a value of 3.14159. This native uses a pi value closer to 3.141592496.

@pure 

*/
native Deg2Rad  takes real degrees returns real

/**
Converts a radian value into its degree equivalent.

@param radians The radian value to be converted.


@pure 

*/
native Rad2Deg  takes real radians returns real


/**
Takes a real value input in radians and returns its sine value. The domain of
the input is all real numbers and the range of the output is -1 to 1 inclusive.

@param radians The input radians.


@pure 

*/
native Sin      takes real radians returns real

/**
Takes a real value input in radians and returns its cosine value. The domain of
the input is all real numbers and the range of the output is -1 to 1 inclusive.

@param radians The input radians.


@pure 

*/
native Cos      takes real radians returns real

/**
Takes a real value input in radians and returns its tangent value.

@param radians The input radians.

@pure 

*/
native Tan      takes real radians returns real

// Expect values between -1 and 1...returns 0 for invalid input

/**
Arcsine, one of inverse trigonometric functions. The result is returned in
radians in range [-Pi/2;Pi/2].
Returns 0 for invalid input.

@param y A value between -1 and 1.

@pure 

*/
native Asin     takes real y returns real

/**
Arccos, one of inverse trigonometric functions. The result is returned in
radians in range [-Pi/2;Pi/2].
Returns 0 for invalid input.

@param x A value between -1 and 1.

@pure 

*/
native Acos     takes real x returns real


/**
Arctangen, one of the inverse trigonometric functions. The result is returned
in radians in range [-π/2, π/2].
Returns 0 for invalid input.

@param x A value between -1 and 1.

@pure 

*/
native Atan     takes real x returns real

// Returns 0 if x and y are both 0

/**
Arctangent function with two arguments.
The result is returned in radians in range (-Pi;Pi].
Returns 0 if x and y are both 0

@pure 

*/
native Atan2    takes real y, real x returns real

// Returns 0 if x <= 0

/**
Returns the square root of x.
If x is less than or equal to zero this returns 0.0

@param x Should be greater than or equal to 0.


@pure 

*/
native SquareRoot takes real x returns real

// computes x to the y power
// y == 0.0             => 1
// x ==0.0 and y < 0    => 0
//

/**
Computes x to the y'th power.
If y is zero this returns 1.0 and if both x is zero and y is less than zero this returns 0.0


@pure 

*/
native Pow      takes real x, real power returns real


/**


@patch 1.32
@pure 

*/
constant native MathRound takes real r returns integer

//============================================================================
// String Utility API

/**
Returns a real representation for integer i.

Lua: If i is not an integer or i is null, raises an error.


@pure 

*/
native I2R  takes integer i returns real

/**
Returns an integer representation for real r. The output will be rounded towards 0 if it is a real number.

Lua: Only raises an error if r is null.

For extermely large values the minimum/maximum representable signed integer will be returned
(e.g. for Lua: `math.mininteger`{.lua} and `math.maxinteger`{.lua})


@note NaN is not a possible value in Warcraft 3 (always reset to 1.0).

@pure 

*/
native R2I  takes real r returns integer

/**
Returns the string representation for integer i.

Lua: Raises an error if i is null or has no integer representation.


@pure 

*/
native I2S  takes integer i returns string

/**
Returns a string representation for real r with precision of 3 digits.
The real is correctly rounded to nearest to fit within the precision.

Lua: Raises an error if r is null.

**Example:**

`R2S(1.12) --> 1.120`{.lua}
Equivalent to: `R2SW(r, 0, 3)` and Lua: `string.format("%.3f", r)`{.lua}


@note See: `R2SW`.

@pure 

*/
native R2S  takes real r returns string

/**
Returns a string representation for real r with precision digits and width.
The real is correctly rounded to nearest to fit within the precision.

Lua: Raises an error if r is null.

Works similar to C/C++ [printf](https://www.cplusplus.com/reference/cstdio/printf/),
but does not support negative width (left-align with right padding).

**Example (Lua):**

```{.lua}
R2SW(31.1235, 5, 3) == "31.124"
R2SW(1, 5, 0) == "  1.0" --> two spaces followed by number
```
	

@param r The number to be converted.
@param width The width of the string. If the width of the resulting conversion
             is too small the string will be filled with spaces.
             Use 0 for no padding.
@param precision The amount of decimal places. The minimum possible precision is 1 (automatically set).


@note See: `R2S` for a simple converter with preset values.

@pure 

*/
native R2SW takes real r, integer width, integer precision returns string

/**
Returns an integer by parsing the string for a number.

For values too big or too small, returns max/min integer respectively.
For an empty string or text that doesn't start with a number, returns 0.


Lua: For null raises an error.

**Examples (Lua):**

```{.lua}
S2I("") == 0
S2I("-123") == -123
S2I("-99999999") == -2147483648
S2I("99999999") == 2147483647
S2I("123abc") == 123
S2I("abc123") == 0
S2I(nil) -- error
```

@param s The string to be converted.


@note This function only works for decimal strings. Hexadecimal or octal strings
are not supported.

@note The parser stops at the first non-number character [0-9.].
If the input string starts with some valid input but ends in invalid input
this will return the conversion of the valid part: `S2I("123asd") == 123`.

@pure 


*/
native S2I  takes string s returns integer

/**
Returns a real by parsing the string for a number.
Returns 0 for: values too big or too small, an empty string or text that doesn't start with a number.

Lua: For null raises an error.

@param s The string to be converted.


@note This function only works for decimal strings. Hexadecimal or octal strings
are not supported.

@note The parser stops at the first non-number character [0-9.] - does not support comma `,` as a decimal point.
If the input string starts with some valid input but ends in invalid input
this will return the conversion of the valid part: `S2R(".123asd") == 0.123`.

@pure 

*/
native S2R  takes string s returns real

/**
Returns the internal index of the given handle; returns 0 if `h` is `null`.

**Example:** `GetHandleId(Player(0)) -> 1048584`

@note Removing a game object does not automatically invalidate an allocated handle:

```{.lua}
uf = CreateUnit(Player(0), FourCC("hfoo"), -30, 0, 90)
GetHandleId(uf) --> 1049016
RemoveUnit(uf)
GetHandleId(uf) --> 1049016
uf = nil
GetHandleId(uf) --> 0
```

@note Sometimes the handle ID may be different between clients.

@note The handle index returned here is only a weak and not a conclusive indicator
of leaking game objects. In other words, the number may be high without an actual leak.

@param h Handle

@patch 1.24b

*/
native GetHandleId takes handle h returns integer

/**
Returns a new substring from the interval [start, end) - inclusive, exclusive.
Positions are zero-indexed.
For empty or invalid out-of-bounds values returns an empty string "" (in Lua).

For start>end returns substring beginning with start until the actual end of string.
For start<0 returns an empty string.

**Examples (Lua):**

```{.lua}
SubString("abc", 0, 0) == ""
SubString("abc", 0, 1) == "a"
SubString("abc", 2, 3) == "c"
SubString("abc", 0, 3) == "abc"
SubString("abcdef", 2, 0) == "cdef"
```

@param source Text string.
@param start Starting position, zero-indexed, inclusive.
@param end Last position, zero-indexed, exclusive.


@pure 

*/
native SubString takes string source, integer start, integer end returns string

/**
Returns the length of the string in *bytes*.
This means Unicode (non-ASCII) characters will take up and return a higher byte count than there are letters.

**Example**: `StringLength("я")` returns 2.


@pure 

*/
native StringLength takes string s returns integer

/**
Turns the text to upper/lower case and returns it. Only works for ASCII characters (A-Z), not Unicode (Дружба).

@param source Text string.

@param upper True: turn to UPPER CASE. False: turn to lower case.


@pure 

*/
native StringCase takes string source, boolean upper returns string

/**
Returns a string hash for the given string. The string is normalized before hashing.

The hash is supposed to be case-insensitive of the input string:
this works for ASCII and (Reforged) some small subset of Unicode (Latin Supplement, Cyrillic...).
Also the backslash is the same as forward slash: `/` and `\`.
A probable explanation for this is the usage of file paths, since the game runs on Windows and Mac OS/OSX.
StringHash is also used for variable lookup: string name -> integer index.

`StringHash("\\") == StringHash("/")`
`StringHash("AB") == StringHash("ab")`


@note Code for the algorithm ["SStrHash2"](https://www.hiveworkshop.com/threads/bits-of-interest.213272/) via ["1997 Dr Dobbs article"](http://burtleburtle.net/bob/hash/doobs.html).

@note *Breaking:* The hashing of multi-byte characters (Unicode) was changed in v1.30.0/1.31.1.
It's unknown if hashes of these characters are different in old versions between Windows/Mac OS
or depends on OS-default character page settings (non-Unicode programs on Windows).

@pure 

@patch 1.24a

*/
native StringHash takes string s returns integer


/**
returns a translated string for the client's local language.
Without an available translation, returns `source`.

The result will differ between players with different languages.
Possible sources are the .fdf files and the war3map.wts file.

**Example:** `GetLocalizedString("REFORGED")` -> "Reforged"


@bug (Jass) Cannot assign it to a constant variable as it will crash the game.
`constant string foo = GetLocalizedString("bar")`

@async 

*/
native GetLocalizedString takes string source returns string

/**
Returns the `integer` hotkey for a specific game action à la `"GAMEOVER_QUIT_GAME"`.
You can look up potential values in `UI\FrameDef\GlobalStrings.fdf`.


@note To define own values you have to import a file named `war3mapMisc.txt`
into your map. A sample `war3mapMisc.txt` could look like this:

    [Hotkeys]
    ,=44
    !='!'
    A='A'
    B='B'
    C='C'
    // etc.

See also <https://www.hiveworkshop.com/threads/chrord.274579/>.

@async 

*/
native GetLocalizedHotkey takes string source returns integer

//============================================================================
// Map Setup API
//
//  These are native functions for describing the map configuration
//  these funcs should only be used in the "config" function of
//  a map script. The functions should also be called in this order
//  ( i.e. call SetPlayers before SetPlayerColor...
//


/**
Sets the map name.


@note This function shall only be used within the scope of function `config`
in war3map.j, it is executed by the game when you load the lobby/selected
the map for preview.

@note v1.32.10: WorldEditor limits map name input to 36 characters (ASCII/Unicode).

@note 
Old game versions (tested v1.0) used different sources for map name based on
where it was intended to be displayed:

- In map selection (to create a lobby), the map name embedded in HM3W map's header
was used (see legacy .w3m/.w3x file format).
- The map preview (right-hand side) runs the map's `config` code and thus
makes use of `SetMapName` and strings in .wts files.
- (Unused) Map name field in war3map.w3i

Reforged runs the configuration code in both cases. Therefore it always uses
the proper name at the expense of increasing the loading time of map selection list.

@note Supports color codes (they also affect sorting)

@note Map name length:

- Classic (1.0): Limited by total text width, e.g. `DescriptionFirstL...`
- Reforged (1.32.10): Up to two lines, then limited by text width, e.g. `VeryLongMapName-ABCDEFGHIJKLMNOP...`

*/
native SetMapName           takes string name returns nothing

/**
Sets the map description.


@note This function shall only be used within the scope of function `config`
in war3map.j, it is executed by the game when you load the lobby/selected
the map for preview.

@note The map description is saved to "war3map.w3i" too, but this field is unused.

@note Supports color codes.

@note Length limits:

- Classic (1.0): Limited by total text width,
approx. 40 latin characters per line. Automatic line breaks.
- Reforged: Seemingly no limit, the description box gets (bugged)
vertical & horizontal scroll bars along with automatic line-breaking.

@note Line limits:

- Classic (1.0): Maximum 9 lines.
- Reforged (1.32.10): Seemingly no limit.

*/
native SetMapDescription    takes string description returns nothing


/**


@note This function shall only be used within the scope of function `config`
in war3map.j, it is executed by the game when you load the lobby/selected
the map for preview.

*/
native SetTeams             takes integer teamcount returns nothing

/**


@note This function shall only be used within the scope of function `config`
in war3map.j, it is executed by the game when you load the lobby/selected
the map for preview.

@note The maximum amount of players (12 or 24) is determined by WorldEditor version specified in the map's war3map.w3i file. [Further reading](https://www.hiveworkshop.com/threads/success-hybrid-12-24-player-map-backwards-compatible-1-24-1-28-5-1-31.339722/).

*/
native SetPlayers           takes integer playercount returns nothing


/**
Defines a player's start location at the specified coordinates. The start
location determines where the camera is initially positioned. For melee maps,
it will also determine where the player's first town hall structure will be placed.

@param whichStartLoc The ID of the player for the starting location. See `GetPlayerStartLocation`.

@param x The x-coordinate of the start location.

@param y The y-coordinate of the start location.

@note This function shall only be used within the scope of function `config`
in war3map.j, it is executed by the game when you load the lobby/selected
the map for preview.
Using it elsewhere will affect the returned values of `GetStartLocationX` and
`GetStartLocationY`, but will have no effect on the camera's initial position and
the melee starting positions.


*/
native DefineStartLocation          takes integer whichStartLoc, real x, real y returns nothing

/**
Defines a player's start location at the specified location. The start
location determines where the camera is initially positioned. For melee maps,
it will also determine where the player's first town hall structure will be placed.

@param whichStartLoc The ID of the player for the starting location. See `GetPlayerStartLocation`.

@param whichLocation The location of the start location.

@note This function shall only be used within the scope of function `config`
in war3map.j, it is executed by the game when you load the lobby/selected
the map for preview.
Using it elsewhere will affect the returned values of `GetStartLocationX` and
`GetStartLocationY`, but will have no effect on the camera's initial position and
the melee starting positions.


*/
native DefineStartLocationLoc       takes integer whichStartLoc, location whichLocation returns nothing

/**


@note This function shall only be used within the scope of function `config`
in war3map.j, it is executed by the game when you load the lobby/selected
the map for preview.

*/
native SetStartLocPrioCount         takes integer whichStartLoc, integer prioSlotCount returns nothing

/**


@note This function shall only be used within the scope of function `config`
in war3map.j, it is executed by the game when you load the lobby/selected
the map for preview.

*/
native SetStartLocPrio              takes integer whichStartLoc, integer prioSlotIndex, integer otherStartLocIndex, startlocprio priority returns nothing
native GetStartLocPrioSlot          takes integer whichStartLoc, integer prioSlotIndex returns integer
native GetStartLocPrio              takes integer whichStartLoc, integer prioSlotIndex returns startlocprio

/**


@patch 1.32

*/
native SetEnemyStartLocPrioCount    takes integer whichStartLoc, integer prioSlotCount returns nothing

/**


@patch 1.32

*/
native SetEnemyStartLocPrio         takes integer whichStartLoc, integer prioSlotIndex, integer otherStartLocIndex, startlocprio priority returns nothing

native SetGameTypeSupported takes gametype whichGameType, boolean value returns nothing
native SetMapFlag           takes mapflag whichMapFlag, boolean value returns nothing

/**


@note This function shall only be used within the scope of function `config`
in war3map.j, it is executed by the game when you load the lobby/selected
the map for preview.

*/
native SetGamePlacement     takes placement whichPlacementType returns nothing

/**
Sets a new gamespeed to run the map at.

@param whichspeed The gamespeed constant to be set as new speed.
The only allowed values are: `MAP_SPEED_SLOWEST`, `MAP_SPEED_SLOW` and `MAP_SPEED_NORMAL`, because `MAP_SPEED_FAST` and `MAP_SPEED_FASTEST` are automatically reverted to normal speed.


@note See: `gamespeed` for values and mechanics.

*/
native SetGameSpeed         takes gamespeed whichspeed returns nothing
native SetGameDifficulty    takes gamedifficulty whichdifficulty returns nothing
native SetResourceDensity   takes mapdensity whichdensity returns nothing
native SetCreatureDensity   takes mapdensity whichdensity returns nothing

native GetTeams             takes nothing returns integer
native GetPlayers           takes nothing returns integer

native IsGameTypeSupported  takes gametype whichGameType returns boolean
native GetGameTypeSelected  takes nothing returns gametype
native IsMapFlagSet         takes mapflag whichMapFlag returns boolean

constant native GetGamePlacement     takes nothing returns placement

/**
Returns the currently set gamespeed.


@note See: `SetGameSpeed` and for values and mechanics `gamespeed`.

*/
constant native GetGameSpeed         takes nothing returns gamespeed
constant native GetGameDifficulty    takes nothing returns gamedifficulty
constant native GetResourceDensity   takes nothing returns mapdensity
constant native GetCreatureDensity   takes nothing returns mapdensity
constant native GetStartLocationX    takes integer whichStartLocation returns real
constant native GetStartLocationY    takes integer whichStartLocation returns real
constant native GetStartLocationLoc  takes integer whichStartLocation returns location


native SetPlayerTeam            takes player whichPlayer, integer whichTeam returns nothing

/**


@note This function shall only be used within the scope of function `config`
in war3map.j, it is executed by the game when you load the lobby/selected
the map for preview.

*/
native SetPlayerStartLocation   takes player whichPlayer, integer startLocIndex returns nothing
// forces player to have the specified start loc and marks the start loc as occupied
// which removes it from consideration for subsequently placed players
// ( i.e. you can use this to put people in a fixed loc and then
//   use random placement for any unplaced players etc )

/**
Forces player to have the specified start loc and marks the start loc as occupied
which removes it from consideration for subsequently placed players
( i.e. you can use this to put people in a fixed loc and then
use random placement for any unplaced players etc. ).


@note This function shall only be used within the scope of function `config`
in war3map.j, it is executed by the game when you load the lobby/selected
the map for preview.

*/
native ForcePlayerStartLocation takes player whichPlayer, integer startLocIndex returns nothing

/**


@note This function is called by the game within the scope of `config`
to set each player's color.

*/
native SetPlayerColor           takes player whichPlayer, playercolor color returns nothing

/**
Sets the given alliance setting on `sourcePlayer` towards `otherPlayer`.

Example, make it so red has vision of blue's units:

```{.jass}
function ShowBlueToRed takes nothing returns nothing
   local player red = Player(0)
   local player blue = Player(1)
   // note the reversed order: "blue's vision is shared to red"
   call SetPlayerAlliance(blue, red, ALLIANCE_SHARED_VISION, true)
endfunction
```

@note Players are not required to be allies. Tested in v1.07.

However in this case after sharing control, the player with control will see controllable enemy units
colored in "green-ally" color when hovering over them. The minimap will still correctly show all of them
as enemies (for all ally color modes, see `GetAllyColorFilterState`). Enemy units will attack correctly.

@note See: `ShareEverythingWithTeam` which is a more limited version available from GUI.

@param sourcePlayer is the target being changed
@param otherPlayer is the receiver of the effect (beneficiary)
@param whichAllianceSetting handle to alliance type
@param value `true` to share, `false` to stop sharing
*/
native SetPlayerAlliance        takes player sourcePlayer, player otherPlayer, alliancetype whichAllianceSetting, boolean value returns nothing
native SetPlayerTaxRate         takes player sourcePlayer, player otherPlayer, playerstate whichResource, integer rate returns nothing

/**


@note This function shall only be used within the scope of function `config`
in war3map.j, it is executed by the game when you load the lobby/selected
the map for preview.

*/
native SetPlayerRacePreference  takes player whichPlayer, racepreference whichRacePreference returns nothing

/**


@note This function shall only be used within the scope of function `config`
in war3map.j, it is executed by the game when you load the lobby/selected
the map for preview.

*/
native SetPlayerRaceSelectable  takes player whichPlayer, boolean value returns nothing

/**


@note This function shall only be used within the scope of function `config`
in war3map.j, it is executed by the game when you load the lobby/selected
the map for preview.

*/
native SetPlayerController      takes player whichPlayer, mapcontrol controlType returns nothing
native SetPlayerName            takes player whichPlayer, string name returns nothing

native SetPlayerOnScoreScreen   takes player whichPlayer, boolean flag returns nothing

native GetPlayerTeam            takes player whichPlayer returns integer

/**
Returns an integer representation of a player's start location. If the player
has a start location on the map (regardless of whether that player slot is filled),
it will return the player's ID (e.g. Player 1 (red) will return 0, Player 2 (blue)
will return 1, and so forth). If the player does not have a start location
on the map, it will return -1.

@param whichPlayer The player of which to return the starting location.


*/
native GetPlayerStartLocation   takes player whichPlayer returns integer
native GetPlayerColor           takes player whichPlayer returns playercolor
native GetPlayerSelectable      takes player whichPlayer returns boolean
native GetPlayerController      takes player whichPlayer returns mapcontrol
native GetPlayerSlotState       takes player whichPlayer returns playerslotstate
native GetPlayerTaxRate         takes player sourcePlayer, player otherPlayer, playerstate whichResource returns integer
native IsPlayerRacePrefSet      takes player whichPlayer, racepreference pref returns boolean

/**
Returns the player name.

**Example (Lua):**

    -- assuming you play as player Red
    local name = GetPlayerName(Player(0)) --> your player name as text

If the player is not present in the game or is one of the internal players, returns localized string + one-based player number (WorldEdit-like):

    local me = GetPlayerName( Player(0) ) --> your player name as text
    local np = GetPlayerName( Player(PLAYER_NEUTRAL_PASSIVE) ) --> "Player 28"



*/
native GetPlayerName            takes player whichPlayer returns string

//============================================================================
// Timer API
//
native CreateTimer          takes nothing returns timer

/**


@bug Destroying does not pause timer, so if call of its callback is scheduled,
then callback is called with `GetElapsedTimer` being `null`.

*/
native DestroyTimer         takes timer whichTimer returns nothing

/**
Starts a previously created timer that calls a function when timeout reaches 0.

It is affected by gamespeed at any point of it execution, if the gamespeed is changed at 50% of timeout duration, the rest of the timeout will be correctly affected by new gamespeed.

@param whichTimer Handle to timer.
@param timeout Delay in seconds.
@param periodic True: repeat timer after expiration (loop).
False: timer only runs once.

@param handlerFunc Callback function to be executed when timer expires.


@note See: `GetExpiredTimer` to retrieve the handle of the expired timer inside handlerFunc.

@note The table below shows how often a 0 millisecond timer is executed
in comparison with `TriggerRegisterTimerEvent`
(aka `TriggerRegisterTimerEventPeriodic`).

| Trigger or Timer \ Tick count  |   ROC 1.0 | Reforged 1.32.10 |
|--------------------------------|----------:|-----------------:|
| 1000ms Trigger periodic        |      1 Hz |             1 Hz |
| 100ms Trigger periodic         |     10 Hz |            10 Hz |
| 20ms Trigger periodic          |     50 Hz |            50 Hz |
| 10ms Trigger periodic          |    100 Hz |           100 Hz |
| 5ms Trigger periodic           |    200 Hz |           100 Hz |
| 1ms Trigger periodic           |   1000 Hz |           100 Hz |
| 0ms Trigger periodic           |  10077 Hz |           100 Hz |
| 1ms Timer                      |   1000 Hz |          1000 Hz |
| 0ms Timer                      |  10077 Hz |         10077 Hz |

*/
native TimerStart           takes timer whichTimer, real timeout, boolean periodic, code handlerFunc returns nothing

/**


@note If passed timer is paused or has expired,
this function returns `(TimerGetTimeout - TimerGetRemaining)`.
@bug If passed timer was resumed by `ResumeTimer`,
this function returns amount of time elapsed after last resuming.

*/
native TimerGetElapsed      takes timer whichTimer returns real

/**


@note Returns remaining time of passed timer while timer is running or paused by `PauseTimer`.
@bug After non-periodic timer expires, this function returns remaining time that was at last pause of this timer.

*/
native TimerGetRemaining    takes timer whichTimer returns real
native TimerGetTimeout      takes timer whichTimer returns real
native PauseTimer           takes timer whichTimer returns nothing

/**


@note Has no effect if passed timer is running.
@bug If passed timer is paused or has expired, launches it for `TimerGetRemaining`,
and after this time is elapsed, launches it again for `TimerGetTimeout`.
After that passed timer is stopped even if it is periodic.

*/
native ResumeTimer          takes timer whichTimer returns nothing

/**


@bug Returns `null` if timer is destroyed right before callback call.
@bug Might crash the game if called when there is no expired timer.
<http://www.wc3c.net/showthread.php?t=84131>

*/
native GetExpiredTimer      takes nothing returns timer

//============================================================================
// Group API
//
native CreateGroup                          takes nothing returns group

/**
Destroys the group.

Accessing a destroyed group shows no units, a size of 0 and cannot be modified in any way.


*/
native DestroyGroup                         takes group whichGroup returns nothing

/**
Appends unit at the end of group, increasing size by 1 (see `BlzGroupGetSize`).
Returns true if the unit was added, false if the unit is already in the group or the group is destroyed.

Even if there's a null "hole" at index 0, the unit will still be added at the tail end.

@param whichGroup Target group.
@param whichUnit Target unit.


*/
native GroupAddUnit                         takes group whichGroup, unit whichUnit returns boolean

/**
Removes unit from group, returns true on success; returns false on failure (no operation).

If unit is null, does nothing and returns false regardless if there're null values at any index in the group (does not remove destroyed units which are still in group).


*/
native GroupRemoveUnit                      takes group whichGroup, unit whichUnit returns boolean

/**
Adds a target addGroup to the desired whichGroup immediately.


@patch 1.31

*/
native BlzGroupAddGroupFast                 takes group whichGroup, group addGroup returns integer

/**


@patch 1.31

*/
native BlzGroupRemoveGroupFast              takes group whichGroup, group removeGroup returns integer

/**
Erase every unit from the group, it becomes size = 0.


*/
native GroupClear                           takes group whichGroup returns nothing

/**
Returns the size (length) of group.
The size refers to game's internal representation of group data (array), group's last index is `size - 1`.


@note See: `BlzGroupUnitAt`.
@patch 1.31

*/
native BlzGroupGetSize                      takes group whichGroup returns integer

/**
Returns unit at the given index in group. Groups start at index 0.

If the unit was removed from the game or index is out of bounds, returns null.


@patch 1.31

*/
native BlzGroupUnitAt                       takes group whichGroup, integer index returns unit
native GroupEnumUnitsOfType                 takes group whichGroup, string unitname, boolexpr filter returns nothing

/**


@note In contrast to other Enum-functions this function enumarates units with locust.

*/
native GroupEnumUnitsOfPlayer               takes group whichGroup, player whichPlayer, boolexpr filter returns nothing

/**


@bug Causes irregular behavior when used with large numbers.
@note *Probably* countLimit doesn't work similar to `GroupEnumUnitsInRangeCounted`. Instead see `GroupEnumUnitsOfType`.

*/
native GroupEnumUnitsOfTypeCounted          takes group whichGroup, string unitname, boolexpr filter, integer countLimit returns nothing
native GroupEnumUnitsInRect                 takes group whichGroup, rect r, boolexpr filter returns nothing

/**


@bug Causes irregular behavior when used with large numbers.
@note *Probably* countLimit doesn't work similar to `GroupEnumUnitsInRangeCounted`. Instead see `GroupEnumUnitsInRect`.

*/
native GroupEnumUnitsInRectCounted          takes group whichGroup, rect r, boolexpr filter, integer countLimit returns nothing

/**
Adds units within radius of map coordinates X, Y who match filter to whichGroup.
A null as filter means that every nearby unit is added to group.

If the group has had units previously, it will be first cleared (old units will not be preserved).
A group that has been destroyed will not be recreated.

@param whichGroup Group to add units to.
@param x X map coordinate.
@param y Y map coordinate.
@param radius Radius in map units.
@param filter Filter function.


@note See: `GroupEnumUnitsInRect`, `GroupEnumUnitsInRangeOfLoc`.

*/
native GroupEnumUnitsInRange                takes group whichGroup, real x, real y, real radius, boolexpr filter returns nothing
native GroupEnumUnitsInRangeOfLoc           takes group whichGroup, location whichLocation, real radius, boolexpr filter returns nothing

/**


@bug Causes irregular behavior when used with large numbers.
@bug countLimit does not work, tested in 1.32.10.18067. Therefore behaves like `GroupEnumUnitsInRange` adding all units in range.

*/
native GroupEnumUnitsInRangeCounted         takes group whichGroup, real x, real y, real radius, boolexpr filter, integer countLimit returns nothing

/**


@bug Causes irregular behavior when used with large numbers.
@note *Probably* countLimit doesn't work similar to `GroupEnumUnitsInRangeCounted`. Instead see `GroupEnumUnitsInRangeOfLoc`.

*/
native GroupEnumUnitsInRangeOfLocCounted    takes group whichGroup, location whichLocation, real radius, boolexpr filter, integer countLimit returns nothing

/**
@param whichGroup Should be an empty group.

@note Must call `SyncSelections` before this to have up-to-date players' selections.
*/
native GroupEnumUnitsSelected               takes group whichGroup, player whichPlayer, boolexpr filter returns nothing

native GroupImmediateOrder                  takes group whichGroup, string order returns boolean
native GroupImmediateOrderById              takes group whichGroup, integer order returns boolean
native GroupPointOrder                      takes group whichGroup, string order, real x, real y returns boolean
native GroupPointOrderLoc                   takes group whichGroup, string order, location whichLocation returns boolean
native GroupPointOrderById                  takes group whichGroup, integer order, real x, real y returns boolean
native GroupPointOrderByIdLoc               takes group whichGroup, integer order, location whichLocation returns boolean
native GroupTargetOrder                     takes group whichGroup, string order, widget targetWidget returns boolean
native GroupTargetOrderById                 takes group whichGroup, integer order, widget targetWidget returns boolean

// This will be difficult to support with potentially disjoint, cell-based regions
// as it would involve enumerating all the cells that are covered by a particularregion
// a better implementation would be a trigger that adds relevant units as they enter
// and removes them if they leave...
native ForGroup                 takes group whichGroup, code callback returns nothing

/**
Returns the unit at the first position in group or null if that unit no longer exists.

Equivalent to: `BlzGroupUnitAt(varGroup, 0)`.


@bug If the first unit of this group was removed from the game (RemoveUnit or decayed) then null be returned, regardless if there're valid units in group at further indeces. To iterate over all existing units of a group, use `ForGroup`/`ForGroupBJ`.
You cannot remove such null "holes" from a group without destroying or clearing it (`DestroyGroup`/`GroupClear`).
If you use FirstOfGroup in iterations with removal, units in the group will eventually leak.

@note See [GroupUtils Library](https://web.archive.org/web/20200918161954/http://wc3c.net/showthread.php?t=104464) for vJass.

*/
native FirstOfGroup             takes group whichGroup returns unit

//============================================================================
// Force API
//

/**
Creates an empty force object, returns a handle to it.

Forces are groups containing players.
To add/remove a player, see `ForceAddPlayer`/`ForceRemovePlayer`.
*/
native CreateForce              takes nothing returns force

/**
Destroys the force. Any further actions on it will have no effect.

For example, checks if player is part of force will return false, enums will not iterate.
*/
native DestroyForce             takes force whichForce returns nothing

/**
Adds player to force.
*/
native ForceAddPlayer           takes force whichForce, player whichPlayer returns nothing
native ForceRemovePlayer        takes force whichForce, player whichPlayer returns nothing

/**


@patch 1.31

*/
native BlzForceHasPlayer        takes force whichForce, player whichPlayer returns boolean
native ForceClear               takes force whichForce returns nothing

/**
Populates the force by iterating all existing players and AI (excluding player neutral etc.) and adds them to force if filter returned true.

Calling `GetFilterPlayer` will return the current player, see `Filter`.

@note If you only want to iterate the force without changing it, use `ForForce`.
*/
native ForceEnumPlayers         takes force whichForce, boolexpr filter returns nothing

/**


@note *Probably* countLimit doesn't work similar to `GroupEnumUnitsInRangeCounted`. Instead see `ForceEnumPlayers`.

*/
native ForceEnumPlayersCounted  takes force whichForce, boolexpr filter, integer countLimit returns nothing
native ForceEnumAllies          takes force whichForce, player whichPlayer, boolexpr filter returns nothing
native ForceEnumEnemies         takes force whichForce, player whichPlayer, boolexpr filter returns nothing

/**
Executes a callback function for every player in a given force. Within the callback, calling `GetEnumPlayer` returns the player of the current iteration.

@note: The iteration order is given by the player id, ascending (e.g., `Player(3)`, then `Player(7)`, then `Player(15)`) regardless in which order the players were added to the force.
*/
native ForForce                 takes force whichForce, code callback returns nothing

//============================================================================
// Region and Location API
//

/**
Returns a new rectangle as defined by two points (minX, minY) and (maxX, maxY).

The rectangle size and coordinates are limited to valid map coordinates, see
`GetWorldBounds`.

In Warcraft 3 the coordinates follow the regular cartesian system you know from
school math. The minimum coordinates (towards negative infinity) are on the left/bottom,
the maximum coordinates on right/top (towards positive infinity).

In the following graphic the N stands for the minimum point (minX, minY) and
X for the maximum point (maxX, maxY).

    +----X
    |    |
    |    |
    N----+
	

@bug You can't create your own rectangle that would match the dimensions
of `GetWorldBounds`. The maxX and maxY will be smaller by `32.0` than that of
the world bounds.

@note See: `RectFromLoc`, `RemoveRect`, `GetWorldBounds`.


*/
native Rect                     takes real minx, real miny, real maxx, real maxy returns rect

/**
Returns new rectangle as defined by two locations: `min` (bottom-left) and
`max` (top-right).

The rectangle size and coordinates are limited to valid map coordinates, see
`GetWorldBounds`.


@bug You can't create your own rectangle that would match the dimensions
of `GetWorldBounds`. The maxX and maxY will be smaller by `32.0` than that of
the world bounds.

@note See: `Rect`, `RemoveRect`, `GetWorldBounds`.


*/
native RectFromLoc              takes location min, location max returns rect

/**
Destroys the rectangle.

If you access the rectangle after removal, all of its values will return zero.



*/
native RemoveRect               takes rect whichRect returns nothing

/**
Changes a rectangle's minimum and maximum points that define it.

The rectangle size and coordinates are limited to valid map coordinates, see
`GetWorldBounds`.


@bug You can't create your own rectangle that would match the dimensions
of `GetWorldBounds`. The maxX and maxY will be smaller by `32.0` than that of
the world bounds.

@note See: `Rect`, `SetRectFromLoc`, `MoveRectTo`, `MoveRectToLoc`.


*/
native SetRect                  takes rect whichRect, real minx, real miny, real maxx, real maxy returns nothing

/**
Changes a rectangle's minimum and maximum points (that define it) to those specified
by `min` and `max` locations.

Does nothing if either location is null or invalid.


@bug You can't create your own rectangle that would match the dimensions
of `GetWorldBounds`. The maxX and maxY will be smaller by `32.0` than that of
the world bounds.

@note See: `Rect`, `SetRect`, `MoveRectTo`, `MoveRectToLoc`.


*/
native SetRectFromLoc           takes rect whichRect, location min, location max returns nothing

/**
Changes the minimum and maximum point of a rectangle to make it centered around the
specified point. Thus it moves the rectangle to a new position.


@bug This can be used to move the rectangle outside of the map bounds, bypassing
the limiting checks.

@note See: `Rect`, `SetRect`, `SetRectFromLoc`, `MoveRectToLoc`.


*/
native MoveRectTo               takes rect whichRect, real newCenterX, real newCenterY returns nothing

/**
Changes the minimum and maximum point of a rectangle to make it centered around the
specified point. Thus it moves the rectangle to a new position.

Does nothing if either location is null or invalid.


@bug This can be used to move the rectangle outside of the map bounds, bypassing
the limiting checks.

@note See: `Rect`, `SetRect`, `SetRectFromLoc`, `MoveRectTo`.


*/
native MoveRectToLoc            takes rect whichRect, location newCenterLoc returns nothing


/**
Returns rectangle's center X coordinate. This is equal to `((maxX + minX)/2)`.

Returns zero if `whichRect` is null or invalid.


*/
native GetRectCenterX           takes rect whichRect returns real

/**
Returns rectangle's center Y coordinate. This is equal to `((maxY + minY)/2)`.

Returns zero if `whichRect` is null or invalid.


*/
native GetRectCenterY           takes rect whichRect returns real

/**
Returns rectangle's bottom-left X coordinate. 

Returns zero if `whichRect` is null or invalid.


*/
native GetRectMinX              takes rect whichRect returns real

/**
Returns rectangle's bottom-left Y coordinate. 

Returns zero if `whichRect` is null or invalid.


*/
native GetRectMinY              takes rect whichRect returns real

/**
Returns rectangle's top-right X coordinate. 

Returns zero if `whichRect` is null or invalid.


*/
native GetRectMaxX              takes rect whichRect returns real

/**
Returns rectangle's top-right Y coordinate. 

Returns zero if `whichRect` is null or invalid.


*/
native GetRectMaxY              takes rect whichRect returns real

native CreateRegion             takes nothing returns region
native RemoveRegion             takes region whichRegion returns nothing

native RegionAddRect            takes region whichRegion, rect r returns nothing
native RegionClearRect          takes region whichRegion, rect r returns nothing

native RegionAddCell           takes region whichRegion, real x, real y returns nothing
native RegionAddCellAtLoc      takes region whichRegion, location whichLocation returns nothing
native RegionClearCell         takes region whichRegion, real x, real y returns nothing
native RegionClearCellAtLoc    takes region whichRegion, location whichLocation returns nothing

native Location                 takes real x, real y returns location
native RemoveLocation           takes location whichLocation returns nothing
native MoveLocation             takes location whichLocation, real newX, real newY returns nothing
native GetLocationX             takes location whichLocation returns real
native GetLocationY             takes location whichLocation returns real

// This function is asynchronous. The values it returns are not guaranteed synchronous between each player.
//  If you attempt to use it in a synchronous manner, it may cause a desync.

/**


@note Reasons for returning different values might be terrain-deformations
caused by spells/abilities and different graphic settings.
Other reasons could be the rendering state of destructables and visibility differences.

@async 

*/
native GetLocationZ             takes location whichLocation returns real

native IsUnitInRegion               takes region whichRegion, unit whichUnit returns boolean
native IsPointInRegion              takes region whichRegion, real x, real y returns boolean
native IsLocationInRegion           takes region whichRegion, location whichLocation returns boolean

// Returns full map bounds, including unplayable borders, in world coordinates

/**
Returns a new instance of rectangle that spans the entire map, including
unplayable borders, in world coordinates.

Since this creates a new rectangle on each call, the rectangle object must be
destroyed manually by calling `RemoveRect`.


@note See: `Rect`, `RemoveRect`.


*/
native GetWorldBounds           takes nothing returns rect

//============================================================================
// Native trigger interface
//

/**
Creates a new blank trigger object without any events, conditions or actions.


*/
native CreateTrigger    takes nothing returns trigger

/**


@bug Do not destroy the current running Trigger (when waits are involved) as
it can cause handle stack corruption as documented [here](http://www.wc3c.net/showthread.php?t=110519).

*/
native DestroyTrigger   takes trigger whichTrigger returns nothing

/**
Resets the evaluate and execution count of the given trigger back to zero.


@note See: `GetTriggerEvalCount`, `GetTriggerExecCount`.

*/
native ResetTrigger     takes trigger whichTrigger returns nothing

/**
See `DisableTrigger`. `EnableTrigger` enables the given trigger again, so it will be fired when the events registered on it occur.


*/
native EnableTrigger    takes trigger whichTrigger returns nothing

/**
Disables the given trigger. A disabled trigger is not fired by the events registered on it but `TriggerEvaluate` and `TriggerExecute` can still be used.
This can be reversed with `EnableTrigger`.


*/
native DisableTrigger   takes trigger whichTrigger returns nothing

/**
Tells whether the given trigger is enabled. See `EnableTrigger` and `DisableTrigger`. A trigger is enabled on default.


*/
native IsTriggerEnabled takes trigger whichTrigger returns boolean


/**
Marks the given trigger to wait/no longer wait for `TriggerSleepAction`s in sub trigger executions started via `TriggerExecuteWait`.
Since this is an attribute of the execution rather than the trigger object, this affects future runs of the given trigger, and not
those already started.


*/
native TriggerWaitOnSleeps   takes trigger whichTrigger, boolean flag returns nothing

/**
Tells whether the given trigger waits for `TriggerSleepAction`s in sub trigger executions started via `TriggerExecuteWait`.
See `TriggerWaitOnSleeps`.


*/
native IsTriggerWaitOnSleeps takes trigger whichTrigger returns boolean


/**
This returns the current unit in calls to the `GroupEnumUnits-`natives.


*/
constant native GetFilterUnit       takes nothing returns unit

/**
This returns the current unit in calls to the `ForGroup` native.


*/
constant native GetEnumUnit         takes nothing returns unit

constant native GetFilterDestructable   takes nothing returns destructable
constant native GetEnumDestructable     takes nothing returns destructable

constant native GetFilterItem           takes nothing returns item
constant native GetEnumItem             takes nothing returns item


/**


@patch 1.32

*/
constant native ParseTags               takes string taggedString returns string

constant native GetFilterPlayer     takes nothing returns player
constant native GetEnumPlayer       takes nothing returns player

constant native GetTriggeringTrigger    takes nothing returns trigger
constant native GetTriggerEventId       takes nothing returns eventid

/**
Returns the count of how often this trigger was evaluated.

A trigger is evaluated each time it is called. A trigger is executed each time it is called and passes the condition. If the condition is not met, the execution count is not incremented.


@note See: `ResetTrigger`, `GetTriggerExecCount`.

*/
constant native GetTriggerEvalCount     takes trigger whichTrigger returns integer

/**
Returns the count of how often this trigger was executed.

A trigger is evaluated each time it is called. A trigger is executed each time it is called and passes the condition. If the condition is not met, the execution count is not incremented.


@note See: `ResetTrigger`, `GetTriggerEvalCount`.

*/
constant native GetTriggerExecCount     takes trigger whichTrigger returns integer


/**
Tries to find a function with the given name and calls it in a new thread.


@note If this is called in a trigger action context, `ExecuteFunc` will use that trigger, so `GetTriggeringTrigger` will return it. If `ExecuteFunc` is
called in another type of context, it will spawn a new trigger, which can be seen with `GetTriggeringTrigger`.

@bug `ExecuteFunc` does not seem to release the trigger it spawns.

@note As `ExecuteFunc` will run the target function in a trigger action context one way or another, `TriggerSleepAction` can be used.

@note Performance numbers:

- 10000 regular function calls in Jass: 3ms (300ns/call)
- 10000 regular function calls in Lua: 0.07ms (6.9ns/call)
- 10000 "ExecuteFunc" calls in Jass: ~50ms (5µs/call)

Result: plain Lua is ~43.5x and ~724x faster respectively.

Source: Unryze's test results using
[this code](https://github.com/Luashine/wc3-test-maps/blob/31138de4f481b0186ee1002481324f0003baa51b/JassTestSpeed-ujAPI-20221109.j)
and his UjAPI (Jass on 1.26a; Lua on 1.32.10 and 1.26a).

*/
native ExecuteFunc          takes string funcName returns nothing

//============================================================================
// Boolean Expr API ( for compositing trigger conditions and unit filter funcs...)
//============================================================================

/**
Always returns a new boolean expression that has the result of evaluating logical (expr1 AND expr2).


@note `boolexpr` extends from `agent` and must be explicitly destroyed with `DestroyBoolExpr` to prevent leaks.
However, most functions from blizzard.j destroy passed boolexpr automatically.

@note See: `Or`, `Not`, `Condition`, `Filter`, `DestroyBoolExpr`
*/
native And              takes boolexpr operandA, boolexpr operandB returns boolexpr

/**
Always returns a new boolean expression that has the result of evaluating logical (expr1 OR expr2).


@note `boolexpr` extends from `agent` and must be explicitly destroyed with `DestroyBoolExpr` to prevent leaks.
However, most functions from blizzard.j destroy passed boolexpr automatically.

@note See: `And`, `Not`, `Condition`, `Filter`, `DestroyBoolExpr`
*/
native Or               takes boolexpr operandA, boolexpr operandB returns boolexpr

/**
Always returns a new boolean expression that has the result of evaluating logical (NOT expr1).

@note `boolexpr` extends from `agent` and must be explicitly destroyed with `DestroyBoolExpr` to prevent leaks.
However, most functions from blizzard.j destroy passed boolexpr automatically.

@note See: `And`, `Or`, `Condition`, `Filter`, `DestroyBoolExpr`
*/
native Not              takes boolexpr operand returns boolexpr

/**
Returns a new conditionfunc, when called by the game returns the result of evaluating func().
func will receive no arguments and must return a boolean: true/false.

@param func A function that returns boolean or `null`.

@note 1.32.10, Lua: `conditionfunc` extends from `boolexpr`->`agent` and must be explicitly destroyed with `DestroyBoolExpr`/`DestroyCondition` to prevent leaks.
However, most functions from blizzard.j destroy passed boolexpr automatically.

@note **Lua:** Always returns a new handle unless the passed parameter is `nil`, in this case
it MAY return the same handle depending on unknown conditions (consecutive calls are likely to reuse previous handle).

**Jass:** Returns same handle when creating multiple filters for the same function:
`Condition(function foo) == Condition(function foo)` ("foo" can be non-constant and constant).

For this reason, do **not** destroy filterfuncs created with `Condition` in Jass,
in the best case it does nothing but in the worst case it would affect some internals.

This behavior is similar to `Condition`.

@pure 

@note See: `And`, `Or`, `Not`, `Condition`, `Filter`, `DestroyCondition`

*/
native Condition        takes code func returns conditionfunc

/**
Destroys the provided condition.


@note `conditionfunc` extends from `boolexpr`->`agent` and must be explicitly destroyed to prevent leaks.
However, most functions from blizzard.j destroy passed boolexpr automatically.

@note Only call this on conditionfuncs created via `And`,`Or`,`Not`.

@note See: `Condition`

*/
native DestroyCondition takes conditionfunc c returns nothing

/**
Returns a new filterfunc, when called by the game returns the result of evaluating func().
func will receive no arguments and must return a boolean: true/false.


@param func A filtering function that returns boolean or `null`.

@note Lua, 1.32.10: `filterfunc` extends from `boolexpr`->`agent` and must be explicitly destroyed with `DestroyBoolExpr`/`DestroyFilter` to prevent leaks.
However, most functions from blizzard.j destroy passed boolexpr automatically.

@note **Lua:** Always returns a new handle unless the passed parameter is `nil`, in this case
it MAY return the same handle depending on unknown conditions (consecutive calls are likely to reuse previous handle).

**Jass:** Returns same handle when creating multiple filters for the same function:
`Filter(function foo) == Filter(function foo)` ("foo" can be non-constant and constant).

For this reason, do **not** destroy filterfuncs created with `Filter` in Jass,
in the best case it does nothing but in the worst case it would affect some internals.

This behavior is similar to `Condition`.

@pure 

@note See: `And`, `Or`, `Not`, `Condition`, `DestroyFilter`;
`GetFilterUnit`, `GetFilterItem`, `GetFilterPlayer`, `GetFilterDestructable`.

*/
native Filter           takes code func returns filterfunc

/**
Destroys the provided filter function.


@note `filterfunc` extends from `boolexpr`->`agent` and must be explicitly destroyed with `DestroyBoolExpr`/`DestroyFilter` to prevent leaks.
However, most functions from blizzard.j destroy passed boolexpr automatically.

@note Only call this on filterfunc created via `And`,`Or`,`Not`.

@note See: `Filter`.

*/
native DestroyFilter    takes filterfunc f returns nothing

/**
Destroys the provided boolean expression.


@note `boolexpr` extends from `agent` and must be explicitly destroyed to prevent leaks.
However, most functions from blizzard.j destroy passed boolexpr automatically.

@note Only call this on boolexpr created via `And`,`Or`,`Not`.

@note See: `And`, `Or`, `Not`, `Condition`, `Filter`.

*/
native DestroyBoolExpr  takes boolexpr e returns nothing

//============================================================================
// Trigger Game Event API
//============================================================================

native TriggerRegisterVariableEvent takes trigger whichTrigger, string varName, limitop opcode, real limitval returns event

    // EVENT_GAME_VARIABLE_LIMIT
    //constant native string GetTriggeringVariableName takes nothing returns string

// Creates it's own timer and triggers when it expires

/**
Creates its own timer and triggers when it expires.


@note The table below shows how often `TriggerRegisterTimerEvent` aka
`TriggerRegisterTimerEventPeriodic` is executed per second with different
timeout values set.
This is in comparison with a 1ms and 0ms `timer`.

Note how its frequency was limited to 100 times per second in v1.32.x.

| Trigger or Timer \ Tick count  |   ROC 1.0 | Reforged 1.32.10 |
|--------------------------------|----------:|-----------------:|
| 1000ms Trigger periodic        |      1 Hz |             1 Hz |
| 100ms Trigger periodic         |     10 Hz |            10 Hz |
| 20ms Trigger periodic          |     50 Hz |            50 Hz |
| 10ms Trigger periodic          |    100 Hz |           100 Hz |
| 5ms Trigger periodic           |    200 Hz |           100 Hz |
| 1ms Trigger periodic           |   1000 Hz |           100 Hz |
| 0ms Trigger periodic           |  10077 Hz |           100 Hz |
| 1ms Timer                      |   1000 Hz |          1000 Hz |
| 0ms Timer                      |  10077 Hz |         10077 Hz |

See: `TimerStart`

*/
native TriggerRegisterTimerEvent takes trigger whichTrigger, real timeout, boolean periodic returns event

// Triggers when the timer you tell it about expires

/**
Attach trigger to timer t. The trigger executes each time when timer expires.
Usually used on periodic timers.

Returns event, which is not used by GUI functions.


@note See: `GetExpiredTimer` to retrieve timer inside trigger's actions.

*/
native TriggerRegisterTimerExpireEvent takes trigger whichTrigger, timer t returns event

native TriggerRegisterGameStateEvent takes trigger whichTrigger, gamestate whichState, limitop opcode, real limitval returns event

native TriggerRegisterDialogEvent       takes trigger whichTrigger, dialog whichDialog returns event
native TriggerRegisterDialogButtonEvent takes trigger whichTrigger, button whichButton returns event

//  EVENT_GAME_STATE_LIMIT

/**


@event EVENT_GAME_STATE_LIMIT

*/
constant native GetEventGameState takes nothing returns gamestate


/**
Registers to execute whichTrigger when a game event occurs.
Returns a handle to event that represents the registration, you can't do anything with those currently.

**Example (Lua):**

    trg_gameev = CreateTrigger()
    -- this will print a message when someone opens a build menu
    TriggerAddAction(trg_gameev, function() print(GetTriggerEventId()) end)
    TriggerRegisterGameEvent(trg_gameev, EVENT_GAME_BUILD_SUBMENU)
    --> new event on build menu open


@bug Registered events cannot be destroyed as an object.

*/
native TriggerRegisterGameEvent takes trigger whichTrigger, gameevent whichGameEvent returns event

// EVENT_GAME_VICTORY

/**


@event EVENT_GAME_VICTORY

*/
constant native GetWinningPlayer takes nothing returns player


native TriggerRegisterEnterRegion takes trigger whichTrigger, region whichRegion, boolexpr filter returns event

// EVENT_GAME_ENTER_REGION

/**


@event EVENT_GAME_ENTER_REGION

*/
constant native GetTriggeringRegion takes nothing returns region

/**


@event EVENT_GAME_ENTER_REGION

*/
constant native GetEnteringUnit takes nothing returns unit

// EVENT_GAME_LEAVE_REGION

native TriggerRegisterLeaveRegion takes trigger whichTrigger, region whichRegion, boolexpr filter returns event

/**


@event EVENT_GAME_LEAVE_REGION

*/
constant native GetLeavingUnit takes nothing returns unit


/**
Registers when a player clicks on the given `trackable`.


*/
native TriggerRegisterTrackableHitEvent takes trigger whichTrigger, trackable t returns event

/**
Registers when a player hovers over the given `trackable`.


*/
native TriggerRegisterTrackableTrackEvent takes trigger whichTrigger, trackable t returns event

// EVENT_COMMAND_BUTTON_CLICK

/**


@patch 1.32
@event EVENT_COMMAND_BUTTON_CLICK

*/
native TriggerRegisterCommandEvent takes trigger whichTrigger, integer whichAbility, string order returns event

/**


@patch 1.32
@event EVENT_COMMAND_BUTTON_CLICK

*/
native TriggerRegisterUpgradeCommandEvent takes trigger whichTrigger, integer whichUpgrade returns event

// EVENT_GAME_TRACKABLE_HIT
// EVENT_GAME_TRACKABLE_TRACK

/**


@event `EVENT_GAME_TRACKABLE_HIT`
@event `EVENT_GAME_TRACKABLE_TRACK`

*/
constant native GetTriggeringTrackable takes nothing returns trackable

// EVENT_DIALOG_BUTTON_CLICK

/**


@event EVENT_DIALOG_BUTTON_CLICK

*/
constant native GetClickedButton takes nothing returns button

/**


@event EVENT_DIALOG_BUTTON_CLICK

*/
constant native GetClickedDialog    takes nothing returns dialog

// EVENT_GAME_TOURNAMENT_FINISH_SOON

/**


@event EVENT_GAME_TOURNAMENT_FINISH_SOON

*/
constant native GetTournamentFinishSoonTimeRemaining takes nothing returns real

/**


@event EVENT_GAME_TOURNAMENT_FINISH_SOON

*/
constant native GetTournamentFinishNowRule takes nothing returns integer

/**


@event EVENT_GAME_TOURNAMENT_FINISH_SOON

*/
constant native GetTournamentFinishNowPlayer takes nothing returns player

/**


@event EVENT_GAME_TOURNAMENT_FINISH_SOON

*/
constant native GetTournamentScore takes player whichPlayer returns integer

// EVENT_GAME_SAVE

/**


@event EVENT_GAME_SAVE

*/
constant native GetSaveBasicFilename takes nothing returns string

//============================================================================
// Trigger Player Based Event API
//============================================================================

native TriggerRegisterPlayerEvent takes trigger whichTrigger, player  whichPlayer, playerevent whichPlayerEvent returns event

// EVENT_PLAYER_DEFEAT
// EVENT_PLAYER_VICTORY

/**


@event EVENT_PLAYER_DEFEAT
@event EVENT_PLAYER_VICTORY

*/
constant native GetTriggerPlayer takes nothing returns player

native TriggerRegisterPlayerUnitEvent takes trigger whichTrigger, player whichPlayer, playerunitevent whichPlayerUnitEvent, boolexpr filter returns event

// EVENT_PLAYER_HERO_LEVEL
// EVENT_UNIT_HERO_LEVEL

/**


@event EVENT_PLAYER_HERO_LEVEL
@event EVENT_UNIT_HERO_LEVEL

*/
constant native GetLevelingUnit takes nothing returns unit

// EVENT_PLAYER_HERO_SKILL
// EVENT_UNIT_HERO_SKILL

/**


@event EVENT_PLAYER_HERO_SKILL
@event EVENT_UNIT_HERO_SKILL

*/
constant native GetLearningUnit      takes nothing returns unit

/**


@event EVENT_PLAYER_HERO_SKILL
@event EVENT_UNIT_HERO_SKILL

*/
constant native GetLearnedSkill      takes nothing returns integer

/**


@event EVENT_PLAYER_HERO_SKILL
@event EVENT_UNIT_HERO_SKILL

*/
constant native GetLearnedSkillLevel takes nothing returns integer

// EVENT_PLAYER_HERO_REVIVABLE

/**


@event EVENT_PLAYER_HERO_REVIVABLE

*/
constant native GetRevivableUnit takes nothing returns unit

// EVENT_PLAYER_HERO_REVIVE_START
// EVENT_PLAYER_HERO_REVIVE_CANCEL
// EVENT_PLAYER_HERO_REVIVE_FINISH
// EVENT_UNIT_HERO_REVIVE_START
// EVENT_UNIT_HERO_REVIVE_CANCEL
// EVENT_UNIT_HERO_REVIVE_FINISH

/**


@event EVENT_PLAYER_HERO_REVIVE_START
@event EVENT_PLAYER_HERO_REVIVE_CANCEL
@event EVENT_PLAYER_HERO_REVIVE_FINISH
@event EVENT_UNIT_HERO_REVIVE_START
@event EVENT_UNIT_HERO_REVIVE_CANCEL
@event EVENT_UNIT_HERO_REVIVE_FINISH

*/
constant native GetRevivingUnit takes nothing returns unit

// EVENT_PLAYER_UNIT_ATTACKED

/**


@event EVENT_PLAYER_UNIT_ATTACKED
@event EVENT_UNIT_ATTACKED

*/
constant native GetAttacker takes nothing returns unit

// EVENT_PLAYER_UNIT_RESCUED

/**


@event EVENT_PLAYER_UNIT_RESCUED
@event EVENT_UNIT_RESCUEDED

*/
constant native GetRescuer  takes nothing returns unit

// EVENT_PLAYER_UNIT_DEATH

/**


@event EVENT_PLAYER_UNIT_DEATH
@event EVENT_UNIT_DEATH



*/
constant native GetDyingUnit takes nothing returns unit

/**


@event EVENT_PLAYER_UNIT_DEATH

*/
constant native GetKillingUnit takes nothing returns unit

// EVENT_PLAYER_UNIT_DECAY

/**


@event EVENT_PLAYER_UNIT_DECAY
@event EVENT_UNIT_DECAY

*/
constant native GetDecayingUnit takes nothing returns unit

// EVENT_PLAYER_UNIT_SELECTED
//constant native GetSelectedUnit takes nothing returns unit

// EVENT_PLAYER_UNIT_CONSTRUCT_START

/**


@event EVENT_PLAYER_UNIT_CONSTRUCT_START

*/
constant native GetConstructingStructure takes nothing returns unit

// EVENT_PLAYER_UNIT_CONSTRUCT_FINISH
// EVENT_PLAYER_UNIT_CONSTRUCT_CANCEL

/**


@event EVENT_PLAYER_UNIT_CONSTRUCT_FINISH
@event EVENT_PLAYER_UNIT_CONSTRUCT_CANCEL

@event EVENT_UNIT_CONSTRUCT_CANCEL

*/
constant native GetCancelledStructure takes nothing returns unit

/**


@event EVENT_PLAYER_UNIT_CONSTRUCT_FINISH
@event EVENT_PLAYER_UNIT_CONSTRUCT_CANCEL

@event EVENT_UNIT_CONSTRUCT_CANCEL
@event EVENT_UNIT_CONSTRUCT_FINISH

*/
constant native GetConstructedStructure takes nothing returns unit

// EVENT_PLAYER_UNIT_RESEARCH_START
// EVENT_PLAYER_UNIT_RESEARCH_CANCEL
// EVENT_PLAYER_UNIT_RESEARCH_FINISH

/**


@event EVENT_PLAYER_UNIT_RESEARCH_START
@event EVENT_PLAYER_UNIT_RESEARCH_CANCEL
@event EVENT_PLAYER_UNIT_RESEARCH_FINISH

*/
constant native GetResearchingUnit takes nothing returns unit

/**


@event EVENT_PLAYER_UNIT_RESEARCH_START
@event EVENT_PLAYER_UNIT_RESEARCH_CANCEL
@event EVENT_PLAYER_UNIT_RESEARCH_FINISH

*/
constant native GetResearched takes nothing returns integer

// EVENT_PLAYER_UNIT_TRAIN_START
// EVENT_PLAYER_UNIT_TRAIN_CANCEL

/**


@event EVENT_PLAYER_UNIT_TRAIN_START
@event EVENT_PLAYER_UNIT_TRAIN_CANCEL

@event EVENT_UNIT_TRAIN_START
@event EVENT_UNIT_TRAIN_CANCELLED
@event EVENT_UNIT_TRAIN_FINISH

*/
constant native GetTrainedUnitType takes nothing returns integer

// EVENT_PLAYER_UNIT_TRAIN_FINISH

/**


@event EVENT_PLAYER_UNIT_TRAIN_FINISH
@event EVENT_UNIT_TRAIN_FINISH

*/
constant native GetTrainedUnit takes nothing returns unit

// EVENT_PLAYER_UNIT_DETECTED

/**


@event EVENT_PLAYER_UNIT_DETECTED

*/
constant native GetDetectedUnit takes nothing returns unit

// EVENT_PLAYER_UNIT_SUMMONED

/**


@event EVENT_PLAYER_UNIT_SUMMONED

*/
constant native GetSummoningUnit    takes nothing returns unit

/**


@event EVENT_PLAYER_UNIT_SUMMONED

*/
constant native GetSummonedUnit     takes nothing returns unit

// EVENT_PLAYER_UNIT_LOADED

/**


@event EVENT_PLAYER_UNIT_LOADED

*/
constant native GetTransportUnit    takes nothing returns unit

/**


@event EVENT_PLAYER_UNIT_LOADED

*/
constant native GetLoadedUnit       takes nothing returns unit

// EVENT_PLAYER_UNIT_SELL

/**


@event EVENT_PLAYER_UNIT_SELL
@event EVENT_UNIT_SELL

*/
constant native GetSellingUnit      takes nothing returns unit

/**


@event EVENT_PLAYER_UNIT_SELL
@event EVENT_UNIT_SELL

*/
constant native GetSoldUnit         takes nothing returns unit

/**


@event EVENT_PLAYER_UNIT_SELL
@event EVENT_UNIT_SELL

*/
constant native GetBuyingUnit       takes nothing returns unit

// EVENT_PLAYER_UNIT_SELL_ITEM

/**


@event EVENT_PLAYER_UNIT_SELL_ITEM

*/
constant native GetSoldItem         takes nothing returns item

// EVENT_PLAYER_UNIT_CHANGE_OWNER

/**


@event EVENT_PLAYER_UNIT_CHANGE_OWNER

*/
constant native GetChangingUnit             takes nothing returns unit

/**


@event EVENT_PLAYER_UNIT_CHANGE_OWNER

*/
constant native GetChangingUnitPrevOwner    takes nothing returns player

// EVENT_PLAYER_UNIT_DROP_ITEM
// EVENT_PLAYER_UNIT_PICKUP_ITEM
// EVENT_PLAYER_UNIT_USE_ITEM

/**


@event EVENT_PLAYER_UNIT_DROP_ITEM
@event EVENT_PLAYER_UNIT_PICKUP_ITEM
@event EVENT_PLAYER_UNIT_USE_ITEM

@event EVENT_UNIT_DROP_ITEM
@event EVENT_UNIT_PICKUP_ITEM
@event EVENT_UNIT_USE_ITEM

*/
constant native GetManipulatingUnit takes nothing returns unit

/**


@event EVENT_PLAYER_UNIT_DROP_ITEM
@event EVENT_PLAYER_UNIT_PICKUP_ITEM
@event EVENT_PLAYER_UNIT_USE_ITEM

@event EVENT_UNIT_DROP_ITEM
@event EVENT_UNIT_PICKUP_ITEM
@event EVENT_UNIT_USE_ITEM

*/
constant native GetManipulatedItem  takes nothing returns item

// For EVENT_PLAYER_UNIT_PICKUP_ITEM, returns the item absorbing the picked up item in case it is stacking.
// Returns null if the item was a powerup and not a stacking item.

/**
For EVENT_PLAYER_UNIT_PICKUP_ITEM, returns the item absorbing the picked up item in case it is stacking.
Returns null if the item was a powerup and not a stacking item.


@event EVENT_PLAYER_UNIT_PICKUP_ITEM
@patch 1.32.3

*/
constant native BlzGetAbsorbingItem takes nothing returns item

/**


@event EVENT_PLAYER_UNIT_PICKUP_ITEM
@patch 1.32.3

*/
constant native BlzGetManipulatedItemWasAbsorbed takes nothing returns boolean

// EVENT_PLAYER_UNIT_STACK_ITEM
// Source is the item that is losing charges, Target is the item getting charges.

/**
Source is the item that is losing charges.


@event EVENT_PLAYER_UNIT_STACK_ITEM
@patch 1.32.3

*/
constant native BlzGetStackingItemSource takes nothing returns item

/**
Target is the item getting charges.


@event EVENT_PLAYER_UNIT_STACK_ITEM
@patch 1.32.3

*/
constant native BlzGetStackingItemTarget takes nothing returns item

/**


@event EVENT_PLAYER_UNIT_STACK_ITEM
@patch 1.32.3

*/
constant native BlzGetStackingItemTargetPreviousCharges takes nothing returns integer

// EVENT_PLAYER_UNIT_ISSUED_ORDER

/**


@event EVENT_PLAYER_UNIT_ISSUED_ORDER

@event EVENT_UNIT_ISSUED_ORDER
@event EVENT_UNIT_ISSUED_POINT_ORDER
@event EVENT_UNIT_ISSUED_TARGET_ORDER

*/
constant native GetOrderedUnit takes nothing returns unit

/**


@event EVENT_PLAYER_UNIT_ISSUED_ORDER

@event EVENT_UNIT_ISSUED_ORDER
@event EVENT_UNIT_ISSUED_POINT_ORDER
@event EVENT_UNIT_ISSUED_TARGET_ORDER

*/
constant native GetIssuedOrderId takes nothing returns integer

// EVENT_PLAYER_UNIT_ISSUED_POINT_ORDER

/**


@event EVENT_PLAYER_UNIT_ISSUED_POINT_ORDER
@event EVENT_UNIT_ISSUED_POINT_ORDER

*/
constant native GetOrderPointX takes nothing returns real

/**


@event EVENT_PLAYER_UNIT_ISSUED_POINT_ORDER
@event EVENT_UNIT_ISSUED_POINT_ORDER

*/
constant native GetOrderPointY takes nothing returns real

/**


@event EVENT_PLAYER_UNIT_ISSUED_POINT_ORDER
@event EVENT_UNIT_ISSUED_POINT_ORDER

*/
constant native GetOrderPointLoc takes nothing returns location

// EVENT_PLAYER_UNIT_ISSUED_TARGET_ORDER

/**


@event EVENT_PLAYER_UNIT_ISSUED_TARGET_ORDER
@event EVENT_UNIT_ISSUED_TARGET_ORDER

*/
constant native GetOrderTarget              takes nothing returns widget

/**


@event EVENT_PLAYER_UNIT_ISSUED_TARGET_ORDER
@event EVENT_UNIT_ISSUED_TARGET_ORDER

*/
constant native GetOrderTargetDestructable  takes nothing returns destructable

/**


@event EVENT_PLAYER_UNIT_ISSUED_TARGET_ORDER
@event EVENT_UNIT_ISSUED_TARGET_ORDER

*/
constant native GetOrderTargetItem          takes nothing returns item

/**


@event EVENT_PLAYER_UNIT_ISSUED_TARGET_ORDER
@event EVENT_UNIT_ISSUED_TARGET_ORDER

*/
constant native GetOrderTargetUnit          takes nothing returns unit

// EVENT_UNIT_SPELL_CHANNEL
// EVENT_UNIT_SPELL_CAST
// EVENT_UNIT_SPELL_EFFECT
// EVENT_UNIT_SPELL_FINISH
// EVENT_UNIT_SPELL_ENDCAST
// EVENT_PLAYER_UNIT_SPELL_CHANNEL
// EVENT_PLAYER_UNIT_SPELL_CAST
// EVENT_PLAYER_UNIT_SPELL_EFFECT
// EVENT_PLAYER_UNIT_SPELL_FINISH
// EVENT_PLAYER_UNIT_SPELL_ENDCAST

/**


@event EVENT_UNIT_SPELL_CHANNEL
@event EVENT_UNIT_SPELL_CAST
@event EVENT_UNIT_SPELL_EFFECT
@event EVENT_UNIT_SPELL_FINISH
@event EVENT_UNIT_SPELL_ENDCAST
@event EVENT_PLAYER_UNIT_SPELL_CHANNEL
@event EVENT_PLAYER_UNIT_SPELL_CAST
@event EVENT_PLAYER_UNIT_SPELL_EFFECT
@event EVENT_PLAYER_UNIT_SPELL_FINISH
@event EVENT_PLAYER_UNIT_SPELL_ENDCAST

*/
constant native GetSpellAbilityUnit         takes nothing returns unit

/**


@event EVENT_UNIT_SPELL_CHANNEL
@event EVENT_UNIT_SPELL_CAST
@event EVENT_UNIT_SPELL_EFFECT
@event EVENT_UNIT_SPELL_FINISH
@event EVENT_UNIT_SPELL_ENDCAST
@event EVENT_PLAYER_UNIT_SPELL_CHANNEL
@event EVENT_PLAYER_UNIT_SPELL_CAST
@event EVENT_PLAYER_UNIT_SPELL_EFFECT
@event EVENT_PLAYER_UNIT_SPELL_FINISH
@event EVENT_PLAYER_UNIT_SPELL_ENDCAST

*/
constant native GetSpellAbilityId           takes nothing returns integer

/**


@event EVENT_UNIT_SPELL_CHANNEL
@event EVENT_UNIT_SPELL_CAST
@event EVENT_UNIT_SPELL_EFFECT
@event EVENT_UNIT_SPELL_FINISH
@event EVENT_UNIT_SPELL_ENDCAST
@event EVENT_PLAYER_UNIT_SPELL_CHANNEL
@event EVENT_PLAYER_UNIT_SPELL_CAST
@event EVENT_PLAYER_UNIT_SPELL_EFFECT
@event EVENT_PLAYER_UNIT_SPELL_FINISH
@event EVENT_PLAYER_UNIT_SPELL_ENDCAST

*/
constant native GetSpellAbility             takes nothing returns ability

/**


@event EVENT_UNIT_SPELL_CHANNEL
@event EVENT_UNIT_SPELL_CAST
@event EVENT_UNIT_SPELL_EFFECT
@event EVENT_UNIT_SPELL_FINISH
@event EVENT_UNIT_SPELL_ENDCAST
@event EVENT_PLAYER_UNIT_SPELL_CHANNEL
@event EVENT_PLAYER_UNIT_SPELL_CAST
@event EVENT_PLAYER_UNIT_SPELL_EFFECT
@event EVENT_PLAYER_UNIT_SPELL_FINISH
@event EVENT_PLAYER_UNIT_SPELL_ENDCAST

*/
constant native GetSpellTargetLoc           takes nothing returns location

/**


@event EVENT_UNIT_SPELL_CHANNEL
@event EVENT_UNIT_SPELL_CAST
@event EVENT_UNIT_SPELL_EFFECT
@event EVENT_UNIT_SPELL_FINISH
@event EVENT_UNIT_SPELL_ENDCAST
@event EVENT_PLAYER_UNIT_SPELL_CHANNEL
@event EVENT_PLAYER_UNIT_SPELL_CAST
@event EVENT_PLAYER_UNIT_SPELL_EFFECT
@event EVENT_PLAYER_UNIT_SPELL_FINISH
@event EVENT_PLAYER_UNIT_SPELL_ENDCAST

@patch 1.24b

*/
constant native GetSpellTargetX				takes nothing returns real

/**


@event EVENT_UNIT_SPELL_CHANNEL
@event EVENT_UNIT_SPELL_CAST
@event EVENT_UNIT_SPELL_EFFECT
@event EVENT_UNIT_SPELL_FINISH
@event EVENT_UNIT_SPELL_ENDCAST
@event EVENT_PLAYER_UNIT_SPELL_CHANNEL
@event EVENT_PLAYER_UNIT_SPELL_CAST
@event EVENT_PLAYER_UNIT_SPELL_EFFECT
@event EVENT_PLAYER_UNIT_SPELL_FINISH
@event EVENT_PLAYER_UNIT_SPELL_ENDCAST

@patch 1.24b

*/
constant native GetSpellTargetY				takes nothing returns real

/**


@event EVENT_UNIT_SPELL_CHANNEL
@event EVENT_UNIT_SPELL_CAST
@event EVENT_UNIT_SPELL_EFFECT
@event EVENT_UNIT_SPELL_FINISH
@event EVENT_UNIT_SPELL_ENDCAST
@event EVENT_PLAYER_UNIT_SPELL_CHANNEL
@event EVENT_PLAYER_UNIT_SPELL_CAST
@event EVENT_PLAYER_UNIT_SPELL_EFFECT
@event EVENT_PLAYER_UNIT_SPELL_FINISH
@event EVENT_PLAYER_UNIT_SPELL_ENDCAST

*/
constant native GetSpellTargetDestructable  takes nothing returns destructable

/**


@event EVENT_UNIT_SPELL_CHANNEL
@event EVENT_UNIT_SPELL_CAST
@event EVENT_UNIT_SPELL_EFFECT
@event EVENT_UNIT_SPELL_FINISH
@event EVENT_UNIT_SPELL_ENDCAST
@event EVENT_PLAYER_UNIT_SPELL_CHANNEL
@event EVENT_PLAYER_UNIT_SPELL_CAST
@event EVENT_PLAYER_UNIT_SPELL_EFFECT
@event EVENT_PLAYER_UNIT_SPELL_FINISH
@event EVENT_PLAYER_UNIT_SPELL_ENDCAST

*/
constant native GetSpellTargetItem          takes nothing returns item

/**


@event EVENT_UNIT_SPELL_CHANNEL
@event EVENT_UNIT_SPELL_CAST
@event EVENT_UNIT_SPELL_EFFECT
@event EVENT_UNIT_SPELL_FINISH
@event EVENT_UNIT_SPELL_ENDCAST
@event EVENT_PLAYER_UNIT_SPELL_CHANNEL
@event EVENT_PLAYER_UNIT_SPELL_CAST
@event EVENT_PLAYER_UNIT_SPELL_EFFECT
@event EVENT_PLAYER_UNIT_SPELL_FINISH
@event EVENT_PLAYER_UNIT_SPELL_ENDCAST

*/
constant native GetSpellTargetUnit          takes nothing returns unit

native TriggerRegisterPlayerAllianceChange takes trigger whichTrigger, player whichPlayer, alliancetype whichAlliance returns event
native TriggerRegisterPlayerStateEvent takes trigger whichTrigger, player whichPlayer, playerstate whichState, limitop opcode, real limitval returns event

// EVENT_PLAYER_STATE_LIMIT

/**


@event EVENT_PLAYER_STATE_LIMIT

*/
constant native GetEventPlayerState takes nothing returns playerstate


/**
Registers a chat event.

@param whichTrigger The trigger to which register the event.

@param whichPlayer The player on which chat-messages to react to.

@param chatMessageToDetect The message to react to. Pass `""` to react to any message.

@param exactMatchOnly `true` if only the exact string in `chatMessageToDetect`
should fire the trigger. `false` will trigger if the `chatMessageToDetect` appears
anywhere in the entered string.


@note The callback event will not have the `EVENT_PLAYER_CHAT` eventid,
instead `ConvertPlayerEvent(96)` which has no attached global in common.j.

@note Players removed by `RemovePlayer` will not fire chat events.

@event ConvertPlayerEvent(96)

*/
native TriggerRegisterPlayerChatEvent takes trigger whichTrigger, player whichPlayer, string chatMessageToDetect, boolean exactMatchOnly returns event

// EVENT_PLAYER_CHAT

// returns the actual string they typed in ( same as what you registered for
// if you required exact match )

/**
Returns the actual string they typed in ( same as what you registered for
 if you required exact match ).
Used in conjunction with `TriggerRegisterPlayerChatEvent`.


@event ConvertPlayerEvent(96)

@bug This function only returns `""` when called in response to `EVENT_PLAYER_CHAT`.

*/
constant native GetEventPlayerChatString takes nothing returns string

// returns the string that you registered for

/**
Returns the string that you registered for.
Used in conjunction with `TriggerRegisterPlayerChatEvent`.


@event ConvertPlayerEvent(96)

@bug This function only returns `""` when called in response to `EVENT_PLAYER_CHAT`.

*/
constant native GetEventPlayerChatStringMatched takes nothing returns string


/**
Makes the target trigger execute when specified widget dies.
Returns registered event.

Use `GetTriggerWidget` to retrieve the target. These work too if the widget
is of the correct sub-type: `GetTriggerUnit`, `GetTriggerDestructable`.

@param whichTrigger Register death event to execute this trigger.
@param whichWidget Trigger when this widget dies.

@note There's no "GetTriggerItem" so you have to downcast it from `widget` type.
See example.

@note **Example (Lua):** This event and trigger can be used to operate on
widgets, units, destructables, items (with typecasting).

```{.lua}
-- Create necessary widgets
u = CreateUnit(Player(0), FourCC("Hamg"), -30, 0, 90)
d = CreateDestructable(FourCC("ZTg1"), 256, 0, 90, 1, 0)
item = CreateItem(FourCC("war2"), 256, 384)

-- This is our trigger action
hasht = InitHashtable() -- for type-casting
function widgetDied()
	local w,u,d,i
	w,u,d = GetTriggerWidget(),GetTriggerUnit(),GetTriggerDestructable()
	if not u and not d then -- the widget is an item
		-- Downcasting (explicit type casting from widget to a child type)
		SaveWidgetHandle(hasht, 1, 1, w) -- put as widget
		i = LoadItemHandle(hasht, 1, 1) -- retrieve as item
	end
	print("died object (widget, unit, destr, item):", w, u, d, i)
	
	local wXpos, uXpos, dXpos, iXpos
	wXpos = GetWidgetX(w)
	if u then uXpos = GetUnitX(u) end
	if d then dXpos = GetDestructableX(d) end
	if i then iXpos = GetItemX(i) end
	print("died obj x pos (widget, unit, destr, item):", wXpos, uXpos, dXpos, iXpos)
end

-- Create and register widgets to this trigger
trig = CreateTrigger()
TriggerAddAction(trig, widgetDied)
for k,widg in pairs({u,d,item}) do TriggerRegisterDeathEvent(trig, widg) end

-- Kill widgets and observe what happens
SetWidgetLife(u, 0)
SetWidgetLife(d, 0)
SetWidgetLife(item, 0)
```

@note You can use this with units, items, destructables. Explained in `widget`.


*/
native TriggerRegisterDeathEvent takes trigger whichTrigger, widget whichWidget returns event

//============================================================================
// Trigger Unit Based Event API
//============================================================================

// returns handle to unit which triggered the most recent event when called from
// within a trigger action function...returns null handle when used incorrectly


/**
Returns handle to unit which triggered the most recent event when called from
within a trigger action function. Returns `null` handle when used incorrectly.


@note Can be used in `TriggerRegisterDeathEvent` if the dead widget is actually
a unit.

*/
constant native GetTriggerUnit takes nothing returns unit

native TriggerRegisterUnitStateEvent takes trigger whichTrigger, unit whichUnit, unitstate whichState, limitop opcode, real limitval returns event

// EVENT_UNIT_STATE_LIMIT

/**


@event EVENT_UNIT_STATE_LIMIT

*/
constant native GetEventUnitState takes nothing returns unitstate

native TriggerRegisterUnitEvent takes trigger whichTrigger, unit whichUnit, unitevent whichEvent returns event

// EVENT_UNIT_DAMAGED

/**


@event EVENT_UNIT_DAMAGED

*/
constant native GetEventDamage takes nothing returns real

/**


@event EVENT_UNIT_DAMAGED

*/
constant native GetEventDamageSource takes nothing returns unit

// EVENT_UNIT_DEATH
// EVENT_UNIT_DECAY
// Use the GetDyingUnit and GetDecayingUnit funcs above

// EVENT_UNIT_DETECTED

/**


@event EVENT_UNIT_DETECTED

*/
constant native GetEventDetectingPlayer takes nothing returns player

native TriggerRegisterFilterUnitEvent takes trigger whichTrigger, unit whichUnit, unitevent whichEvent, boolexpr filter returns event

// EVENT_UNIT_ACQUIRED_TARGET
// EVENT_UNIT_TARGET_IN_RANGE

/**


@event EVENT_UNIT_ACQUIRED_TARGET
@event EVENT_UNIT_TARGET_IN_RANGE

*/
constant native GetEventTargetUnit takes nothing returns unit

// EVENT_UNIT_ATTACKED
// Use GetAttacker from the Player Unit Event API Below...

// EVENT_UNIT_RESCUEDED
// Use GetRescuer from the Player Unit Event API Below...

// EVENT_UNIT_CONSTRUCT_CANCEL
// EVENT_UNIT_CONSTRUCT_FINISH

// See the Player Unit Construction Event API above for event info funcs

// EVENT_UNIT_TRAIN_START
// EVENT_UNIT_TRAIN_CANCELLED
// EVENT_UNIT_TRAIN_FINISH

// See the Player Unit Training Event API above for event info funcs

// EVENT_UNIT_SELL

// See the Player Unit Sell Event API above for event info funcs

// EVENT_UNIT_DROP_ITEM
// EVENT_UNIT_PICKUP_ITEM
// EVENT_UNIT_USE_ITEM
// See the Player Unit/Item manipulation Event API above for event info funcs

// EVENT_UNIT_STACK_ITEM
// See the Player Unit/Item stack Event API above for event info funcs

// EVENT_UNIT_ISSUED_ORDER
// EVENT_UNIT_ISSUED_POINT_ORDER
// EVENT_UNIT_ISSUED_TARGET_ORDER

// See the Player Unit Order Event API above for event info funcs

native TriggerRegisterUnitInRange takes trigger whichTrigger, unit whichUnit, real range, boolexpr filter returns event

native TriggerAddCondition    takes trigger whichTrigger, boolexpr condition returns triggercondition
native TriggerRemoveCondition takes trigger whichTrigger, triggercondition whichCondition returns nothing
native TriggerClearConditions takes trigger whichTrigger returns nothing


/**
Adds an action to be called when the given trigger is fired through registered events or through `TriggerExecute`.


@note More than one action can be added to the trigger. The actions run in the order they were added.

@note The same function can be used more than once on the same trigger.

@note Actions wait for their forerunner to finish. So if there are `TriggerSleepAction`s, subsequent actions will be delayed accordingly.

@note If an action execution crashes, subsequent actions will be unaffected and still be called.

@bug If an action execution crashes after a `TriggerSleepAction` in the same action execution, subsequent actions will not be run.

@note New actions added to the trigger during the execution of the actions won't be subject for execution for this run.

*/
native TriggerAddAction     takes trigger whichTrigger, code actionFunc returns triggeraction

/**
Removes an action from a trigger.


@bug If the actions of the trigger are currently running and the removed action was still pending to be called, it will still be called unless
there is a `TriggerSleepAction` after the `TriggerRemoveAction`.

*/
native TriggerRemoveAction  takes trigger whichTrigger, triggeraction whichAction returns nothing

/**
Removes all actions from a trigger.


@bug If the actions of the trigger are currently running, hereby removed actions still pending to be called will still be called. In contrast to
`TriggerRemoveAction`, this will be the case regardless if there is a `TriggerSleepAction` after `TriggerClearActions` or not.

*/
native TriggerClearActions  takes trigger whichTrigger returns nothing

/**
Makes a trigger execution sleep for a given duration. The thread will yield so other threads can do their work.


@note This works only in a trigger action execution context, not in trigger conditions nor for example in timer functions or `ForGroup` functions. However, it
also works in `ExecuteFunc` contexts, even if the `ExecuteFunc` call is not in a trigger action execution context.

@note This has many implications, see other trigger-related natives.

@note This ticks while the game was paused with the `PauseGame` native, unlike timers.

@note This does not tick while the game was paused by the user, neither in singleplayer nor in multiplayer. (But the Trigger Editor of the World Editor
denotes it as a real-time wait. Is this a bug?)

@note This ticks independently from game speed, i.e., in Fast mode, it will be about the same as a game time, in Normal mode, it will be about 25% faster than
game time and in Slow mode, it will be about 67% faster than game time, see table below.

Game speed | TSA speed (%) | game speed (%)
-----------|---------------|---------------
Fast       | 100%          | 100%
Normal     | 100%          | 80%
Slow       | 100%          | 60%

Example elapsed game time after TSA with timeout = 30:

Game speed | game time elapsed
-----------|------------------
Fast       | 30s
Normal     | 24s
Slow       | 18s

*/
native TriggerSleepAction   takes real timeout returns nothing
native TriggerWaitForSound  takes sound s, real offset returns nothing

/**
Evaluates all functions that were added to the trigger via `TriggerAddCondition`.
All return-values from all added condition-functions are `and`ed together as the final return-value.
Returns the boolean value of the return value from the condition-function.
So if the condition-functions return `0`/`0.0`/`null`, then `TriggerEvaluate`
will return `false`. Note that an empty string `""` would return `true`.


@note If a condition-function crashes the thread or does not return any value
`TriggerEvaluate` will return `false`.

@note If you want to return false for a condition-function that returns
string (for whatever reason) return `null` instead of `""`.

@note *All* functions added via `TriggerAddCondition` are run.
There is no short-circuting. If you want short-circuting use `And` or `Or`.

@note All functions added via `TriggerAddCondition` are run in the order they
were added.

*/
native TriggerEvaluate      takes trigger whichTrigger returns boolean

/**
Calls the actions of a trigger in a new execution context. Control will return to the caller when the
trigger has finished or has been suspended via `TriggerSleepAction`.


*/
native TriggerExecute       takes trigger whichTrigger returns nothing

/**
Does the same as `TriggerExecute` but if the caller has been marked with `TriggerWaitOnSleeps` before its
execution, it will additionally wait for `TriggerSleepAction`s of the callee, so this really ensures that
the callee has finished. If there was a `TriggerSleepAction`, there will be a short delay before returning.


*/
native TriggerExecuteWait   takes trigger whichTrigger returns nothing
native TriggerSyncStart     takes nothing returns nothing

/**
Waits until all currently running `SyncStoredX` (like `SyncStoredInteger`)
calls are done.


*/
native TriggerSyncReady     takes nothing returns nothing

//============================================================================
// Widget API

/**
Returns target's health points on succcess. Returns 0.0 if widget was removed or is null.

@param whichWidget target widget.

@note See: `SetWidgetLife`.
See `widget` for an explanation how this applies to units, destructables, items.


*/
native  GetWidgetLife   takes widget whichWidget returns real

/**
Sets target's health points.

It is limited by target's maximum health points. A value of ≤0 kills the target.

@param whichWidget target widget.
@param newLife set health points to this amount.

@note See: `GetWidgetLife`.
See `widget` for an explanation how this applies to units, destructables, items.


*/
native  SetWidgetLife   takes widget whichWidget, real newLife returns nothing

/**
Returns X map coordinate of widget on success. Returns 0.0 if widget was removed or is null.

@param whichWidget target widget.

@note See: `GetWidgetY`.
See `widget` for an explanation how this applies to units, destructables, items.


*/
native  GetWidgetX      takes widget whichWidget returns real

/**
Returns Y map coordinate of widget on success. Returns 0.0 if widget was removed or is null.

@param whichWidget target widget.

@note See: `GetWidgetX`.
See `widget` for an explanation how this applies to units, destructables, items.


*/
native  GetWidgetY      takes widget whichWidget returns real

/**
Returns the target widget (that caused the trigger-event) inside a trigger action.
Otherwise returns null.


@note Only works in triggers that operate on actual `widget` type, like `TriggerRegisterDeathEvent`.

*/
constant native GetTriggerWidget takes nothing returns widget

//============================================================================
// Destructable Object API
// Facing arguments are specified in degrees
native          CreateDestructable          takes integer objectid, real x, real y, real face, real scale, integer variation returns destructable

/**
Creates a destructable at the coordinates ( x , y ).

@param objectid The rawcode of the destructable to be created.
@param x The x-coordinate of the destructable.
@param y The y-coordinate of the destructable.
@param face The facing of the destructable.
@param scale The X-Y-Z scaling value of the destructable.
@param variation The integer representing the variation of the destructable to be created.


*/
native          CreateDestructableZ         takes integer objectid, real x, real y, real z, real face, real scale, integer variation returns destructable

/**
Creates the dead version of a destructable at the coordinates ( x , y ).
If the destructable has no animations, it will show the destructable's default
form. If it has a death animation, but no decay animation, then the object will
be created in memory but will not visibly appear.

@param objectid The rawcode of the destructable to be created.
@param x The x-coordinate of the destructable.
@param y The y-coordinate of the destructable.
@param face The facing of the destructable.
@param scale The X-Y-Z scaling value of the destructable.
@param variation The integer representing the variation of the destructable to be created.


*/
native          CreateDeadDestructable      takes integer objectid, real x, real y, real face, real scale, integer variation returns destructable

/**
Creates the dead version of a destructable at the coordinates ( x , y , z ).
If the destructable has no animations, it will show the destructable's default
form. If it has a death animation, but no decay animation, then the object will
be created in memory but will not visibly appear.

@param objectid The rawcode of the destructable to be created.
@param x The x-coordinate of the destructable.
@param y The y-coordinate of the destructable.
@param z The z-coordinate of the destructable.
@param face The facing of the destructable.
@param scale The X-Y-Z scaling value of the destructable.
@param variation The integer representing the variation of the destructable to be created.


*/
native          CreateDeadDestructableZ     takes integer objectid, real x, real y, real z, real face, real scale, integer variation returns destructable
native          RemoveDestructable          takes destructable d returns nothing
native          KillDestructable            takes destructable d returns nothing
native          SetDestructableInvulnerable takes destructable d, boolean flag returns nothing
native          IsDestructableInvulnerable  takes destructable d returns boolean
native          EnumDestructablesInRect     takes rect r, boolexpr filter, code actionFunc returns nothing
native          GetDestructableTypeId       takes destructable d returns integer
native          GetDestructableX            takes destructable d returns real
native          GetDestructableY            takes destructable d returns real
native          SetDestructableLife         takes destructable d, real life returns nothing
native          GetDestructableLife         takes destructable d returns real
native          SetDestructableMaxLife      takes destructable d, real max returns nothing
native          GetDestructableMaxLife      takes destructable d returns real

/**
Resurrects a destructable with the specified hit points.

@param d The destructable to resurrect. If it is not dead, there will be no effect.

@param life The amount of hit points the destructable will have when it is
resurrected. A value of 0, or any value above the destructable's maximum HP,
will give the destructable its maximum HP (as defined in the object editor).
Any value below 0.5 will give the destructable 0.5 hit points.

@param birth If true, the destructable will play its birth animation upon resurrection.



*/
native          DestructableRestoreLife     takes destructable d, real life, boolean birth returns nothing
native          QueueDestructableAnimation  takes destructable d, string whichAnimation returns nothing
native          SetDestructableAnimation    takes destructable d, string whichAnimation returns nothing
native          SetDestructableAnimationSpeed takes destructable d, real speedFactor returns nothing
native          ShowDestructable            takes destructable d, boolean flag returns nothing
native          GetDestructableOccluderHeight takes destructable d returns real
native          SetDestructableOccluderHeight takes destructable d, real height returns nothing

/**


@async 

*/
native          GetDestructableName         takes destructable d returns string

/**


@note Can be used in `TriggerRegisterDeathEvent` if the dead widget is actually
a destructable.

*/
constant native GetTriggerDestructable takes nothing returns destructable

//============================================================================
// Item API

/**
Creates an item object at the specified coordinates ( x , y ).

@param itemid The rawcode of the item.

@param x The x-coordinate of the item.

@param y The y-coordinate of the item.


*/
native          CreateItem      takes integer itemid, real x, real y returns item
native          RemoveItem      takes item whichItem returns nothing
native          GetItemPlayer   takes item whichItem returns player
native          GetItemTypeId   takes item i returns integer
native          GetItemX        takes item i returns real
native          GetItemY        takes item i returns real
native          SetItemPosition takes item i, real x, real y returns nothing
native          SetItemDropOnDeath  takes item whichItem, boolean flag returns nothing
native          SetItemDroppable takes item i, boolean flag returns nothing
native          SetItemPawnable takes item i, boolean flag returns nothing
native          SetItemPlayer    takes item whichItem, player whichPlayer, boolean changeColor returns nothing
native          SetItemInvulnerable takes item whichItem, boolean flag returns nothing
native          IsItemInvulnerable  takes item whichItem returns boolean

/**


@note 
An item can be hidden locally, but it will desync if visibility causes a local side-effect.

*/
native          SetItemVisible  takes item whichItem, boolean show returns nothing
native          IsItemVisible   takes item whichItem returns boolean
native          IsItemOwned     takes item whichItem returns boolean
native          IsItemPowerup   takes item whichItem returns boolean
native          IsItemSellable  takes item whichItem returns boolean
native          IsItemPawnable  takes item whichItem returns boolean
native          IsItemIdPowerup takes integer itemId returns boolean
native          IsItemIdSellable takes integer itemId returns boolean
native          IsItemIdPawnable takes integer itemId returns boolean
native          EnumItemsInRect     takes rect r, boolexpr filter, code actionFunc returns nothing
native          GetItemLevel    takes item whichItem returns integer
native          GetItemType     takes item whichItem returns itemtype
native          SetItemDropID   takes item whichItem, integer unitId returns nothing

/**


@async 

*/
constant native GetItemName     takes item whichItem returns string
native          GetItemCharges  takes item whichItem returns integer
native          SetItemCharges  takes item whichItem, integer charges returns nothing
native          GetItemUserData takes item whichItem returns integer
native          SetItemUserData takes item whichItem, integer data returns nothing

//============================================================================
// Unit API
// Facing arguments are specified in degrees

/**
Creates a unit of type `unitid` for player `id`, facing a certain direction at the provided coordinates.
Returns handle to unit.

**Example:** Create a human footman for first player (red) at map coordinates -30, 0, facing north:

```
    // Jass
    call CreateUnit(Player(0), 'hfoo', -30, 0, 90)
```
```{.lua}
    -- Lua
    CreateUnit(Player(0), FourCC("hfoo"), -30, 0, 90)
```
	
@param id The owner of the unit.
@param unitid The rawcode of the unit.
@param x The x-coordinate of the unit.
@param y The y-coordinate of the unit.
@param face Unit facing in degrees.

* 0   = East
* 90  = North
* 180 = West
* 270 = South
* -90 = South (wraps around)


@note See: `bj_UNIT_FACING` constant for default facing direction of units in BJ scripts and GUI.


*/
native          CreateUnit              takes player id, integer unitid, real x, real y, real face returns unit


/**
@param face Unit facing in degrees.
*/
native          CreateUnitByName        takes player whichPlayer, string unitname, real x, real y, real face returns unit


/**
@param id The owner of the unit.
@param unitid The rawcode of the unit.
@param whichLocation The position of the unit.
@param face Unit facing in degrees.
*/
native          CreateUnitAtLoc         takes player id, integer unitid, location whichLocation, real face returns unit

/**
@param face Unit facing in degrees.
*/
native          CreateUnitAtLocByName   takes player id, string unitname, location whichLocation, real face returns unit

/**
Creates the corpse of a specific unit for a player at the coordinates ( x , y ).
The unit will die upon spawning and play their decay animation, therefore they
will not necessarily be a corpse immediately after this function call. If the
unit corresponding to the rawcode cannot have a corpse, then the returned value is null.

@param whichPlayer The owner of the corpse.
@param unitid The rawcode of the unit for the corpse.
@param x The x-coordinate of the corpse.
@param y The y-coordinate of the corpse.
@param face Unit facing in degrees.


*/
native          CreateCorpse            takes player whichPlayer, integer unitid, real x, real y, real face returns unit

native          KillUnit            takes unit whichUnit returns nothing

/**

@note A comment in Blizzard.j, `ReplaceUnitBJ` states that it's "sometimes unsafe to remove hidden units",
as a workaround it calls `KillUnit` right before `RemoveUnit`.

TODO: Propagate this note to other functions that use hidden units.
*/
native          RemoveUnit          takes unit whichUnit returns nothing
native          ShowUnit            takes unit whichUnit, boolean show returns nothing


/**
Set unit's unit state to a new absolute value.

**Example:** Set unit's max mana to 105 MP.

    call SetUnitState(myUnit, UNIT_STATE_MAX_MANA, 105.0)
	

@note See: `GetUnitState`.

*/
native          SetUnitState        takes unit whichUnit, unitstate whichUnitState, real newVal returns nothing

/**


@note If the unit has movementspeed of zero the unit will be moved but the model
of the unit will not move.
@note This does not cancel orders of the unit. `SetUnitPosition` does cancel orders.

*/
native          SetUnitX            takes unit whichUnit, real newX returns nothing

/**


@note If the unit has movementspeed of zero the unit will be moved but the model
of the unit will not move.
@note This does not cancel orders of the unit. `SetUnitPosition` does cancel orders.

*/
native          SetUnitY            takes unit whichUnit, real newY returns nothing

/**


@note This cancels the orders of the unit. If you want to move a unit without
canceling its orders use `SetUnitX`/`SetUnitY`.

*/
native          SetUnitPosition     takes unit whichUnit, real newX, real newY returns nothing
native          SetUnitPositionLoc  takes unit whichUnit, location whichLocation returns nothing


/**
Makes the unit slowly turn around on the spot to look at new direction.

@param whichUnit Target unit.
@param facingAngle Unit facing in degrees.

* 0   = East
* 90  = North
* 180 = West
* 270 = South
* -90 = South (wraps around)

@note While the unit is moving, calling this function will have no effect.

*/
native          SetUnitFacing       takes unit whichUnit, real facingAngle returns nothing


/**
Makes the unit slowly turn around on the spot to look at new direction,
the turn speed can be modified with `duration`.

@note Not affected by `GetUnitTurnSpeed`/`SetUnitTurnSpeed`.

@note If `duration < 0.1`, while the unit is moving, calling this function will have no effect.

@bug For `duration == 0.5` the footman plays the running animation while turning.

```{.lua}
uf1 = CreateUnit(Player(0), FourCC("hfoo"), -128, 0, 90)
uf2 = CreateUnit(Player(0), FourCC("hfoo"), 128, 0, 90)

SetUnitFacing(uf1, -90)
SetUnitFacingTimed(uf2, -90, 0.5)
```

@bug For `duration` values other than zero, the final angle is different
than the requested angle, even when called repeatedly.

```{.lua}
SetUnitFacing(uf1, 90)
SetUnitFacingTimed(uf2, 90, 1) --> final angle = 96.91184 (expected 90)
```

@param whichUnit Target unit.

@param facingAngle New angle in degrees (direction), see: `SetUnitFacing`.

@param duration
Value >= 0 and < 1: same turn speed as `SetUnitFacing`.

Values >= 1 seem to be applied like a multiplier, slowing down the turn speed.
*/
native          SetUnitFacingTimed  takes unit whichUnit, real facingAngle, real duration returns nothing
native          SetUnitMoveSpeed    takes unit whichUnit, real newSpeed returns nothing
native          SetUnitFlyHeight    takes unit whichUnit, real newHeight, real rate returns nothing
native          SetUnitTurnSpeed    takes unit whichUnit, real newTurnSpeed returns nothing

/**
Sets a unit's propulsion window to the specified angle (in radians).

The propulsion window determines at which facing angle difference to the target
command's location (move, attack, patrol, smart) a unit will begin to move if
movement is required to fulfil the command, or if it will turn without movement.
A propulsion window of 0 makes the unit unable to move at all.
A propulsion window of 180 will force it to start moving as soon as the command
is given (if movement is required). In practice, this means that setting a
unit's prop window to 0 will prevent it from attacking.

<http://www.hiveworkshop.com/forums/2391397-post20.html>


@param whichUnit The function will modify this unit's propulsion window.
@param newPropWindowAngle The propulsion window angle to assign. Should be in radians.



*/
native          SetUnitPropWindow   takes unit whichUnit, real newPropWindowAngle returns nothing

/**
Sets a unit's acquire range.  This is the value that a unit uses to choose targets to
engage with.  Note that this is not the attack range.  When acquisition range is
greater than attack range, the unit will attempt to move towards acquired targets, and
then attack.

Setting acquisition range lower than attack range in the object editor limits the
unit's attack range to the acquisition range, but changing a unit's acquisition range
with this native does not change its attack range, nor the value displayed in the UI.


@note It is a myth that reducing acquire range with this native can limit a unit's
attack range.

*/
native          SetUnitAcquireRange takes unit whichUnit, real newAcquireRange returns nothing
native          SetUnitCreepGuard   takes unit whichUnit, boolean creepGuard returns nothing

native          GetUnitAcquireRange     takes unit whichUnit returns real
native          GetUnitTurnSpeed        takes unit whichUnit returns real

/**
Returns a unit's propulsion window angle in radians.

@param whichUnit The function will return this unit's propulsion window angle.


*/
native          GetUnitPropWindow       takes unit whichUnit returns real
native          GetUnitFlyHeight        takes unit whichUnit returns real

native          GetUnitDefaultAcquireRange      takes unit whichUnit returns real
native          GetUnitDefaultTurnSpeed         takes unit whichUnit returns real

/**
Returns a unit's default propulsion window angle in degrees.

@param whichUnit The unit of which to return the default prop window.


@note This native is the odd case in the asymmetric prop window API, since the
other prop window natives use radians. Therefore, to reset a unit's prop window
you need the explicit conversion, i.e.
`SetUnitPropWindow(u, GetUnitDefaultPropWindow(u) * bj_DEGTORAD)`.

*/
native          GetUnitDefaultPropWindow        takes unit whichUnit returns real
native          GetUnitDefaultFlyHeight         takes unit whichUnit returns real


/**
Changes ownership of a unit.

@param whichUnit Unit to modify.
@param whichPlayer The unit's new owner.
@param changeColor True to change unit's accent color to new owner's color, false to leave old color.


@note Reforged: The HP bar will always have the color of its owner player, regardless of `changeColor`.

@note See: `GetOwningPlayer`, `Player`.

*/
native          SetUnitOwner        takes unit whichUnit, player whichPlayer, boolean changeColor returns nothing

/**
Sets a unit's player color accent.

@param whichUnit Unit to modify.
@param whichColor Set to this player's color.


@bug Visual bug (tested v1.32.10): if you create two units of the same type (Normal and Colored)
and set Colored's color to a different color, then clicking between the two units
will not change the portrait color. The portrait will only update correctly if you
deselect the unit. 


*/
native          SetUnitColor        takes unit whichUnit, playercolor whichColor returns nothing


/**

@param scaleX This is actually the scale for *all* dimensions.
@param scaleY This parameter is not taken into account.
@param scaleZ This parameter is not taken into account.

@bug Only takes scaleX into account and uses scaleX for all three dimensions.

*/
native          SetUnitScale        takes unit whichUnit, real scaleX, real scaleY, real scaleZ returns nothing
native          SetUnitTimeScale    takes unit whichUnit, real timeScale returns nothing
native          SetUnitBlendTime    takes unit whichUnit, real blendTime returns nothing

/**
Sets the unit's entire model color to the color defined by (red, green, blue, alpha).

The vertex color changes how the model is rendered. For example, setting all r,g,b=0 will make the model entirely black; no colors will be visible (like Illidan's demon form).

To imagine the final result of changing vertex colors, it is helpful to think of individual RGB layers in a color image, if you disable the Red channel, only Green & Blue channels will be shown.

@param whichUnit The unit to modify.
@param red visibility of red channel (clamped to 0-255).
@param green visibility of green channel (clamped to 0-255).
@param blue visibility of blue channel (clamped to 0-255).
@param alpha opacity (clamped to 0-255). A value of 255 is total opacity (fully visible). A value of 0 is total transparency; the model will be invisible, but you'll still see the shadow, HP bar etc.


@note Not to be confused with `SetUnitColor` which changes a unit's player accent color.

*/
native          SetUnitVertexColor  takes unit whichUnit, integer red, integer green, integer blue, integer alpha returns nothing

native          QueueUnitAnimation          takes unit whichUnit, string whichAnimation returns nothing
native          SetUnitAnimation            takes unit whichUnit, string whichAnimation returns nothing
native          SetUnitAnimationByIndex     takes unit whichUnit, integer whichAnimation returns nothing
native          SetUnitAnimationWithRarity  takes unit whichUnit, string whichAnimation, raritycontrol rarity returns nothing
native          AddUnitAnimationProperties  takes unit whichUnit, string animProperties, boolean add returns nothing


/**
Locks a unit's bone to face the target until ResetUnitLookAt is called.

The offset coordinates ( X, Y, Z ) are taken from the target's origin.
The bones will lock to the lookAtTarget, offset by those coordinates. You can't
have both the head and the chest locked to the target at the same time.



@param whichUnit The unit that will have its bone locked to face the target.

@param whichBone The bone to lock onto the target. The engine only supports
locking the head and the chest. To lock the head, you can put in any input
except a null string. To lock the chest, the string must start with `"bone_chest"`.
All leading spaces are ignored, it is case insensitive, and anything after the
first non-leading space will be ignored.

@param lookAtTarget The bone will be locked to face this unit.

@param offsetX The x-offset from lookAtTarget's origin point.

@param offsetY The y-offset from lookAtTarget's origin point.

@param offsetZ The z-offset from lookAtTarget's origin point (this already factors in the terrain Z).


@note The parameter `whichBone` can only move the head bones and the chest bones.
All other input will default to the head bone. However, the function only looks
for the helper named `"Bone_Head"` (or `"Bone_Chest"`) in the MDL, so you can just
rename a helper so that it will move that set of bones instead.

@note SetUnitLookAt is affected by animation speed and blend time.

@note [How to instantly set a unit's facing](http://www.wc3c.net/showthread.php?t=105830).

*/
native          SetUnitLookAt       takes unit whichUnit, string whichBone, unit lookAtTarget, real offsetX, real offsetY, real offsetZ returns nothing

/**
Unlocks the bone oriented by `SetUnitLookAt`, allowing it to move in accordance
to the unit's regular animations.

@param whichUnit The unit that will have its bone unlocked.


*/
native          ResetUnitLookAt     takes unit whichUnit returns nothing

native          SetUnitRescuable    takes unit whichUnit, player byWhichPlayer, boolean flag returns nothing
native          SetUnitRescueRange  takes unit whichUnit, real range returns nothing


/**
Sets the hero's strength property. If the new strength property is less than current value, the hero will lose HP.


@note Hero cannot lose HP below 1.0, which means that removing X strength and then adding X strength back can result in healing.

*/
native          SetHeroStr          takes unit whichHero, integer newStr, boolean permanent returns nothing
native          SetHeroAgi          takes unit whichHero, integer newAgi, boolean permanent returns nothing
native          SetHeroInt          takes unit whichHero, integer newInt, boolean permanent returns nothing

native          GetHeroStr          takes unit whichHero, boolean includeBonuses returns integer
native          GetHeroAgi          takes unit whichHero, boolean includeBonuses returns integer
native          GetHeroInt          takes unit whichHero, boolean includeBonuses returns integer

native          UnitStripHeroLevel  takes unit whichHero, integer howManyLevels returns boolean

native          GetHeroXP           takes unit whichHero returns integer
native          SetHeroXP           takes unit whichHero, integer newXpVal,  boolean showEyeCandy returns nothing


/**
Returns the units available skill points.


*/
native          GetHeroSkillPoints      takes unit whichHero returns integer

/**
Adds the amount to the units available skill points. Calling with a negative
number reduces the skill points by that amount.
Returns false if the amount of available skill points is already zero and
if it's called with any non-positive number.
Returns true in any other case.


@note If `skillPointDelta` is greater than the amount of skillpoints the hero
actually can spend (like 9 for three 3-level abilities) only that amount will
be added. Negative `skillPointDelta` works as expected.

*/
native          UnitModifySkillPoints   takes unit whichHero, integer skillPointDelta returns boolean


/**
Adds the input value of experience to the hero unit specified.

If the experience added exceeds the amount required for the hero to gain a level,
then it will force the unit to gain a level and the remaining experience will
spill over for the next level.

@param whichHero The hero unit to add experience to.

@param xpToAdd The amount of experience to add to the hero unit.

@param showEyeCandy If the boolean input is true, then the hero-level-gain
effect will be shown if the hero gains a level from the added experience.

@bug Adding negative value to experience will decrease it
by the stated value, but won't lower the level even if the experience value
after deduction is lower than the lower bound of the experience required to get
the stated level.

@bug If the value will become lower than zero, the experience won't
be negative, instead of it it'll be equal
to `4294967296+(supposed_negative_experience_value)` which actually proves
that WarCraft III uses unsigned int type for storing experience points.


*/
native          AddHeroXP           takes unit whichHero, integer xpToAdd,   boolean showEyeCandy returns nothing

/**
Sets the hero to chosen level.

The level can only be increased; lowering the level does nothing.
Further, the level will not exceed the hero's maximum level set in WorldEditor.

@param whichHero The target hero unit.
@param level New level of the hero.
@param showEyeCandy False to hide level-up effects, true to show.
The level-up effects include: floating experience gain text, sound and a visual effect.


*/
native          SetHeroLevel        takes unit whichHero, integer level,  boolean showEyeCandy returns nothing
constant native GetHeroLevel        takes unit whichHero returns integer
constant native GetUnitLevel        takes unit whichUnit returns integer

/**
Returns the hero's "Proper Name", which is the name displayed above the level bar.


@note Will return `null` on non-hero units or `null`.

*/
native          GetHeroProperName   takes unit whichHero returns string
native          SuspendHeroXP       takes unit whichHero, boolean flag returns nothing
native          IsSuspendedXP       takes unit whichHero returns boolean

/**
Spends a skill point of the hero to learn a skill.
If the requirements are not met, does nothing.
This is equivalent to clicking the red plus button in game and choosing a skill.

Requirements:

1. The hero has an unspent skill point
2. The skill is available for learning (not level-locked etc.)

@param whichHero Target hero.
@param abilcode Abilities' raw code identifier.


*/
native          SelectHeroSkill     takes unit whichHero, integer abilcode returns nothing

/**
Returns the level of the ability for the unit.

@param whichUnit Target unit.
@param abilcode Abilities' raw code identifier.


*/
native          GetUnitAbilityLevel takes unit whichUnit, integer abilcode returns integer

/**
Decreases the level of a unit's ability by 1. The level will not go below 1.
Returns the new ability level.

@param whichUnit The unit with the ability.
@param abilcode The four digit rawcode representation of the ability.


*/
native          DecUnitAbilityLevel takes unit whichUnit, integer abilcode returns integer

/**
Increases the level of a unit's ability by 1.

Returns the new ability level.

@param whichUnit The unit with the ability.
@param abilcode The four digit rawcode representation of the ability.


@note `IncUnitAbilityLevel` can increase an abilities level to maxlevel+1.
On maxlevel+1 all ability fields are 0.
See <http://www.wc3c.net/showthread.php?p=1029039#post1029039>
and <http://www.hiveworkshop.com/forums/lab-715/silenceex-everything-you-dont-know-about-silence-274351/>.

*/
native          IncUnitAbilityLevel takes unit whichUnit, integer abilcode returns integer

/**
Sets the new level of unit's ability.

@param whichUnit Target unit.
@param abilcode Abilities' raw code identifier.
@param level New ability level.


@note You can only set levels which are defined for the current ability.
For example, most WC3 abilities have levels 1-3.
Setting level <=0 will instead set it to level 1.
Setting level >maximum will instead set it to abilities' highest level defined in WorldEditor.

@note When a unit picks up an item with an ability, the ability will be added to the unit's list of abilities. Thus, this function can be used
to set the level of an ability for an item on the unit, too. Since it's an attribute of the unit, a level set this way will not be retained
when the item is dropped and picked up again. Via item abilities, a unit can have more than one instance of ability with the same ability id.
This function will only set the level of the most recently obtained ability instance, then, which corresponds to the first ability instance found
when using `BlzGetUnitAbilityByIndex` counting upwards.
*/
native          SetUnitAbilityLevel takes unit whichUnit, integer abilcode, integer level returns integer

/**
Revives a dead hero at target coordinates, with or without special effects.

Returns true if hero was dead and revived.
Returns false otherwise (hero alive, unit isn't a hero/doesn't exist etc.).

@param whichHero Target dead hero.
@param x X map coordinate.
@param y Y map coordinate.
@param doEyecandy True to revive with revival special effects, false without.
Special effects include: sound, visual effect. 


@note See: `ReviveHeroLoc`.

*/
native          ReviveHero          takes unit whichHero, real x, real y, boolean doEyecandy returns boolean

/**
Revives a dead hero at target location, with or without special effects.

Returns true if hero was dead and revived.
Returns false otherwise (hero alive, unit isn't a hero/doesn't exist etc.)

@param loc Location on map.
@param doEyecandy True to revive with revival special effects, false without.
Special effects include: sound, visual effect. 


@note See: `ReviveHero`.

*/
native          ReviveHeroLoc       takes unit whichHero, location loc, boolean doEyecandy returns boolean
native          SetUnitExploded     takes unit whichUnit, boolean exploded returns nothing

/**
Renders a unit invulnerable/lifts that specific invulnerability.


@note The native seems to employ the `'Avul'` ability, which is defined in the
default AbilityData.slk.
If there is no `'Avul'` defined, this will crash the game.

*/
native          SetUnitInvulnerable takes unit whichUnit, boolean flag returns nothing

/**
Pauses a unit. A paused unit has the following properties:

  * Buffs/effects are suspended
  * Orders are stored when paused and fired on unpause
  * The paused unit does not accept powerups. `UnitAddItem` returns true but
    the item is not picked up


*/
native          PauseUnit           takes unit whichUnit, boolean flag returns nothing
native          IsUnitPaused        takes unit whichHero returns boolean
native          SetUnitPathing      takes unit whichUnit, boolean flag returns nothing


/**
Clears all widget selections for all players.


@note Use `ClearSelectionForPlayer` to clear selection for only one player.

*/
native          ClearSelection      takes nothing returns nothing

/**


@bug If you use this function to select a unit after calling `ClearSelection` (like `SelectUnitSingle`
does) and you have a unit sub-menu opened (like spellbook or build structure), the command buttons
of the selected unit won't show until you select another unit.

*/
native          SelectUnit          takes unit whichUnit, boolean flag returns nothing

native          GetUnitPointValue       takes unit whichUnit returns integer
native          GetUnitPointValueByType takes integer unitType returns integer
//native        SetUnitPointValueByType takes integer unitType, integer newPointValue returns nothing


/**
Puts specific item in target unit's inventory.

Returns:

- true if this exact item is already in unit's inventory or if it was put there successfully
- false if unit has no inventory or space, or invalid item/unit

@param whichUnit Target unit.
@param whichItem Handle to item instance.


*/
native          UnitAddItem             takes unit whichUnit, item whichItem returns boolean

/**
Creates a new item of type `itemId` and puts it in unit's inventory.
If the inventory is full, it is dropped on the ground at unit's position instead.

This function works in two steps:

1. Spawn the item if both `whichUnit` and `itemId` are valid and exist
2. Attempt to put the item in unit's inventory. If inventory is full or unit is
dead then the item is dropped on the ground at unit position.

Returns:

- item handle if the item was successfully placed in unit's inventory
- null if inventory is full, unit cannot carry items, itemId/unit invalid etc.

@param whichUnit Target unit.
@param itemId Item's raw code identifier.


@note See: `UnitAddItemToSlotById`.

*/
native          UnitAddItemById         takes unit whichUnit, integer itemId returns item

/**
Creates a new item of type `itemId`
and puts it in unit's inventory in slot specified by `itemSlot`.
If the slot is occupied or invalid (<0 or >6, or higher than unit's max slots),
it is dropped on the ground at unit's position instead.

This function works in two steps:

1. Spawn the item if both `whichUnit` and `itemId` are valid and exist
2. Attempt to put the item in unit's inventory at specified slot.
If the slot is occupied or unit is dead then the item is dropped on the ground
at unit position.

Returns:

- true if the item was successfully placed in unit's inventory
- false otherwise: slot occupied, unit cannot carry items, itemId/unit/itemSlot invalid etc.

@param whichUnit Target unit.
@param itemId Item's raw code identifier.
@param itemSlot Slot number (zero-based, i.e. 0 to 5).


@note See: `UnitAddItemById`.

*/
native          UnitAddItemToSlotById   takes unit whichUnit, integer itemId, integer itemSlot returns boolean

/**
If the target unit carries the item, it is removed from the inventory and dropped at unit's position.

Nothing happens if unit or item instance is invalid.

@param whichUnit Target unit.
@param whichItem Handle to item instance.


@note See `UnitRemoveItemFromSlot` to drop an item from a specific slot.

*/
native          UnitRemoveItem          takes unit whichUnit, item whichItem returns nothing

/**
If an item exists in the given slot, it is removed from the inventory and dropped at unit's position.

Returns the handle of dropped item when successful.
Returns null on failure (no item, invalid slot/unit).

@param whichUnit Target unit.
@param itemSlot Slot number (zero-based, i.e. 0 to 5).


@note See `UnitRemoveItem` to drop an item by handle.

*/
native          UnitRemoveItemFromSlot  takes unit whichUnit, integer itemSlot returns item

/**
Returns true if unit has this specific instance of item somewhere in inventory.
Returns false otherwise (null unit, item not found in inventory, null item etc).

@param whichUnit Target unit.
@param whichItem Handle to item instance.


*/
native          UnitHasItem             takes unit whichUnit, item whichItem returns boolean

/**
Returns a handle to item in slot number `itemSlot` of the specified unit.

Returns null otherwise:
when there's no item in slot, no slot (less than 6 slots), invalid slot number, invalid unit.

@param whichUnit Target unit.
@param itemSlot Slot number (zero-based, i.e. 0 to 5).


*/
native          UnitItemInSlot          takes unit whichUnit, integer itemSlot returns item

/**
Returns amount of inventory slots for unit (0 to `bj_MAX_INVENTORY` inclusive).
Returns zero if unit is invalid or has no inventory.

@param whichUnit Target unit.


*/
native          UnitInventorySize       takes unit whichUnit returns integer


/**
Issues an immediate order for the unit to go to point (x,y) and drop the item there.

If the unit cannot reach the point, it will run up to closest location and stop there,
without dropping the item.

Returns:

- true if item was found in inventory of unit and an order was issued.
- false if unit/item invalid, unit is paused and cannot take orders etc.

@param whichUnit Target unit.
@param whichItem Handle to item instance.
@param x X map coordinate.
@param y Y map coordinate.


@note See: `UnitDropItemSlot`, `UnitDropItemTarget`.

*/
native          UnitDropItemPoint       takes unit whichUnit, item whichItem, real x, real y returns boolean

/**
Moves an item inside unit's inventory to specified slot.
If this slot contains an item, their positions are swapped.

This is the same as if you'd right-click an item in game to move it to a different slot.

Returns:

- true if the move was successful, even if moving an item to its current slot (no change)
- false if unit/item invalid, item not in inventory, invalid item slot specified

@param whichUnit Target unit.
@param whichItem Handle to item instance.
@param slot Move to this slot.


*/
native          UnitDropItemSlot        takes unit whichUnit, item whichItem, integer slot returns boolean

/**
Issues an immediate order for the `whichUnit` to go to `target` (usually another unit)
and give the item to target. If the target has no inventory slots/full inventory,
the item is dropped at target's feet.

If the `whichUnit` cannot reach the target, it will run up to closest location and stop there,
without giving the item. If the target is a running unit, `whichUnit` will follow it
to give the item.

Returns:

- true if item was found in inventory of `whichUnit` and an order was issued.
- false if `whichUnit`/item/widget invalid, `whichUnit` is paused and cannot take orders etc.
Target widget can be a paused unit and will receive the item.

@param whichUnit Target unit.
@param whichItem Handle to item instance.
@param target Target unit or widget.


@note See: `UnitDropItemSlot`, `UnitDropItemPoint`.


*/
native          UnitDropItemTarget      takes unit whichUnit, item whichItem, widget target returns boolean


/**
Issues an immediate order for the unit to use the specified item.

This is the same as left-clicking and using an item in inventory.
Units that cannot use items will not do anything.

Returns:

- true if item was successfully used / an order was issued
- false otherwise, because an order was not issued:
unit doesn't have item, item on cooldown, invalid unit/item etc.

Examples:

- Potion of Healing `'phea'`:
    - Unit on patrol, but has full HP: does nothing, unit continues running
    - Unit on patrol, but has low HP: Uses potion to restore HP, stops patrolling

- Dagger of Escape `'desc'`:
is not casted, because requires a position as a target.
However, an order is issued, hence returns true.

- Inferno Stone `'infs'`: same as with dagger above.


@note See: `UnitUseItemPoint`, `UnitUseItemTarget`.


*/
native          UnitUseItem             takes unit whichUnit, item whichItem returns boolean

/**
Issues an immediate order for the unit to use item pointed at position (x,y).

This is the same as left-clicking and using an item in inventory.
Units that cannot use items will not do anything.

Examples:

- Potion of Healing `'phea'`:
Restores HP 

- Dagger of Escape `'desc'`:
Casts immediately towards (x,y), even if too far, item on cooldown.
Does not cast if position is already reached (no cooldown).

- Inferno Stone `'infs'`:
runs towards (x,y) and once in range, casts to spawn an Infernal.
If already in range, casts immediately.

@param whichUnit Target unit.
@param whichItem Handle to item instance.
@param x Point at X map coordinate to use the item.
@param y Point at Y map coordinate to use the item.


@bug Seems to always return false (tested v1.32.10).

@note See: `UnitUseItem`, `UnitUseItemTarget`.


*/
native          UnitUseItemPoint        takes unit whichUnit, item whichItem, real x, real y returns boolean

/**
Issues an immediate order for the unit to use item pointed `target` (widget or unit).

This is the similar to left-clicking and using an item in inventory.
Units that cannot use items will not do anything.

Returns:

- true if item was successfully used / an order was issued
- false otherwise, because an order was not issued:
unit doesn't have item, item on cooldown, invalid unit/item/target etc.

Examples:

- Dagger of Escape `'desc'`: does not cast.
Explanation: when you click a dagger on a building in game, the target is not
actually the building, but the map position you're pointing at, even though you
see the building being highlighted on cursor hover.

- Inferno Stone `'infs'`: does not cast, same as above.

@param whichUnit Target unit.
@param whichItem Handle to item instance.
@param target Target unit or widget.


@note See: `UnitUseItem`, `UnitUseItemPoint`.


*/
native          UnitUseItemTarget       takes unit whichUnit, item whichItem, widget target returns boolean


/**
Returns X map coordinate of whichUnit (alive or dead). Returns 0.0 if unit was removed or is null.


@bug If the unit is loaded into a zeppelin this will not return the position
of the zeppelin but the last position of the unit before it was loaded into
the zeppelin.

@note Since unit extends from `widget`, you can use widget-related functions too.
See: `GetUnitY`, `BlzGetLocalUnitZ`, `BlzGetUnitZ`, `GetWidgetX`, `GetWidgetY`.

*/
constant native GetUnitX            takes unit whichUnit returns real

/**
Returns Y map coordinate of whichUnit (alive or dead). Returns 0.0 if unit was removed or is null.


@bug If the unit is loaded into a zeppelin this will not return the position
of the zeppelin but the last position of the unit before it was loaded into
the zeppelin.

@note Since unit extends from `widget`, you can use widget-related functions too.
See: `GetUnitX`, `BlzGetLocalUnitZ`, `BlzGetUnitZ`, GetWidgetX`, `GetWidgetY`.

*/
constant native GetUnitY            takes unit whichUnit returns real

/**


@bug If the unit is loaded into a zeppelin this will not return the position
of the zeppelin but the last position of the unit before it was loaded into
the zeppelin.

*/
constant native GetUnitLoc          takes unit whichUnit returns location

/**
Returns the units facing in degrees.


*/
constant native GetUnitFacing       takes unit whichUnit returns real
constant native GetUnitMoveSpeed    takes unit whichUnit returns real
constant native GetUnitDefaultMoveSpeed takes unit whichUnit returns real

/**
Returns unit's current unit state as an absolute value.

**Example:** Retrieve a unit's current/max HP and mana:

    call GetUnitState(myUnit, UNIT_STATE_MAX_MANA) // returns 285.0
	

@note See: `SetUnitState`.

*/
constant native GetUnitState        takes unit whichUnit, unitstate whichUnitState returns real

/**
Returns the owner player of the unit.

@param whichUnit Target unit.


@note See: `SetUnitOwner`.

*/
constant native GetOwningPlayer     takes unit whichUnit returns player
constant native GetUnitTypeId       takes unit whichUnit returns integer
constant native GetUnitRace         takes unit whichUnit returns race

/**
Returns localized name for unit.

**Example (Lua)**:

```{.lua}
u = CreateUnit(Player(0), FourCC("hfoo"), -30, 0, 90)
print(GetUnitName(u)) --> "Footman"
```

@param whichUnit Target unit.

@async 

*/
constant native GetUnitName         takes unit whichUnit returns string
constant native GetUnitFoodUsed     takes unit whichUnit returns integer
constant native GetUnitFoodMade     takes unit whichUnit returns integer
constant native GetFoodMade         takes integer unitId returns integer
constant native GetFoodUsed         takes integer unitId returns integer
native          SetUnitUseFood      takes unit whichUnit, boolean useFood returns nothing

constant native GetUnitRallyPoint           takes unit whichUnit returns location
constant native GetUnitRallyUnit            takes unit whichUnit returns unit
constant native GetUnitRallyDestructable    takes unit whichUnit returns destructable

constant native IsUnitInGroup       takes unit whichUnit, group whichGroup returns boolean
constant native IsUnitInForce       takes unit whichUnit, force whichForce returns boolean
constant native IsUnitOwnedByPlayer takes unit whichUnit, player whichPlayer returns boolean
constant native IsUnitAlly          takes unit whichUnit, player whichPlayer returns boolean
constant native IsUnitEnemy         takes unit whichUnit, player whichPlayer returns boolean
constant native IsUnitVisible       takes unit whichUnit, player whichPlayer returns boolean
constant native IsUnitDetected      takes unit whichUnit, player whichPlayer returns boolean
constant native IsUnitInvisible     takes unit whichUnit, player whichPlayer returns boolean
constant native IsUnitFogged        takes unit whichUnit, player whichPlayer returns boolean
constant native IsUnitMasked        takes unit whichUnit, player whichPlayer returns boolean
constant native IsUnitSelected      takes unit whichUnit, player whichPlayer returns boolean
constant native IsUnitRace          takes unit whichUnit, race whichRace returns boolean

/**


@note This native returns a boolean, which when typecasted to integer might
be greater than 1. It's probably implemented via a bitset.

@note In past patches this native bugged when used in `conditionfunc`s.
The fix back then was to compare with true (`==true`).
I cannot reproduce the faulty behaviour in patch 1.27 so this is only a note.

*/
constant native IsUnitType          takes unit whichUnit, unittype whichUnitType returns boolean

/**


@note Useless. Use operator== instead.
@pure 

*/
constant native IsUnit              takes unit whichUnit, unit whichSpecifiedUnit returns boolean
constant native IsUnitInRange       takes unit whichUnit, unit otherUnit, real distance returns boolean
constant native IsUnitInRangeXY     takes unit whichUnit, real x, real y, real distance returns boolean
constant native IsUnitInRangeLoc    takes unit whichUnit, location whichLocation, real distance returns boolean

/**
Returns `true` if `whichUnit` is hidden, for example by means of `ShowUnit`.

@note A comment in Blizzard.j, `ReplaceUnitBJ` states that it's "sometimes unsafe to remove hidden units",
as a workaround it calls `KillUnit` right before `RemoveUnit`.
*/
constant native IsUnitHidden        takes unit whichUnit returns boolean
constant native IsUnitIllusion      takes unit whichUnit returns boolean

constant native IsUnitInTransport   takes unit whichUnit, unit whichTransport returns boolean
constant native IsUnitLoaded        takes unit whichUnit returns boolean

constant native IsHeroUnitId        takes integer unitId returns boolean
constant native IsUnitIdType        takes integer unitId, unittype whichUnitType returns boolean

native UnitShareVision              takes unit whichUnit, player whichPlayer, boolean share returns nothing
native UnitSuspendDecay             takes unit whichUnit, boolean suspend returns nothing
native UnitAddType                  takes unit whichUnit, unittype whichUnitType returns boolean
native UnitRemoveType               takes unit whichUnit, unittype whichUnitType returns boolean


/**
Adds the ability to target unit. Can be used to add an ability to any hero.
The added ability is level 1 and without a cooldown.

Returns:

- true if the addition was successful (hero did not have this ability before)
- false otherwise (hero already has this ability)

@param whichUnit Target unit.
@param abilityId Abilities' raw code identifier.


*/
native UnitAddAbility               takes unit whichUnit, integer abilityId returns boolean

/**
Removes the ability from target unit.

Returns:

- true if the removal was successful (hero did have this ability before)
- false otherwise (hero does not have this ability)

@param whichUnit Target unit.
@param abilityId Abilities' raw code identifier.


@bug Removing non-interrupt abilities like divine shile while they're being
cast (at the EVENT_PLAYER_UNIT_SPELL_EFFECT point), and while the caster is
moving, will cause the caster to become unresponsive to new commands until
they reach their ordered move point. 

*/
native UnitRemoveAbility            takes unit whichUnit, integer abilityId returns boolean

/**
This native is used to keep abilities when morphing units.


*/
native UnitMakeAbilityPermanent     takes unit whichUnit, boolean permanent, integer abilityId returns boolean
native UnitRemoveBuffs              takes unit whichUnit, boolean removePositive, boolean removeNegative returns nothing
native UnitRemoveBuffsEx            takes unit whichUnit, boolean removePositive, boolean removeNegative, boolean magic, boolean physical, boolean timedLife, boolean aura, boolean autoDispel returns nothing
native UnitHasBuffsEx               takes unit whichUnit, boolean removePositive, boolean removeNegative, boolean magic, boolean physical, boolean timedLife, boolean aura, boolean autoDispel returns boolean
native UnitCountBuffsEx             takes unit whichUnit, boolean removePositive, boolean removeNegative, boolean magic, boolean physical, boolean timedLife, boolean aura, boolean autoDispel returns integer
native UnitAddSleep                 takes unit whichUnit, boolean add returns nothing
native UnitCanSleep                 takes unit whichUnit returns boolean
native UnitAddSleepPerm             takes unit whichUnit, boolean add returns nothing
native UnitCanSleepPerm             takes unit whichUnit returns boolean
native UnitIsSleeping               takes unit whichUnit returns boolean
native UnitWakeUp                   takes unit whichUnit returns nothing
native UnitApplyTimedLife           takes unit whichUnit, integer buffId, real duration returns nothing
native UnitIgnoreAlarm              takes unit whichUnit, boolean flag returns boolean
native UnitIgnoreAlarmToggled       takes unit whichUnit returns boolean
native UnitResetCooldown            takes unit whichUnit returns nothing
native UnitSetConstructionProgress  takes unit whichUnit, integer constructionPercentage returns nothing
native UnitSetUpgradeProgress       takes unit whichUnit, integer upgradePercentage returns nothing
native UnitPauseTimedLife           takes unit whichUnit, boolean flag returns nothing
native UnitSetUsesAltIcon           takes unit whichUnit, boolean flag returns nothing


/**


@bug Has been known to cause crashes in battle.net.

*/
native UnitDamagePoint              takes unit whichUnit, real delay, real radius, real x, real y, real amount, boolean attack, boolean ranged, attacktype attackType, damagetype damageType, weapontype weaponType returns boolean

/**
Deals damage to target widget from a source unit.

@param whichUnit The source of the damage. To actual deal damage it should be
not `null`.

@param target The target being damaged.

@param amount How much damage is being dealt.

@param attack Consider the damage dealt as being an attack.

@param ranged Consider the damage dealt as being from a ranged source.


@note For some insight about the different configurations of the different
types see [this post](http://www.wc3c.net/showpost.php?p=1030046&postcount=19).


*/
native UnitDamageTarget             takes unit whichUnit, widget target, real amount, boolean attack, boolean ranged, attacktype attackType, damagetype damageType, weapontype weaponType returns boolean

native IssueImmediateOrder          takes unit whichUnit, string order returns boolean
native IssueImmediateOrderById      takes unit whichUnit, integer order returns boolean

/**
@note If the order is to build a structure and the unit can build that structure in principle but the player lacks the resources for it, then the unit
will be pinged on the minimap in yellow for its owning player.

@bug If the order is to build a structure, this function will return `false` even if the unit accepts the order.
*/
native IssuePointOrder              takes unit whichUnit, string order, real x, real y returns boolean

/**
@note If the order is to build a structure and the unit can build that structure in principle but the player lacks the resources for it, then the unit
will be pinged on the minimap in yellow for its owning player.

@bug If the order is to build a structure, this function will return `false` even if the unit accepts the order.
*/
native IssuePointOrderLoc           takes unit whichUnit, string order, location whichLocation returns boolean

/**
@note If the order is to build a structure and the unit can build that structure in principle but the player lacks the resources for it, then the unit
will be pinged on the minimap in yellow for its owning player.

@bug If the order is to build a structure, this function will return `false` even if the unit accepts the order.
*/
native IssuePointOrderById          takes unit whichUnit, integer order, real x, real y returns boolean

/**
@note If the order is to build a structure and the unit can build that structure in principle but the player lacks the resources for it, then the unit
will be pinged on the minimap in yellow for its owning player.

@bug If the order is to build a structure, this function will return `false` even if the unit accepts the order.
*/
native IssuePointOrderByIdLoc       takes unit whichUnit, integer order, location whichLocation returns boolean
native IssueTargetOrder             takes unit whichUnit, string order, widget targetWidget returns boolean
native IssueTargetOrderById         takes unit whichUnit, integer order, widget targetWidget returns boolean
native IssueInstantPointOrder       takes unit whichUnit, string order, real x, real y, widget instantTargetWidget returns boolean
native IssueInstantPointOrderById   takes unit whichUnit, integer order, real x, real y, widget instantTargetWidget returns boolean
native IssueInstantTargetOrder      takes unit whichUnit, string order, widget targetWidget, widget instantTargetWidget returns boolean
native IssueInstantTargetOrderById  takes unit whichUnit, integer order, widget targetWidget, widget instantTargetWidget returns boolean

/**
@note If the order is to build a structure and the unit can build that structure in principle but the player lacks the resources for it, then the unit
will be pinged on the minimap in yellow for its owning player.

@bug If the order is to build a structure and the unit can build that structure in principle (and the spot is not blocked, either),
this function will still return `true` even if the player lacks the resources for it.
*/
native IssueBuildOrder              takes unit whichPeon, string unitToBuild, real x, real y returns boolean

/**
@note If the order is to build a structure and the unit can build that structure in principle but the player lacks the resources for it, then the unit
will be pinged on the minimap in yellow for its owning player.

@bug If the order is to build a structure and the unit can build that structure in principle (and the spot is not blocked, either),
this function will still return `true` even if the player lacks the resources for it.
*/
native IssueBuildOrderById          takes unit whichPeon, integer unitId, real x, real y returns boolean

native IssueNeutralImmediateOrder       takes player forWhichPlayer, unit neutralStructure, string unitToBuild returns boolean

/**
Can be used to buy items and units at a shop.


*/
native IssueNeutralImmediateOrderById   takes player forWhichPlayer,unit neutralStructure, integer unitId returns boolean
native IssueNeutralPointOrder           takes player forWhichPlayer,unit neutralStructure, string unitToBuild, real x, real y returns boolean
native IssueNeutralPointOrderById       takes player forWhichPlayer,unit neutralStructure, integer unitId, real x, real y returns boolean
native IssueNeutralTargetOrder          takes player forWhichPlayer,unit neutralStructure, string unitToBuild, widget target returns boolean
native IssueNeutralTargetOrderById      takes player forWhichPlayer,unit neutralStructure, integer unitId, widget target returns boolean

native GetUnitCurrentOrder          takes unit whichUnit returns integer


/**
Sets the amount of available gold of a gold mine. The amount can be negative, which is practically the same as 0.

@param whichUnit Change amount of this gold mine unit.

@param amount The new gold amount.


@bug If the final value, after adding a negative amount, will be less than zero, then it
will display the correct negative amount, but mining won't yield any gold.
If peasant enters a mine with 0 gold, it's destroyed and he stops next to mine.
If peasant enters a mine with <0 gold, it's destroyed and he runs back to the castle.

@note See: `AddResourceAmount`, `GetResourceAmount`.

*/
native SetResourceAmount            takes unit whichUnit, integer amount returns nothing

/**
Adds the amount of available gold to a gold mine. The amount can be negative, which is practically the same as 0.

@param whichUnit Add gold to this gold mine unit.

@param amount The amount of gold to add to the unit.


@note See `SetResourceAmount` for edge-case descriptions. Also: `SetResourceAmount`, `GetResourceAmount`.

*/
native AddResourceAmount            takes unit whichUnit, integer amount returns nothing

/**
Returns the amount of available gold in a gold mine. The amount can be negative, which is practically the same as 0.

@param whichUnit Add gold to this gold mine unit.


@note See `SetResourceAmount` for edge-case descriptions. Also: `SetResourceAmount`, `AddResourceAmount`.

*/
native GetResourceAmount            takes unit whichUnit returns integer

native WaygateGetDestinationX       takes unit waygate returns real
native WaygateGetDestinationY       takes unit waygate returns real
native WaygateSetDestination        takes unit waygate, real x, real y returns nothing
native WaygateActivate              takes unit waygate, boolean activate returns nothing
native WaygateIsActive              takes unit waygate returns boolean


/**
Adds an item of the type itemId with current stock of currentStock and max stock
of stockMax to all shops in game.

@param itemId The item to add to the stock.

@param currentStock Determines the amount of that item in stock upon being added
to the buildings.

@param stockMax The item will grow in stock count up to the value of stockMax.
The rate at which the item grows in stock is determined by its stock replenish
interval, which can be modified in the object editor.

@note Some issues with default Blizzard initialization and that function were met.
See <http://www.hiveworkshop.com/forums/l-715/a-251815/> for details.

@note Adding an item which already is in stock for a building will replace it
and refresh the interval and stock count.




*/
native AddItemToAllStock            takes integer itemId, integer currentStock, integer stockMax returns nothing

/**
Adds an item of the type itemId with current stock of currentStock and max stock
of stockMax to the specific shop whichUnit.


@note Some issues with default Blizzard initialization and that function were met.
See <http://www.hiveworkshop.com/forums/l-715/a-251815/> for details.

*/
native AddItemToStock               takes unit whichUnit, integer itemId, integer currentStock, integer stockMax returns nothing
native AddUnitToAllStock            takes integer unitId, integer currentStock, integer stockMax returns nothing
native AddUnitToStock               takes unit whichUnit, integer unitId, integer currentStock, integer stockMax returns nothing

native RemoveItemFromAllStock       takes integer itemId returns nothing
native RemoveItemFromStock          takes unit whichUnit, integer itemId returns nothing
native RemoveUnitFromAllStock       takes integer unitId returns nothing
native RemoveUnitFromStock          takes unit whichUnit, integer unitId returns nothing

native SetAllItemTypeSlots          takes integer slots returns nothing
native SetAllUnitTypeSlots          takes integer slots returns nothing
native SetItemTypeSlots             takes unit whichUnit, integer slots returns nothing
native SetUnitTypeSlots             takes unit whichUnit, integer slots returns nothing

native GetUnitUserData              takes unit whichUnit returns integer

/**
Sets a single custom integer for a unit.


@note This value is not used by any standard mechanisms in Warcraft III nor
in the blizzard.j, so it is free to be harnessed.
Besides `GetHandleId`, this is an excellent possibility to assign a unique
integer id to a unit, which can serve as an index in other data structures.

*/
native SetUnitUserData              takes unit whichUnit, integer data returns nothing

//============================================================================
// Player API

/**
Returns the instance of player based on ID number.

This function always returns the same instance, does not create new objects.
If used with invalid values (below 0 or above `GetBJMaxPlayerSlots`), returns null in Reforged, crashed on Classic.


@note 
Common.j: IDs start from 0, e.g. Player(0) is red, 1 is blue etc. -> `GetPlayerId`
Blizzard.j (WorldEdit): IDs start with 1 -> `GetConvertedPlayerId`.

@note See: `GetPlayerId`, `GetBJMaxPlayers`, `GetBJMaxPlayerSlots`, `GetPlayerNeutralPassive`, `GetPlayerNeutralAggressive`.

@bug In old versions (which?) crashes the game if used with wrong values, that is values greather than 15
or values lower than 0.

@pure 

*/
constant native Player              takes integer number returns player

/**
Returns reference to local player, as such it always points to yourself.
Every other player in the game gets their player respectively.

Do not use this function until you understand it fully and know how to avoid desyncs!
Always test your map in LAN multiplayer after changing code around it.

Warcraft 3 has the lock-step network/execution model.
This means the game simulation and code run on all players' computers with the exact same data at any point in time.
Look at this example (Lua):
    
```{.lua}
function whoami_command()
    -- all players know who entered this command, equal value on every computer
    local player_who_entered_command = GetTriggerPlayer()
    -- this always points to you!
    local myself = GetLocalPlayer() 
    
    local command_player_name = GetPlayerName(player_who_entered_command)
    local my_player_name = GetPlayerName(myself)
    -- everybody will see this player's name
    DisplayTextToForce(GetPlayersAll(), "Player ".. command_player_name .." entered the whoami command!")
    -- everybody will see their own name!
    DisplayTextToForce(GetPlayersAll(), "But my name is: ".. my_player_name)
end
```

This function is always used when you want something only to happen to a certain player.
**However if you aren't careful, you will cause a desync:** where one player's game state is different from everybody else!
For example, you can apply camera position locally, this is how `SetCameraPositionForPlayer` works (Jass):

    if (GetLocalPlayer() == whichPlayer) then
       // Use only local code (no net traffic) within this block to avoid desyncs.
       call SetCameraPosition(x, y)
    endif

Basic rule: anything that's only visual (like unit color) will not desync... unless your code relies on the exact value later in the game:
*if cameraX is 123 then ...* different players will have different camera positions here.

On the other hand, manipulating handles or creating units locally,
changing their health, attack, invisibility etc. - anything that changes the game will desync:

    if (GetLocalPlayer() == whichPlayer) then
       // INSTANTLY DESYNCS! The unit was only killed on one player's screen! Others think it's alive
       call KillUnit(someUnit)
    endif


@async 

*/
constant native GetLocalPlayer      takes nothing returns player
constant native IsPlayerAlly        takes player whichPlayer, player otherPlayer returns boolean
constant native IsPlayerEnemy       takes player whichPlayer, player otherPlayer returns boolean
constant native IsPlayerInForce     takes player whichPlayer, force whichForce returns boolean
constant native IsPlayerObserver    takes player whichPlayer returns boolean
constant native IsVisibleToPlayer           takes real x, real y, player whichPlayer returns boolean
constant native IsLocationVisibleToPlayer   takes location whichLocation, player whichPlayer returns boolean
constant native IsFoggedToPlayer            takes real x, real y, player whichPlayer returns boolean
constant native IsLocationFoggedToPlayer    takes location whichLocation, player whichPlayer returns boolean
constant native IsMaskedToPlayer            takes real x, real y, player whichPlayer returns boolean
constant native IsLocationMaskedToPlayer    takes location whichLocation, player whichPlayer returns boolean


/**
Returns race of the player.

The handle is constant and does not change between invocations. (Lua, v1.32.10)
*/
constant native GetPlayerRace           takes player whichPlayer returns race

/**
Returns player ID of player (which starts with zero; e.g. player red is 0).

@param whichPlayer Target player.


@note For one-based WorldEdit-type IDs see: `GetConvertedPlayerId`. Also: `Player`.

*/
constant native GetPlayerId             takes player whichPlayer returns integer
constant native GetPlayerUnitCount      takes player whichPlayer, boolean includeIncomplete returns integer
constant native GetPlayerTypedUnitCount takes player whichPlayer, string unitName, boolean includeIncomplete, boolean includeUpgrades returns integer
constant native GetPlayerStructureCount takes player whichPlayer, boolean includeIncomplete returns integer
constant native GetPlayerState          takes player whichPlayer, playerstate whichPlayerState returns integer
constant native GetPlayerScore          takes player whichPlayer, playerscore whichPlayerScore returns integer
constant native GetPlayerAlliance       takes player sourcePlayer, player otherPlayer, alliancetype whichAllianceSetting returns boolean

constant native GetPlayerHandicap       takes player whichPlayer returns real
constant native GetPlayerHandicapXP     takes player whichPlayer returns real

/**


@patch 1.32

*/
constant native GetPlayerHandicapReviveTime takes player whichPlayer returns real

/**


@patch 1.32

*/
constant native GetPlayerHandicapDamage takes player whichPlayer returns real
constant native SetPlayerHandicap       takes player whichPlayer, real handicap returns nothing
constant native SetPlayerHandicapXP     takes player whichPlayer, real handicap returns nothing

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
research level, see `SetPlayerTechResearched` to set the research level).

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

@param techid The id of the tech item. Either an upgrade like Iron Plating `'Rhar'`, a unit type like Footman `'hfoo'` or one of the following special equivalent ids:

- `'HERO'` - any hero
- `'TALT'` - any altar
- `'TWN1'` - town hall tier 1
- `'TWN2'` - town hall tier 2
- `'TWN3'` - town hall tier 3
- `'TWN4'` - town hall tier 4
- `'TWN5'` - town hall tier 5
- `'TWN6'` - town hall tier 6
- `'TWN7'` - town hall tier 7
- `'TWN8'` - town hall tier 8
- `'TWN9'` - town hall tier 9

@param specificonly When this is false, it will consider some additional dependencies between the techs:
When specificonly is false, the Human Guard Tower 'hgtw' will also be considered when querying for the Scout Tower `'hwtw'` (even if the Guard Tower is preplaced, i.e. not doing the upgrade on runtime, so this checks the Teechtree - Upgrades To `'uupt'` field?).
Higher tier townhalls will be considered when querying for lower tier hownhalls, i.e. querying for Great Hall `'ogre'` will also consider Stronghold `'ostr'` and Fortress `'ofrt'` when specificonly is false.
Ability morph does not seem to be considered when specificonly is false, tested with Berserker Upgrade of Headhunter.
Techtree - Dependency Equivalents `'udep'` seems to be considered even if specificonly is true, i.e. when you set Scout Tower as an equivalent for Farm `'hhou'`, querying for `'hhou'` will also consider Scout Towers.


*/
constant native GetPlayerTechCount      takes player whichPlayer, integer techid, boolean specificonly returns integer

native SetPlayerUnitsOwner takes player whichPlayer, integer newOwner returns nothing

/**
Reveals a player's remaining buildings to a force. The black mask over the
buildings will be removed as if the territory had been discovered.

@param whichPlayer The player to reveal.

@param toWhichPlayers The players who will see whichPlayer's buildings.

@param flag If true, the buildings will be revealed. If false, the buildings
will not be revealed. Note that if you set it to false, it will not hide the buildings with a black mask.

@note his function will not check whether the player has a town hall before revealing.


*/
native CripplePlayer takes player whichPlayer, force toWhichPlayers, boolean flag returns nothing

native SetPlayerAbilityAvailable        takes player whichPlayer, integer abilid, boolean avail returns nothing

native SetPlayerState   takes player whichPlayer, playerstate whichPlayerState, integer value returns nothing
native RemovePlayer     takes player whichPlayer, playergameresult gameResult returns nothing

// Used to store hero level data for the scorescreen
// before units are moved to neutral passive in melee games
//

/**
Used to store hero level data for the scorescreen, before units are moved
to neutral passive in melee games.

@param whichPlayer The player to store hero data for.


*/
native CachePlayerHeroData takes player whichPlayer returns nothing

//============================================================================
// Fog of War API

/**
Sets target player's fog of war data in the specified rectangle.

Individual player's fog state is a reflection of player's map exploration progress:
which areas were explored or still hidden; which are fogged (not visible); which are
visible. What is visible in game is a combination of personal fog state & fog modifiers.

@param forWhichPlayer Target player.
@param whichState Change fog to this type. See `fogstate` for type explanation.
@param where Target rectangle area.
@param useSharedVision If true, apply new state to player and whoever player shares their vision.
If false, apply only to player themself.


*/
native  SetFogStateRect      takes player forWhichPlayer, fogstate whichState, rect where, boolean useSharedVision returns nothing

/**
Sets target player's fog of war data in the specified circle area.

Individual player's fog state is a reflection of player's map exploration progress:
which areas were explored or still hidden; which are fogged (not visible); which are
visible. What is visible in game is a combination of personal fog state & fog modifiers.

@param forWhichPlayer Target player.
@param whichState Change fog to this type. See `fogstate` for type explanation.
@param centerx X-coordinate of the circle center.
@param centerY Y-coordinate of the circle center.
@param radius Circle's radius (from center to its edge).
@param useSharedVision If true, apply new state to player and whoever player shares their vision.
If false, apply only to player themself.


*/
native  SetFogStateRadius    takes player forWhichPlayer, fogstate whichState, real centerx, real centerY, real radius, boolean useSharedVision returns nothing

/**
Sets target player's fog of war data in the specified circle area.

Individual player's fog state is a reflection of player's map exploration progress:
which areas were explored or still hidden; which are fogged (not visible); which are
visible. What is visible in game is a combination of personal fog state & fog modifiers.

@param forWhichPlayer Target player.
@param whichState Change fog to this type. See `fogstate` for type explanation.
@param center Location describing the center of the circle.
@param radius Circle's radius (from center to its edge).
@param useSharedVision If true, apply new state to player and whoever player shares their vision.
If false, apply only to player themself.


*/
native  SetFogStateRadiusLoc takes player forWhichPlayer, fogstate whichState, location center, real radius, boolean useSharedVision returns nothing

/**
Toggles global FOW masking of unexplored areas.

An individual player's fog state is not modified, that means this toggle is fully
reversible.

@param enable True: unexplored areas are masked.

False: unexplored areas are not masked (whether visible depends on `IsFogEnabled`).


@note See: `IsFogMaskEnabled`, `IsFogEnabled`. "Fog mask" is explained in `fogstate`.

*/
native  FogMaskEnable        takes boolean enable returns nothing

/**
Returns whether global FOW masking is in effect.

True: unexplored areas are globally masked.

False: unexplored areas are not globally masked (whether visible depends on `IsFogEnabled`).


@note See: `FogMaskEnable`, `FogEnable`. "Fog mask" is explained in `fogstate`.

*/
native  IsFogMaskEnabled     takes nothing returns boolean

/**
Toggles global FOW fogging of explored yet not visible areas.

An individual player's fog state is not modified, that means this toggle is fully
reversible.

True: explored areas are fogged if not in sight.

False: explored areas remain permanently visible.


@note See: `IsFogEnabled`, `IsFogMaskEnabled`. "Fog" is explained in `fogstate`.

*/
native  FogEnable            takes boolean enable returns nothing

/**
Toggles global FOW fogging of explored yet not visible areas.

True: explored areas are fogged if not in sight.

False: explored areas remain permanently visible.


@note See: `FogEnable`, `FogMaskEnable`. "Fog" is explained in `fogstate`.

*/
native  IsFogEnabled         takes nothing returns boolean


/**
Creates an object that overrides the fog in a rect for a specific player.

A fog modifier is disabled by default, use `FogModifierStart` to enable.

This creates a new object with a handle and must be removed to avoid leaks: `DestroyFogModifier`.

@param whichState Determines what type of fog the area is being modified to. See `fogstate` for type explanation.

@param where The rect where the fog is.

@param useSharedVision Apply modifier to target's allied players with shared vision?

@param afterUnits Will determine whether or not units in that area will be masked by the fog. If it is set to true and the fogstate is masked, it will hide all the units in the fog modifier's radius and mask the area. If set to false, it will only mask the areas that are not visible to the units.


@bug (v1.32.10) Just by creating a modifier of type `FOG_OF_WAR_FOGGED` or
`FOG_OF_WAR_VISIBLE`, this will modify the player's global fog state before it is
enabled. "VISIBLE" will instantly become "FOGGED" and "FOGGED" will cause unexplored
areas to become explored. You can workaround this by using e.g. `SetFogStateRect`
after fog modifier creation.

*/
native CreateFogModifierRect        takes player forWhichPlayer, fogstate whichState, rect where, boolean useSharedVision, boolean afterUnits returns fogmodifier

/**
Creates an object that overrides the fog in a circular radius for a specific player.

A fog modifier is disabled by default, use `FogModifierStart` to enable.

This creates a new object with a handle and must be removed to avoid leaks: `DestroyFogModifier`.

@param whichState Determines what type of fog the area is being modified to. See `fogstate` for type explanation.

@param centerx The x-coordinate where the fog modifier begins.

@param centerY The y-coordinate where the fog modifier begins.

@param radius Determines the extent that the fog travels (expanding from the coordinates ( centerx , centery )).

@param useSharedVision Apply modifier to target's allied players with shared vision?

@param afterUnits Will determine whether or not units in that area will be masked by the fog. If it is set to true and the `fogstate` is masked, it will hide all the units in the fog modifier's radius and mask the area. If set to false, it will only mask the areas that are not visible to the units.


@bug (v1.32.10) Just by creating a modifier of type `FOG_OF_WAR_FOGGED` or
`FOG_OF_WAR_VISIBLE`, this will modify the player's global fog state before it is
enabled. "VISIBLE" will instantly become "FOGGED" and "FOGGED" will cause unexplored
areas to become explored. You can workaround this by using e.g. `SetFogStateRect`
after fog modifier creation.

*/
native CreateFogModifierRadius      takes player forWhichPlayer, fogstate whichState, real centerx, real centerY, real radius, boolean useSharedVision, boolean afterUnits returns fogmodifier

/**
Creates an object that overrides the fog in a circular radius for a specific player.

A fog modifier is disabled by default, use `FogModifierStart` to enable.

This creates a new object with a handle and must be removed to avoid leaks: `DestroyFogModifier`.

@param whichState Determines what type of fog the area is being modified to. See `fogstate` for type explanation.

@param center The location where the fog modifier begins.

@param radius Determines the extent that the fog travels (expanding from the location `center`).

@param useSharedVision Apply modifier to target's allied players with shared vision?

@param afterUnits Will determine whether or not units in that area will be masked by the fog. If it is set to true and the `fogstate` is masked, it will hide all the units in the fog modifier's radius and mask the area. If set to false, it will only mask the areas that are not visible to the units.


@bug (v1.32.10) Just by creating a modifier of type `FOG_OF_WAR_FOGGED` or
`FOG_OF_WAR_VISIBLE`, this will modify the player's global fog state before it is
enabled. "VISIBLE" will instantly become "FOGGED" and "FOGGED" will cause unexplored
areas to become explored. You can workaround this by using e.g. `SetFogStateRect`
after fog modifier creation.

*/
native CreateFogModifierRadiusLoc   takes player forWhichPlayer, fogstate whichState, location center, real radius, boolean useSharedVision, boolean afterUnits returns fogmodifier

/**
Destroys the fog modifier object and removes its effect.


*/
native DestroyFogModifier           takes fogmodifier whichFogModifier returns nothing

/**
Enable the effect of the modifier. While enabled, it will override the player's
regular fog state.


*/
native FogModifierStart             takes fogmodifier whichFogModifier returns nothing

/**
Disable the effect of the modifier. Once disabled the player's visibility
will return to player's regular fog state.


*/
native FogModifierStop              takes fogmodifier whichFogModifier returns nothing

//============================================================================
// Game API
native VersionGet takes nothing returns version
native VersionCompatible takes version whichVersion returns boolean
native VersionSupported takes version whichVersion returns boolean

native EndGame takes boolean doScoreScreen returns nothing

// Async only!

/**
Loads the next level for all players. Note that this function is asynchronous,
so each player will be sent to their own map. If the boolean is set to true,
the score screen will appear before the user progresses to the next level.

@param newLevel The path of the next level. The path is relative to the Warcraft III folder.

@param doScoreScreen If set to true, the score screen will appear before the user progresses to the next level.


*/
native          ChangeLevel         takes string newLevel, boolean doScoreScreen returns nothing
native          RestartGame         takes boolean doScoreScreen returns nothing
native          ReloadGame          takes nothing returns nothing
// %%% SetCampaignMenuRace is deprecated.  It must remain to support
// old maps which use it, but all new maps should use SetCampaignMenuRaceEx

/**


@note Deprecated. Use SetCampaignMenuRaceEx instead.

*/
native          SetCampaignMenuRace takes race r returns nothing
native          SetCampaignMenuRaceEx takes integer campaignIndex returns nothing
native          ForceCampaignSelectScreen takes nothing returns nothing


/**


@bug The filename seems to have some limitations:

- No underscores in campaign names.
- Shorter file names for savegames.
- Probably no dots in savegames or campaign names.

For more info see <http://www.hiveworkshop.com/threads/map-transition-does-not-work-when-loading-a-custom-savegame.286927/>.

*/
native          LoadGame            	takes string saveFileName, boolean doScoreScreen returns nothing
native          SaveGame            	takes string saveFileName returns nothing
native          RenameSaveDirectory 	takes string sourceDirName, string destDirName returns boolean
native          RemoveSaveDirectory 	takes string sourceDirName returns boolean
native          CopySaveGame        	takes string sourceSaveName, string destSaveName returns boolean
native          SaveGameExists      	takes string saveName returns boolean

/**


@patch 1.32

*/
native          SetMaxCheckpointSaves  	takes integer maxCheckpointSaves returns nothing

/**


@patch 1.32

*/
native          SaveGameCheckpoint  	takes string saveFileName, boolean showWindow returns nothing
native          SyncSelections      	takes nothing returns nothing
native          SetFloatGameState   	takes fgamestate whichFloatGameState, real value returns nothing
constant native GetFloatGameState   	takes fgamestate whichFloatGameState returns real
native          SetIntegerGameState 	takes igamestate whichIntegerGameState, integer value returns nothing
constant native GetIntegerGameState		takes igamestate whichIntegerGameState returns integer


//============================================================================
// Campaign API
native  SetTutorialCleared      takes boolean cleared returns nothing
native  SetMissionAvailable     takes integer campaignNumber, integer missionNumber, boolean available returns nothing
native  SetCampaignAvailable    takes integer campaignNumber, boolean available  returns nothing
native  SetOpCinematicAvailable takes integer campaignNumber, boolean available  returns nothing
native  SetEdCinematicAvailable takes integer campaignNumber, boolean available  returns nothing
native  GetDefaultDifficulty    takes nothing returns gamedifficulty
native  SetDefaultDifficulty    takes gamedifficulty g returns nothing
native  SetCustomCampaignButtonVisible  takes integer whichButton, boolean visible returns nothing
native  GetCustomCampaignButtonVisible  takes integer whichButton returns boolean
native  DoNotSaveReplay         takes nothing returns nothing

//============================================================================
// Dialog API

/**
Creates a new dialog. It is empty and hidden by default.

Since this creates an object and returns a handle, it must be freed when no longer needed
with `DialogDestroy`.

An empty dialog must be populated with buttons
(`DialogSetMessage`, `DialogAddButton`, `DialogAddQuitButton`)
and finally displayed with `DialogDisplay`.


@note While a dialog is open, the player can only interact with the dialog.
Everything else is disabled.

An empty dialog completely blocks all player input (except multiboard interaction and
probably other scripted custom elements). A player can only exit the game with Alt+F4.

@bug The top-bar menu buttons are greyed out when a dialog is shown. If the player presses
Alt+F4 and then clicks "Cancel", the menu buttons become visible and clickable but do nothing.

*/
native DialogCreate                 takes nothing returns dialog

/**
Destroys the dialog and frees the handle.

Due to a bug, you must first hide the dialog for all players (who have it open).


@bug If you destroy a dialog that is still shown, it will no longer show **but**
the player who had it open will be unable to interact with the game. Everything will be still
disabled, all menus and units unclickable.

*/
native DialogDestroy                takes dialog whichDialog returns nothing

/**
Completely clears the dialog, its title and buttons, even if it is already open.

You must hide the dialog first, else the player will need to quit the game,
because they will be unable to click anything or send a chat message.

@param whichDialog Target dialog to clear.


*/
native DialogClear                  takes dialog whichDialog returns nothing

/**
Sets the menu title.

@param whichDialog Target dialog.
@param messageText New title.

@note If the dialog is not set (empty string), then no vertical space is reserved for it.
The buttons start at the very top.

@note The new message shows up instantly, even when the menu is already open.
@note Unlike with buttons, if the message is too long it overflows to the left and right beyond the
screen edges (it is centered).


*/
native DialogSetMessage             takes dialog whichDialog, string messageText returns nothing

/**
Creates a menu button and returns a handle to it.

You must save the button handle to later compare it to the selected button in
a `EVENT_DIALOG_BUTTON_CLICK` using `GetClickedButton` and `GetClickedDialog`.

New buttons are added to the bottom of the menu.

@param whichDialog Target dialog to add the button to.
@param buttonText Custom text.
@param hotkey Integer value of the ASCII upper-case character for the hotkey.
Example: "F" = 70.

@note If the menu is already open, you must refresh the menu with `DialogDisplay`
to show new buttons.

@note **Line-width:** With the default font (v1.32.10) there's just enough space to display
`Yo dawg I put this text in here.` or 19 full-width characters like "@" (at character).
If longer, the text becomes multi-line, up to 3 lines max.
If longer than 3 full lines, the rest of string is not shown.

@note **Hotkey (uppercase):**
When adding a hotkey use the uppercase, e.g. `'F'` instead of `'f'` as it
does not work with lowercased keys. The button still gets triggered when the player
presses a lowercased letter.

@note **Duplicated hotkeys**: When multiple buttons have the same hotkey, the last button has priority.

@note **Hotkeys are layout-dependent.**

In other words, the English QWERTY, the German QWERTZ and the French AZERTY layouts etc. will have some
keyboard keys on different physical buttons, based on user's currently enabled layout.

The Russian keyboard layout adheres to QWERTY (as an example of a non-latin layout).

@note Hotkeys like "@" (ASCII 64) don't work (or maybe they have a different integer value).
On a QWERTY layout you need to press SHIFT+2 to enter "@".

@note You can add up to 12 working buttons.

@bug 
The 13th button will still render correctly, but not work when clicked/hotkey is used.
The 14th button will render outside the dialog border background.
The 15th button will render outside the visible area (you'll see a few pixels of it at the bottom).


*/
native DialogAddButton              takes dialog whichDialog, string buttonText, integer hotkey returns button

/**
Creates a menu button that will exit the game for the player who clicks it.
Returns a handle of button.

See the detailed description in `DialogAddButton`.

@param whichDialog Target dialog to add the button to.
@param doScoreScreen When a button with `true` is pressed, you quit the map and see the end game leaderboards.

When a button with `false` is pressed, you quit the map to game's main menu.

@param buttonText Custom text.
@param hotkey Integer value of the ASCII upper-case character for the hotkey.
Example: "F" = 70.


*/
native DialogAddQuitButton          takes dialog whichDialog, boolean doScoreScreen, string buttonText, integer hotkey returns button

/**
Open/close the dialog for the target player.

Since the dialogs are created for all players, they are hidden by default. Then you display the
dialog to players who should see and interact with it.

@param whichPlayer Target player to whom to show the dialog.
@param whichDialog Target dialog.
@param flag `true` to show (or refresh), `false` to hide.

@note Technically, because every player knows about each dialog,
cheaters could interact with dialogs that are invisible to them.

@note Dialogs can not be shown at map-init. Use a wait or a zero-timer to
display it as soon as possible.


*/
native DialogDisplay                takes player whichPlayer, dialog whichDialog, boolean flag returns nothing

// Creates a new or reads in an existing game cache file stored
// in the current campaign profile dir
//
native  ReloadGameCachesFromDisk takes nothing returns boolean


/**


@note You cannot create more than 255 gamecaches.
In multiplayer the existing game caches are not considered, so you can get a
full 255 new game caches.

In singleplayer, when you call `InitGameCache`, it looks in the Campaigns.w3v
file if a `gamecache` with that name already exists, if yes, it will create a
`gamecache` handle (you can get multiple handles for the same game cache, and
that will only count once to the 255 limit in the current game), if no and it
does not exist yet in the current game either, it will take a new slot among
the 255.

*/
native  InitGameCache    takes string campaignFile returns gamecache
native  SaveGameCache    takes gamecache whichCache returns boolean

native  StoreInteger					takes gamecache cache, string missionKey, string key, integer value returns nothing
native  StoreReal						takes gamecache cache, string missionKey, string key, real value returns nothing
native  StoreBoolean					takes gamecache cache, string missionKey, string key, boolean value returns nothing

/**
Stores a description of a unit in a game cache that can be retrieved with `RestoreUnit`.

The saved attributes of the unit are (non-exhaustive): unit id, experience, hero level, unused skill points, hero proper name (index),
strength/agility/intelligence, attack speed/move speed increments from agility, life, mana and attack damage increments
(can be adjusted individually using tome abilities), sight range (day) (can be adjusted with `UNIT_RF_SIGHT_RADIUS`), armor increment

Descriptions of the items in the unit's inventory will also be saved (non-exhaustive): item id, charges, flags: drop upon death, perishable,
invulnerable, pawnable, used on acquire (powerup), droppable, actively used

Descriptions of the unit's hero abilities will also be saved: ability id, current level

See also the unit entry in the following Kaitai Struct file describing the w3v format (gamecaches file): https://github.com/WaterKnight/Warcraft3-Formats-KaitaiStruct/blob/main/w3-w3v.ksy


@bug When a unit obtains armor from a research and is then stored in a game cache, restoring it will retain the armor increment without the research, so if
the research is done again, the unit will benefit doubly.

@bug If a hero unit was stored under some key pair and later the key pair is overwritten with a non-hero unit, the previous hero attributes will not
be overwritten, i.e., they will remain and be merged with the non-hero attributes. This can be observed in the persisted .w3v file. Ingame, it
would not make a difference because the restored non-hero unit normally would not use the hero attributes.

*/
native  StoreUnit						takes gamecache cache, string missionKey, string key, unit whichUnit returns boolean
native  StoreString						takes gamecache cache, string missionKey, string key, string value returns boolean


/**
Synchronizes the value stored in the `gamecache` under the mission key and key.
Calling this function sends a sync packet from each player in the calling
context (citation needed), that is everybody sends a packet to everybody.
The game then picks the first packet arrived (at the host). Often (but not
always) that is the packet coming from the game host.

More interesting perhaps is the use to synchronize local data (like a player's
camera position) to all other players. To do this only store and sync the value
in a local context:

```
if GetLocalPlayer() == p then
    call StoreInteger(my_cache, "missionkey", "key", my_value)
    call SyncStoredInteger(my_cache, "missionkey", "key")
endif
```

Now this will synchronize the local value `my_value` to each player but we don't
know when each player has actually received it. You can use `TriggerSyncReady`
to wait for each sync action, but it's not recommended as it is very slow and
can hang for minutes (cf. [sync doc](https://www.hiveworkshop.com/pastebin/1ce4fe042832e6bd7d06697a43055373.5801))
Instead it is recommended to use a rapid timer to check if the key is present
in the gamecache. Note that this is still a local operation as different players
can receive the sync at different times. If a player has received all the data
you synchronize the fact that that player has got all the data. This is
reasonably done via `SelectUnit` and
`TriggerRegisterPlayerUnitEvent(trig, p, EVENT_PLAYER_UNIT_SELECTED, null)`.
Now once the last player has sent their selection event you have synchronized
your data.

This is a very high-level overview and the process has many edges to look out
for, so it's probably a good idea to use an already made system like
[this one](https://www.hiveworkshop.com/threads/sync-game-cache.279148/).


@note You might rather use `BlzSendSyncData` if possible.

@note Calling multiple `SyncStoredX` in a row will keep their order in the
syncing process, i.e. first sync will be received first (FIFO).

*/
native SyncStoredInteger        takes gamecache cache, string missionKey, string key returns nothing

/**
Synchronizes the value stored in the `gamecache` under the mission key and key.
See `SyncStoredInteger` for a more in-depth explanation.


*/
native SyncStoredReal           takes gamecache cache, string missionKey, string key returns nothing

/**
Synchronizes the value stored in the `gamecache` under the mission key and key.
See `SyncStoredInteger` for a more in-depth explanation.


*/
native SyncStoredBoolean        takes gamecache cache, string missionKey, string key returns nothing
native SyncStoredUnit           takes gamecache cache, string missionKey, string key returns nothing

/**


@bug Does not seem to work.

*/
native SyncStoredString         takes gamecache cache, string missionKey, string key returns nothing

native  HaveStoredInteger					takes gamecache cache, string missionKey, string key returns boolean
native  HaveStoredReal						takes gamecache cache, string missionKey, string key returns boolean
native  HaveStoredBoolean					takes gamecache cache, string missionKey, string key returns boolean
native  HaveStoredUnit						takes gamecache cache, string missionKey, string key returns boolean
native  HaveStoredString					takes gamecache cache, string missionKey, string key returns boolean

native  FlushGameCache						takes gamecache cache returns nothing
native  FlushStoredMission					takes gamecache cache, string missionKey returns nothing
native  FlushStoredInteger					takes gamecache cache, string missionKey, string key returns nothing
native  FlushStoredReal						takes gamecache cache, string missionKey, string key returns nothing
native  FlushStoredBoolean					takes gamecache cache, string missionKey, string key returns nothing
native  FlushStoredUnit						takes gamecache cache, string missionKey, string key returns nothing
native  FlushStoredString					takes gamecache cache, string missionKey, string key returns nothing

// Will return 0 if the specified value's data is not found in the cache

/**
Returns `0` if the specified value's data is not found in the cache.


*/
native  GetStoredInteger				takes gamecache cache, string missionKey, string key returns integer

/**
Returns `0.0` if the specified value's data is not found in the cache.


*/
native  GetStoredReal					takes gamecache cache, string missionKey, string key returns real

/**
Returns `false` if the specified value's data is not found in the cache.


*/
native  GetStoredBoolean				takes gamecache cache, string missionKey, string key returns boolean

/**
Returns `""` if the specified value's data is not found in the cache.


*/
native  GetStoredString					takes gamecache cache, string missionKey, string key returns string

/**
Returns `null` if the specified value's data is not found in the cache.


*/
native  RestoreUnit						takes gamecache cache, string missionKey, string key, player forWhichPlayer, real x, real y, real facing returns unit



/**


@note You cannot create more than 255 hashtables.
@patch 1.24

*/
native  InitHashtable    takes nothing returns hashtable


/**


@patch 1.24

*/
native  SaveInteger						takes hashtable table, integer parentKey, integer childKey, integer value returns nothing

/**


@patch 1.24

*/
native  SaveReal						takes hashtable table, integer parentKey, integer childKey, real value returns nothing

/**


@patch 1.24

*/
native  SaveBoolean						takes hashtable table, integer parentKey, integer childKey, boolean value returns nothing

/**


@patch 1.24

*/
native  SaveStr							takes hashtable table, integer parentKey, integer childKey, string value returns boolean

/**


@patch 1.24

*/
native  SavePlayerHandle				takes hashtable table, integer parentKey, integer childKey, player whichPlayer returns boolean

/**


@patch 1.24

*/
native  SaveWidgetHandle				takes hashtable table, integer parentKey, integer childKey, widget whichWidget returns boolean

/**


@patch 1.24

*/
native  SaveDestructableHandle			takes hashtable table, integer parentKey, integer childKey, destructable whichDestructable returns boolean

/**


@patch 1.24

*/
native  SaveItemHandle					takes hashtable table, integer parentKey, integer childKey, item whichItem returns boolean

/**


@patch 1.24

*/
native  SaveUnitHandle					takes hashtable table, integer parentKey, integer childKey, unit whichUnit returns boolean

/**


@patch 1.24

*/
native  SaveAbilityHandle				takes hashtable table, integer parentKey, integer childKey, ability whichAbility returns boolean

/**


@patch 1.24

*/
native  SaveTimerHandle					takes hashtable table, integer parentKey, integer childKey, timer whichTimer returns boolean

/**


@patch 1.24

*/
native  SaveTriggerHandle				takes hashtable table, integer parentKey, integer childKey, trigger whichTrigger returns boolean

/**


@patch 1.24

*/
native  SaveTriggerConditionHandle		takes hashtable table, integer parentKey, integer childKey, triggercondition whichTriggercondition returns boolean

/**


@patch 1.24

*/
native  SaveTriggerActionHandle			takes hashtable table, integer parentKey, integer childKey, triggeraction whichTriggeraction returns boolean

/**


@patch 1.24

*/
native  SaveTriggerEventHandle			takes hashtable table, integer parentKey, integer childKey, event whichEvent returns boolean

/**


@patch 1.24

*/
native  SaveForceHandle					takes hashtable table, integer parentKey, integer childKey, force whichForce returns boolean

/**


@patch 1.24

*/
native  SaveGroupHandle					takes hashtable table, integer parentKey, integer childKey, group whichGroup returns boolean

/**


@patch 1.24

*/
native  SaveLocationHandle				takes hashtable table, integer parentKey, integer childKey, location whichLocation returns boolean

/**


@patch 1.24

*/
native  SaveRectHandle					takes hashtable table, integer parentKey, integer childKey, rect whichRect returns boolean

/**


@patch 1.24

*/
native  SaveBooleanExprHandle			takes hashtable table, integer parentKey, integer childKey, boolexpr whichBoolexpr returns boolean

/**


@patch 1.24

*/
native  SaveSoundHandle					takes hashtable table, integer parentKey, integer childKey, sound whichSound returns boolean

/**


@patch 1.24

*/
native  SaveEffectHandle				takes hashtable table, integer parentKey, integer childKey, effect whichEffect returns boolean

/**


@patch 1.24

*/
native  SaveUnitPoolHandle				takes hashtable table, integer parentKey, integer childKey, unitpool whichUnitpool returns boolean

/**


@patch 1.24

*/
native  SaveItemPoolHandle				takes hashtable table, integer parentKey, integer childKey, itempool whichItempool returns boolean

/**


@patch 1.24

*/
native  SaveQuestHandle					takes hashtable table, integer parentKey, integer childKey, quest whichQuest returns boolean

/**


@patch 1.24

*/
native  SaveQuestItemHandle				takes hashtable table, integer parentKey, integer childKey, questitem whichQuestitem returns boolean

/**


@patch 1.24

*/
native  SaveDefeatConditionHandle		takes hashtable table, integer parentKey, integer childKey, defeatcondition whichDefeatcondition returns boolean

/**


@patch 1.24

*/
native  SaveTimerDialogHandle			takes hashtable table, integer parentKey, integer childKey, timerdialog whichTimerdialog returns boolean

/**


@patch 1.24

*/
native  SaveLeaderboardHandle			takes hashtable table, integer parentKey, integer childKey, leaderboard whichLeaderboard returns boolean

/**


@patch 1.24

*/
native  SaveMultiboardHandle			takes hashtable table, integer parentKey, integer childKey, multiboard whichMultiboard returns boolean

/**


@patch 1.24

*/
native  SaveMultiboardItemHandle		takes hashtable table, integer parentKey, integer childKey, multiboarditem whichMultiboarditem returns boolean

/**


@patch 1.24

*/
native  SaveTrackableHandle				takes hashtable table, integer parentKey, integer childKey, trackable whichTrackable returns boolean

/**


@patch 1.24

*/
native  SaveDialogHandle				takes hashtable table, integer parentKey, integer childKey, dialog whichDialog returns boolean

/**


@patch 1.24

*/
native  SaveButtonHandle				takes hashtable table, integer parentKey, integer childKey, button whichButton returns boolean

/**


@patch 1.24

*/
native  SaveTextTagHandle				takes hashtable table, integer parentKey, integer childKey, texttag whichTexttag returns boolean

/**


@patch 1.24

*/
native  SaveLightningHandle				takes hashtable table, integer parentKey, integer childKey, lightning whichLightning returns boolean

/**


@patch 1.24

*/
native  SaveImageHandle					takes hashtable table, integer parentKey, integer childKey, image whichImage returns boolean

/**


@patch 1.24

*/
native  SaveUbersplatHandle				takes hashtable table, integer parentKey, integer childKey, ubersplat whichUbersplat returns boolean

/**


@patch 1.24

*/
native  SaveRegionHandle				takes hashtable table, integer parentKey, integer childKey, region whichRegion returns boolean

/**


@patch 1.24

*/
native  SaveFogStateHandle				takes hashtable table, integer parentKey, integer childKey, fogstate whichFogState returns boolean

/**


@patch 1.24

*/
native  SaveFogModifierHandle			takes hashtable table, integer parentKey, integer childKey, fogmodifier whichFogModifier returns boolean

/**


@patch 1.24b

*/
native  SaveAgentHandle					takes hashtable table, integer parentKey, integer childKey, agent whichAgent returns boolean

/**


@patch 1.24b

*/
native  SaveHashtableHandle				takes hashtable table, integer parentKey, integer childKey, hashtable whichHashtable returns boolean

/**


@patch 1.31

*/
native  SaveFrameHandle					takes hashtable table, integer parentKey, integer childKey, framehandle whichFrameHandle returns boolean



/**


@patch 1.24

*/
native  LoadInteger					takes hashtable table, integer parentKey, integer childKey returns integer

/**


@patch 1.24

*/
native  LoadReal					takes hashtable table, integer parentKey, integer childKey returns real

/**


@patch 1.24

*/
native  LoadBoolean				    takes hashtable table, integer parentKey, integer childKey returns boolean

/**


@patch 1.24

*/
native  LoadStr 					takes hashtable table, integer parentKey, integer childKey returns string

/**


@patch 1.24

*/
native  LoadPlayerHandle			takes hashtable table, integer parentKey, integer childKey returns player

/**


@patch 1.24

*/
native  LoadWidgetHandle			takes hashtable table, integer parentKey, integer childKey returns widget

/**


@patch 1.24

*/
native  LoadDestructableHandle		takes hashtable table, integer parentKey, integer childKey returns destructable

/**


@patch 1.24

*/
native  LoadItemHandle				takes hashtable table, integer parentKey, integer childKey returns item

/**


@patch 1.24

*/
native  LoadUnitHandle				takes hashtable table, integer parentKey, integer childKey returns unit

/**


@patch 1.24

*/
native  LoadAbilityHandle			takes hashtable table, integer parentKey, integer childKey returns ability

/**


@patch 1.24

*/
native  LoadTimerHandle				takes hashtable table, integer parentKey, integer childKey returns timer

/**


@patch 1.24

*/
native  LoadTriggerHandle			takes hashtable table, integer parentKey, integer childKey returns trigger

/**


@patch 1.24

*/
native  LoadTriggerConditionHandle	takes hashtable table, integer parentKey, integer childKey returns triggercondition

/**


@patch 1.24

*/
native  LoadTriggerActionHandle		takes hashtable table, integer parentKey, integer childKey returns triggeraction

/**


@patch 1.24

*/
native  LoadTriggerEventHandle		takes hashtable table, integer parentKey, integer childKey returns event

/**


@patch 1.24

*/
native  LoadForceHandle				takes hashtable table, integer parentKey, integer childKey returns force

/**


@patch 1.24

*/
native  LoadGroupHandle				takes hashtable table, integer parentKey, integer childKey returns group

/**


@patch 1.24

*/
native  LoadLocationHandle			takes hashtable table, integer parentKey, integer childKey returns location

/**


@patch 1.24

*/
native  LoadRectHandle				takes hashtable table, integer parentKey, integer childKey returns rect

/**


@patch 1.24

*/
native  LoadBooleanExprHandle		takes hashtable table, integer parentKey, integer childKey returns boolexpr

/**


@patch 1.24

*/
native  LoadSoundHandle				takes hashtable table, integer parentKey, integer childKey returns sound

/**


@patch 1.24

*/
native  LoadEffectHandle			takes hashtable table, integer parentKey, integer childKey returns effect

/**


@patch 1.24

*/
native  LoadUnitPoolHandle			takes hashtable table, integer parentKey, integer childKey returns unitpool

/**


@patch 1.24

*/
native  LoadItemPoolHandle			takes hashtable table, integer parentKey, integer childKey returns itempool

/**


@patch 1.24

*/
native  LoadQuestHandle				takes hashtable table, integer parentKey, integer childKey returns quest

/**


@patch 1.24

*/
native  LoadQuestItemHandle			takes hashtable table, integer parentKey, integer childKey returns questitem

/**


@patch 1.24

*/
native  LoadDefeatConditionHandle	takes hashtable table, integer parentKey, integer childKey returns defeatcondition

/**


@patch 1.24

*/
native  LoadTimerDialogHandle		takes hashtable table, integer parentKey, integer childKey returns timerdialog

/**


@patch 1.24

*/
native  LoadLeaderboardHandle		takes hashtable table, integer parentKey, integer childKey returns leaderboard

/**


@patch 1.24

*/
native  LoadMultiboardHandle		takes hashtable table, integer parentKey, integer childKey returns multiboard

/**


@patch 1.24

*/
native  LoadMultiboardItemHandle	takes hashtable table, integer parentKey, integer childKey returns multiboarditem

/**


@patch 1.24

*/
native  LoadTrackableHandle			takes hashtable table, integer parentKey, integer childKey returns trackable

/**


@patch 1.24

*/
native  LoadDialogHandle			takes hashtable table, integer parentKey, integer childKey returns dialog

/**


@patch 1.24

*/
native  LoadButtonHandle			takes hashtable table, integer parentKey, integer childKey returns button

/**


@patch 1.24

*/
native  LoadTextTagHandle			takes hashtable table, integer parentKey, integer childKey returns texttag

/**


@patch 1.24

*/
native  LoadLightningHandle			takes hashtable table, integer parentKey, integer childKey returns lightning

/**


@patch 1.24

*/
native  LoadImageHandle				takes hashtable table, integer parentKey, integer childKey returns image

/**


@patch 1.24

*/
native  LoadUbersplatHandle			takes hashtable table, integer parentKey, integer childKey returns ubersplat

/**


@patch 1.24

*/
native  LoadRegionHandle			takes hashtable table, integer parentKey, integer childKey returns region

/**


@patch 1.24

*/
native  LoadFogStateHandle			takes hashtable table, integer parentKey, integer childKey returns fogstate

/**


@patch 1.24

*/
native  LoadFogModifierHandle		takes hashtable table, integer parentKey, integer childKey returns fogmodifier

/**


@patch 1.24

*/
native  LoadHashtableHandle			takes hashtable table, integer parentKey, integer childKey returns hashtable

/**


@patch 1.31

*/
native  LoadFrameHandle				takes hashtable table, integer parentKey, integer childKey returns framehandle


/**


@patch 1.24

*/
native  HaveSavedInteger					takes hashtable table, integer parentKey, integer childKey returns boolean

/**


@patch 1.24

*/
native  HaveSavedReal						takes hashtable table, integer parentKey, integer childKey returns boolean

/**


@patch 1.24

*/
native  HaveSavedBoolean					takes hashtable table, integer parentKey, integer childKey returns boolean

/**


@patch 1.24

*/
native  HaveSavedString					    takes hashtable table, integer parentKey, integer childKey returns boolean

/**


@patch 1.24

*/
native  HaveSavedHandle     				takes hashtable table, integer parentKey, integer childKey returns boolean


/**


@patch 1.24

*/
native  RemoveSavedInteger					takes hashtable table, integer parentKey, integer childKey returns nothing

/**


@patch 1.24

*/
native  RemoveSavedReal						takes hashtable table, integer parentKey, integer childKey returns nothing

/**


@patch 1.24

*/
native  RemoveSavedBoolean					takes hashtable table, integer parentKey, integer childKey returns nothing

/**


@patch 1.24

*/
native  RemoveSavedString					takes hashtable table, integer parentKey, integer childKey returns nothing

/**


@patch 1.24

*/
native  RemoveSavedHandle					takes hashtable table, integer parentKey, integer childKey returns nothing


/**


@patch 1.24

*/
native  FlushParentHashtable						takes hashtable table returns nothing

/**


@patch 1.24

*/
native  FlushChildHashtable					takes hashtable table, integer parentKey returns nothing


//============================================================================
// Randomization API

/**
Returns a random integer in the range [lowBound, highBound] (inclusive).
Bounds may be negative, but should be lowBound <= highBound.
When lowBound==highBound, always returns that number.

@param lowBound The inclusive lower bound of the random number returned.

@param highBound The inclusive higher bound of the random number returned.


@note If lowBound > highBound then it just swaps the values.

@bug If you call `GetRandomInt(INT_MIN, INT_MAX)` or `GetRandomInt(INT_MAX, INT_MIN)`
it will always return the same value, namely `INT_MIN` or `INT_MAX`.

@note See <http://hiveworkshop.com/threads/random.286109#post-3073222> for an overview of the algorithm used.

@note **Desyncs!** The random number generator is a global, shared resource.
Do not change its state in local blocks asynchronously.

@note See: `GetRandomReal`, `SetRandomSeed`.

*/
native GetRandomInt takes integer lowBound, integer highBound returns integer

/**
Returns a real in range [lowBound, highBound) that is: inclusive, exclusive.
Bounds may be negative, but must be lowBound <= highBound. When lowBound==highBound, always returns that number.

**Example (Lua):**

	SetRandomSeed(1229611)
	string.format("%.16f", GetRandomReal(0, 0.002)) == "0.00"
	SetRandomSeed(1229611)
	string.format("%.16f", GetRandomReal(-0.002, 0)) == "-0.002"
	

@note **Desyncs!** The random number generator is a global, shared resource. Do not change its state in local blocks asynchronously.

@note Undefined behavior when lowBound > highBound. Test code:

	-- Set seed to zero and then generate and print a random real
	function testRReal(low, high) SetRandomSeed(0); return string.format("%.16f", GetRandomReal(low,high)) end
	testRReal(-42, 42) == "-4.0800933837890625"
	testRReal(42, -42) == "79.9199066162109375"

@note See: `GetRandomInt`, `SetRandomSeed`.

*/
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

@param whichItemPool The itempool to add the item to.

@param itemId The rawcode of the item.
An invalid itemId (like 0) can be added & rolled.

@param weight The weight of the item.
The weight determines how likely it is for the item to be chose by `PlaceRandomItem`.


*/
native ItemPoolAddItemType      takes itempool whichItemPool, integer itemId, real weight returns nothing
native ItemPoolRemoveItemType   takes itempool whichItemPool, integer itemId returns nothing

/**
Draws a random itemid from the itempool and creates the item.

@param whichItemPool The itempool to draw from.

@param x The x-coordinate of the item.

@param y The y-coordinate of the item.


*/
native PlaceRandomItem          takes itempool whichItemPool, real x, real y returns item

// Choose any random unit/item. (NP means Neutral Passive)

/**
Returns the rawcode ID of a random unit of the specified level. The unit chosen
will come from the set of units that include or are assigned to the base tileset
of the map. Passing a level of -1 is equivalent to picking a creep of any level.
If there are no units of the specified level, the returned value is 0.

@param level The level of the units to choose from.


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

@param level The level of the items to choose from. Passing a level of -1 is equivalent to any level.


@note The item returned is not chosen from preplaced items on the map, but rather any item of that level.

*/
native ChooseRandomItem         takes integer level returns integer

/**
Returns the rawcode ID of a random item of the specified level and item type.
Passing a level of -1 will return an item of any level. If there are no items
of the specified level, the id returned will be 0.

@param whichType The classification of items to choose from.

@param level The level of the items to choose from. Passing a level of -1 is equivalent to any level.


@note The item returned is not chosen from preplaced items on the map, but rather any item of that level.

*/
native ChooseRandomItemEx       takes itemtype whichType, integer level returns integer

/**
Sets the internal [PRNG's](https://en.wikipedia.org/wiki/Pseudorandom_number_generator) seed.

Useful for testing or when you want a repeatable outcome. WorldEdit has an option to run test maps with a fixed seed, you can achieve the same result with this.

**Example:**

	SetRandomSeed(42)
	GetRandomInt(0, 18) == 12
	GetRandomInt(0, 18) == 2
	SetRandomSeed(42)
	GetRandomInt(0, 18) == 12
	
@param seed New seed for the PRNG.

@note **Desyncs!** The random number generator is a global, shared resource. Do not change its state in local blocks asynchronously.

@note See: `GetRandomInt`, `GetRandomReal`.


*/
native SetRandomSeed            takes integer seed returns nothing

//============================================================================
// Visual API

/**


@bug Does nothing (unknown, unused).

*/
native SetTerrainFog                takes real a, real b, real c, real d, real e returns nothing
native ResetTerrainFog              takes nothing returns nothing


/**
Unknown, unused.


*/
native SetUnitFog                   takes real a, real b, real c, real d, real e returns nothing
native SetTerrainFogEx              takes integer style, real zstart, real zend, real density, real red, real green, real blue returns nothing

/**
Displays a trigger message to player.

The text line fades out in the end.

@param toPlayer target player
@param x new text box position (default is 0, clamped to: 0.0-1.0)
@param y new text box position (default is 0, clamped to: 0.0-1.0)
@param message text (supports color codes)

@bug Changing x or y moves the entire text box, including previously displayed lines.
An example is shown at 
[Luashine/DisplayTextToPlayer-position](https://github.com/Luashine/wc3-test-maps/blob/master/DisplayTextToPlayer-position/DisplayTextToPlayer-position.md)

@note The text lines are bottom-left aligned: text continues to the right and new lines
continue upwards.

@note This is equivalent to `DisplayTimedTextToPlayer` with `duration` set to 4.

@note See: `DisplayTimedTextToPlayer`, `DisplayTimedTextFromPlayer`, `BlzDisplayChatMessage`.


*/
native DisplayTextToPlayer          takes player toPlayer, real x, real y, string message returns nothing

/**
Displays a trigger message to player with a custom display duration.

The text line fades out in the end.

@param toPlayer target player
@param x new text box position (default is 0, clamped to: 0.0-1.0)
@param y new text box position (default is 0, clamped to: 0.0-1.0)
@param duration text lifetime in seconds
@param message text (supports color codes)

@note See: `DisplayTextToPlayer` for the full description.
Also: `DisplayTimedTextFromPlayer`, `BlzDisplayChatMessage`.


*/
native DisplayTimedTextToPlayer     takes player toPlayer, real x, real y, real duration, string message returns nothing

/**
Displays a trigger message to *all* players but the first "%s" in the message will
be replaced by `GetPlayerName(toPlayer)`.

@param toPlayer this player's name will be used to replace the `%s` placeholder
@param x new text box position (default is 0, clamped to: 0.0-1.0)
@param y new text box position (default is 0, clamped to: 0.0-1.0)
@param duration text lifetime in seconds
@param message text (supports color codes), may contain only one `%s` placeholder

@bug Only the first "%s" will be replaced correctly. Following "%s" will be
printed as garbage or (v1.32.10, Lua) crash the game.

Using formatters like "%i" will also print garbage and following "%s" wont
work either.

See: [C stdlib printf documentation](https://cplusplus.com/reference/cstdio/printf/).

@note A better name for the parameter `toPlayer` would be `fromPlayer`.

@note See: `DisplayTextToPlayer` for the full description.
Also: `DisplayTimedTextToPlayer`, `BlzDisplayChatMessage`.


*/
native DisplayTimedTextFromPlayer   takes player toPlayer, real x, real y, real duration, string message returns nothing

/**
Clears all messages displayed via triggers. All messages will still show up in the message log, however.


@note This does not remove player chat messages.

*/
native ClearTextMessages            takes nothing returns nothing
native SetDayNightModels            takes string terrainDNCFile, string unitDNCFile returns nothing

/**


@patch 1.32

*/
native SetPortraitLight             takes string portraitDNCFile returns nothing
native SetSkyModel                  takes string skyModelFile returns nothing

/**
Toggles user's input controls.

When disabled this includes:

- hide the cursor (you can still see UI on-hover effects with menu buttons and
even mouse-down animation on ability buttons)
- on-hover unit selection circles no longer show (cannot be overriden with
`EnableDragSelect`, `EnablePreSelect`, `EnableSelect`)
- disable all hotkeys (binds, abilities, minimap, menus like F10), only Alt+F4 continues to work

@param b `true` to enable control, `false` to disable


*/
native EnableUserControl            takes boolean b returns nothing

/**
Toggles the display of tooltips, other features unknown (v1.32.10).
Group hotkeys, selection etc. continue to work.

@param b `true` to enable tooltips, `false` to disable the display of tooltips.


*/
native EnableUserUI                 takes boolean b returns nothing

/**
Controls the ticking of the in-game day/night time.

@param b `true` to stop time ticking, `false` to enable time progression (default).


*/
native SuspendTimeOfDay             takes boolean b returns nothing

/**
Sets the speed of the in-game day/night time.

By default: `1.0` or 100%. `2.0` would make it twice as fast.

@param r new scaling factor


@bug A negative scaling factor is applied and the time ticks backwards until
00:00 is reached. Then the time freezes at 00:00, the day does not progress backwards.

*/
native SetTimeOfDayScale            takes real r returns nothing

/**
Returns the speed of the in-game day/night time (a scaling factor).
By default: `1.0` or 100%.


*/
native GetTimeOfDayScale            takes nothing returns real

/**


@bug If fadeDuration equals 0 the unit portrait always appears invisible.

*/
native ShowInterface                takes boolean flag, real fadeDuration returns nothing
native PauseGame                    takes boolean flag returns nothing

/**


@note See: `AddIndicator` (it is a more generic version as it takes a `widget`).

*/
native UnitAddIndicator             takes unit whichUnit, integer red, integer green, integer blue, integer alpha returns nothing

/**
Adds a blinking circle around widget with the color (red,green,blue,alpha).
The circle blinks twice. This function is commonly used for cinematic modes
and is seen in `TransmissionFromUnitWithNameBJ`.

@param whichWidget The widget the indicator will be applied to.
@param red 0-255 red color (value mod 256).
@param green 0-255 green color (value mod 256).
@param blue 0-255 blue color (value mod 256).
@param alpha 0-255 opacity (value mod 256). Determining the transparency
of the indicator. `0` is total transparency, `255` is total opacity.

@note The size of the indicator depends on a widget's selection size. To modify
this, you must edit the object editor field of the widget listed as "Art - Selection Size".

The indicator is shown below the unit selection.
If the unit is currently selected, the blinking indicator will be practically
hidden by the selection circle. For more see `SetImageType` description.

@note See: `UnitAddIndicator` (functionally equivalent to this widget version).


*/
native AddIndicator                 takes widget whichWidget, integer red, integer green, integer blue, integer alpha returns nothing

/**
Pings a spot on the minimap.

@param x horizontal world coordinate of the ping.
@param y vertical world coordinate of the ping.
@param duration duration of the ping in seconds.


@note This ping has the semantics of a "simple" ping (GUI/blizzard.j terminology).

@note As a "simple" ping, pings created with this function have a default shape of a rotating circle with 4 arrows
pointing inwards and periodically emitting a growing circle that fades out like a pulse. There is also a dot in the center.

@note This ping is neon green on default.

@note There can only be 16 pings at a time. When a new one is created but there are already 16,
the oldest will be deleted in favor of the new one. This includes user pings: user pings can be deleted by this function
and user pings can overwrite scripted pings.

*/
native PingMinimap                  takes real x, real y, real duration returns nothing

/**
Pings a spot on the minimap.

@param x horizontal world coordinate of the ping.
@param y vertical world coordinate of the ping.
@param duration duration of the ping in seconds.
@param red 0-255 red color (value mod 256).
@param green 0-255 green color (value mod 256).
@param blue 0-255 blue color (value mod 256).
@param extraEffects When true, the ping will have the appearance of a "flashy" ping. Otherwise it will be a "simple" ping (see notes).


@note "Simple" pings (GUI/blizzard.j terminology) have a default shape of a rotating circle with 4 arrows
pointing inwards and periodically emitting a growing circle that fades out like a pulse. There is also a dot in the center.

@note "Flashy" pings (GUI/blizzard.j terminology) have the same shape as user-generated pings. On default, they first feature an exclamation mark
and a large circle growing and fading out before going into a stable state where smaller circles are periodically emitted growing and fading out
like a pulse and there is a static exclamation mark in the center.

@note Pings with red == 255 && green == 0 && blue == 0 (mod 256) have a special shape, appearing as "attack" or "warning" pings (GUI/blizzard.j terminology).
On default, if extraEffects is false, it is similar to "simple" pings but the rotating arrows are flying in from outside before getting attached to the circle.
On default, if extraEffects is true, it additionally briefly shows an exclamation mark when the ping vanishes (bug?).

@note There can only be 16 pings at a time. When a new one is created but there are already 16,
the oldest will be deleted in favor of the new one. This includes user pings: user pings can be deleted by this function
and user pings can overwrite scripted pings.

*/
native PingMinimapEx                takes real x, real y, real duration, integer red, integer green, integer blue, boolean extraEffects returns nothing

/**


@patch 1.32

*/
native CreateMinimapIconOnUnit      takes unit whichUnit, integer red, integer green, integer blue, string pingPath, fogstate fogVisibility returns minimapicon

/**


@patch 1.32

*/
native CreateMinimapIconAtLoc       takes location where, integer red, integer green, integer blue, string pingPath, fogstate fogVisibility returns minimapicon

/**


@patch 1.32

*/
native CreateMinimapIcon            takes real x, real y, integer red, integer green, integer blue, string pingPath, fogstate fogVisibility returns minimapicon

/**


@patch 1.32

*/
native SkinManagerGetLocalPath      takes string key returns string

/**


@patch 1.32

*/
native DestroyMinimapIcon           takes minimapicon pingId returns nothing

/**


@patch 1.32

*/
native SetMinimapIconVisible        takes minimapicon whichMinimapIcon, boolean visible returns nothing

/**


@patch 1.32

*/
native SetMinimapIconOrphanDestroy  takes minimapicon whichMinimapIcon, boolean doDestroy returns nothing
native EnableOcclusion              takes boolean flag returns nothing
native SetIntroShotText             takes string introText returns nothing
native SetIntroShotModel            takes string introModelPath returns nothing
native EnableWorldFogBoundary       takes boolean b returns nothing
native PlayModelCinematic           takes string modelName returns nothing
native PlayCinematic                takes string movieName returns nothing

/**
Emulates a key press within the game. Seems to only work with latin alphabet, only for printable ASCII characters.


@note See `ForceUICancel` for limitations and bugs. Most importantly, the outcome is affected by local player's hotkey layout.

*/
native ForceUIKey                   takes string key returns nothing

/**
Emulates an ESCAPE key press internally, used to interact with UI, e.g. close F10 menu.


@bug Does not always work as expected if you use it to "Cancel" something on behalf of a player, like cancel research in the current building. Since it always sends the Escape key, it will break if hotkey layout was changed from classic to grid/custom in game settings. Explanation:

1. OldPlayer plays with classic hotkey layout, the Cancelling abilities are bound to Escape.
2. ModernPlayer plays with grid layout, the Cancelling abilities' hotkey depends on their position but it's usually V.
3. ForceUICancel() is executed for both players
4. OldPlayer executes a Cancel ability, nothing happens to ModernPlayer
5. The game doesn't desync because it thinks OldPlayer really pressed that key, and even though ModernPlayer did "press" it too, he didn't trigger Cancel for his unit.

@note Does not trigger (physical) player key events like `BlzTriggerRegisterPlayerKeyEvent`.

*/
native ForceUICancel                takes nothing returns nothing

/**
Opens the "Load game" menu where you can load a previous save.


@note Singleplayer only! This menu is disabled in multiplayer and nothing will happen.

*/
native DisplayLoadDialog            takes nothing returns nothing

/**
Sets the "alternative icon". You can display this icon for any unit via
`UnitSetUsesAltIcon`.


@note Only one icon can be the "alternative icon" but you can give each
player a different icon via `GetLocalPlayer`.

*/
native SetAltMinimapIcon            takes string iconPath returns nothing

/**
Toggles the "Restart Mission" button (found in: Menu (F10) -> End Game).

@param flag `true` to disable the button, `false` to allow game restarts by the player.

@note This button is only enabled in singleplayer (default),
you cannot enable it in multiplayer.


*/
native DisableRestartMission        takes boolean flag returns nothing

native CreateTextTag                takes nothing returns texttag
native DestroyTextTag               takes texttag t returns nothing
native SetTextTagText               takes texttag t, string s, real height returns nothing
native SetTextTagPos                takes texttag t, real x, real y, real heightOffset returns nothing
native SetTextTagPosUnit            takes texttag t, unit whichUnit, real heightOffset returns nothing
native SetTextTagColor              takes texttag t, integer red, integer green, integer blue, integer alpha returns nothing
native SetTextTagVelocity           takes texttag t, real xvel, real yvel returns nothing
native SetTextTagVisibility         takes texttag t, boolean flag returns nothing
native SetTextTagSuspended          takes texttag t, boolean flag returns nothing
native SetTextTagPermanent          takes texttag t, boolean flag returns nothing
native SetTextTagAge                takes texttag t, real age returns nothing
native SetTextTagLifespan           takes texttag t, real lifespan returns nothing
native SetTextTagFadepoint          takes texttag t, real fadepoint returns nothing

native SetReservedLocalHeroButtons  takes integer reserved returns nothing

/**
Returns the currently chosen player color display mode.

This is called "ally color mode" by the game (hotkey: Alt+A).

- `0` aka "Mode 1" (default):
    - Minimap: Player colors, youself are white
	- World: Unit colors same as player color
- `1` aka "Mode 2":
    - Minimap: Allies are teal, enemies are red, yourself are white
	- World: Unit colors same as player color
- `2` aka "Mode 3":
    - Minimap: Allies are teal, enemies are red, yourself are white
	- World: Allies are teal, enemies are red, own units are blue


@note See: `SetAllyColorFilterState`

@note This setting affects how a unit's "Art - Team Color" (WE name) is displayed.
If the models you use rely on this color to match player color,
you can choose to force state=0 with `SetAllyColorFilterState`.

@async 

*/
native GetAllyColorFilterState      takes nothing returns integer

/**
Sets the player color display mode.

@param state new state (only 0, 1, 2 are valid).
See `GetAllyColorFilterState` for a description.

@note This is a player setting. Do not change it without a reason.
Moreover this is an accessibility setting that may be used by visually impaired
players.

@bug You can set other states than 0-2, but they'll still display like state 0.

@bug (v1.32.10) You can permanently break this feature for a player
if you set a large negative value.

Any negative value will display like `state=0` and clicking the button
will increase the state by 1. However if you set a very large negative value,
the player will use the button to no avail. The issue here is that the value
will be saved in player's game settings and persist forever, thus breaking this
feature until a reinstall or until you set this to a sane value (0-2).

Using large positive values instantly reverts to `state=0` after the first button
click.

@note See: `GetAllyColorFilterState` for full description; `EnableMinimapFilterButtons`.


*/
native SetAllyColorFilterState      takes integer state returns nothing

/**
Returns `true` if the local player has enabled the display of creep camps on the minimap.

The creep camps are shown as green/orange/red circles by default and there's a button
next to the minimap to toggle it while playing (hotkey: Alt+R).


@note See: `SetCreepCampFilterState`, `GetAllyColorFilterState`

@async 

*/
native GetCreepCampFilterState      takes nothing returns boolean

/**
Toggles minimap creep display.

@param state `true` to highlight camps, `false` to hide

@note See: `GetCreepCampFilterState` for full description; `SetAllyColorFilterState`, `EnableMinimapFilterButtons`.


*/
native SetCreepCampFilterState      takes boolean state returns nothing

/**
Toggles the "player color display mode" and "minimap creep display" buttons.

When the buttons are disabled, the player cannot control the minimap appearance
or player colors (ally/enemy).

@param enableAlly `true` to enable the button (default), `false` to disable.
See: `GetAllyColorFilterState` for an explanation.

@param enableCreep `true` to enable the button (default), `false` to disable.
See: `GetCreepCampFilterState` for an explanation.

@note This controls a player setting. Do not change it without a reason.
Moreover this is an accessibility setting that may be used by visually impaired
players.

@note The buttons turn gray and their hotkeys stop working too.


*/
native EnableMinimapFilterButtons   takes boolean enableAlly, boolean enableCreep returns nothing

/**
Sets the functionality of the rectangular unit multi-select.

"Drag Select" allows you to hold left-click to select multiple units by
expanding the green selection rectangle over the units.

@param state If `true`, default game behavior (drag select is enabled).

If `false`, drag select is disabled. Only the first unit in the rectangle will
be selected (closest to the point where you first clicked the mouse).

Note that you can still select multiple units with Shift+Click even if drag
select is disabled.

@param ui If `true`, render the visual indicator that shows the green rectangular selection area (default).
Units, that are not yet selected but are inside the rectangle,
have a semi-transparent green circle around them.

If `false`, the green rectangle is not rendered.
This has no effect on `state`, Drag Select can still work without the visual indicator.


*/
native EnableDragSelect             takes boolean state, boolean ui returns nothing

/**
Sets the functionality when you hover over a unit with your cursor.

@param state unknown
@param ui If `true`, show semi-transparent green circle around the unit and the health bar etc.

If `false`, the green circle and the health bar is not shown.
The cursor still blinks green/yellow/red like when you hover over a unit.
The color depends on whether the unit is your own/ally/enemy.


*/
native EnablePreSelect              takes boolean state, boolean ui returns nothing

/**
Controls whether you can de/select any units and the green visual indicator.

@param state If `true`, you can de/select units (default).

If `false`, deselects any currently selected units and disables your ability
to select any unit. Mouse clicks and group binds ("CTRL+1" then press "1")
don't work any more.
Drag select will not allow you to select too.

@param ui If `true`, show the green selection indicator around selected units (default).

If `false`, no visual indicator is shown.

@note 
You can use `SelectUnit` and other functions to select the units for a player,
even when `state` is set to `false`.

The player cannot manually deselect any units they have control over (after `SelectUnit`).


*/
native EnableSelect                 takes boolean state, boolean ui returns nothing

//============================================================================
// Trackable API

/**
Creates a trackable at the given coordinates but with zero z-coordinate.
Trackables are used to register mouse clicks or hovers at the trackables
position. But their functionality is very limited, as you can't, for example
distinguish the triggering player out of the box. To get a general overview
to the common workarounds see the `trackable` documentation.

@param trackableModelPath The path to the model the trackable should use. Models
with team colours will use the neutral-hostile team colour. To create an
invisible trackable provide the empty string `""`.

@param x The x-coordinate where the trackable should be created.

@param y The x-coordinate where the trackable should be created.

@param facing The facing of the trackable.


@note To create a trackable with a non-zero z-coordinate you can use the same
technique as with `AddSpecialEffect`, that is create an invisible platform
before creating the trackable.

```
function CreateTrackableZ takes string trackableModelPath, real x, real y, real z, real facing returns trackable
    local destructable d = CreateDestructableZ('OTip', x, y, z, 0, 1, 0)
    local trackable t = CreateTrackable(trackableModelPath, x, y, facing)
    call RemoveDestructable(d)
    set d = null
    return t
endfunction
```


*/
native CreateTrackable      takes string trackableModelPath, real x, real y, real facing returns trackable

//============================================================================
// Quest API

/**


@bug Do not use this in a global initialisation as it crashes the game there.

*/
native CreateQuest          takes nothing returns quest
native DestroyQuest         takes quest whichQuest returns nothing
native QuestSetTitle        takes quest whichQuest, string title returns nothing
native QuestSetDescription  takes quest whichQuest, string description returns nothing
native QuestSetIconPath     takes quest whichQuest, string iconPath returns nothing

native QuestSetRequired     takes quest whichQuest, boolean required   returns nothing
native QuestSetCompleted    takes quest whichQuest, boolean completed  returns nothing
native QuestSetDiscovered   takes quest whichQuest, boolean discovered returns nothing
native QuestSetFailed       takes quest whichQuest, boolean failed     returns nothing
native QuestSetEnabled      takes quest whichQuest, boolean enabled    returns nothing

native IsQuestRequired     takes quest whichQuest returns boolean
native IsQuestCompleted    takes quest whichQuest returns boolean
native IsQuestDiscovered   takes quest whichQuest returns boolean
native IsQuestFailed       takes quest whichQuest returns boolean
native IsQuestEnabled      takes quest whichQuest returns boolean

native QuestCreateItem          takes quest whichQuest returns questitem
native QuestItemSetDescription  takes questitem whichQuestItem, string description returns nothing
native QuestItemSetCompleted    takes questitem whichQuestItem, boolean completed returns nothing

native IsQuestItemCompleted     takes questitem whichQuestItem returns boolean


/**
Defeat conditions tell players what conditions would warrant a defeat.
They are shown above all quest descriptions. Note that this function will only
display text. To put the condition in effect, you would need additional
triggering (i.e. registering when a unit dies to end the game). This updates
all quests with the list of defeat condition descriptions.
To actually set the text use `DefeatConditionSetDescription`.


@note Each defeat condition has a hyphen "-" symbol appended to the front.

*/
native CreateDefeatCondition            takes nothing returns defeatcondition
native DestroyDefeatCondition           takes defeatcondition whichCondition returns nothing
native DefeatConditionSetDescription    takes defeatcondition whichCondition, string description returns nothing

native FlashQuestDialogButton   takes nothing returns nothing
native ForceQuestDialogUpdate   takes nothing returns nothing

//============================================================================
// Timer Dialog API

/**
Creates a new timer dialog based on the underlying timer.
It is hidden by default and has "Remaining" as title (localized).

Timer dialog works as a visible countdown timer in the format: "Title hh:mm:ss".

Since this creates an object and returns a handle, it must be freed when no longer needed
with `DestroyTimerDialog`.

@param t connect the timer dialog to this timer, it'll always follow its
"time remaining".

@note (v1.32.10, Lua) If `t` is nil then the dialog is still created,
but will never show any time.

Alternatively, you can set the visible time with `TimerDialogSetRealTimeRemaining`.


*/
native CreateTimerDialog                takes timer t returns timerdialog

/**
Destroys the timer dialog and frees the handle.

This does not affect the timer you might have provided in `CreateTimerDialog`.

@param whichDialog target dialog.


*/
native DestroyTimerDialog               takes timerdialog whichDialog returns nothing

/**
Sets the shown dialog title. Replaces the default "Remaining" text.

@param whichDialog target dialog.
@param title new title.

@note Depending on font and version, there's enough space to display
14 full-width characters like "@" (at character). If the text is wider,
it is shortened and an ellipsis "..." is shown at the end.

@note See: `TimerDialogSetTitle`, `TimerDialogSetTitleColor`, `TimerDialogSetTimeColor`.


*/
native TimerDialogSetTitle              takes timerdialog whichDialog, string title returns nothing

/**
Sets the timer-dialogs color.

See: `TimerDialogSetTitle`, `TimerDialogSetTimeColor`.

@param whichDialog The timerdialog.
@param red 0-255 red color (value mod 256).
@param green 0-255 green color (value mod 256).
@param blue 0-255 blue color (value mod 256).
@param alpha (unused) 0-255 transparency, please set to 255.
A value of 0 is complete transparency, while a value of 255 is complete opacity.


*/
native TimerDialogSetTitleColor         takes timerdialog whichDialog, integer red, integer green, integer blue, integer alpha returns nothing

/**
Sets the timer-dialogs time color.

@param whichDialog The timerdialog.
@param red 0-255 red color (value mod 256).
@param green 0-255 green color (value mod 256).
@param blue 0-255 blue color (value mod 256).
@param alpha (unused) 0-255 transparency, please set to 255.
A value of 0 is complete transparency, while a value of 255 is complete opacity.

@note See: `TimerDialogSetTitleColor`.


*/
native TimerDialogSetTimeColor          takes timerdialog whichDialog, integer red, integer green, integer blue, integer alpha returns nothing

/**
Set a new multiplier for the shown time remaining. Default is `1.0`.

The multiplier factor is applied literally to the displayed time:
`timerTimeRemainingSec * speedMultFactor`.

@param whichDialog target dialog to modify the speed of.
@param speedMultFactor new multiplicator factor.

For factor `2.0` the displayed time will appear twice as fast (200% speed).

For factor `0.5` the displayed time will appear half as fast (50% speed).

Factor `0.0` will always display `00:00:00`.

@note It does not affect the underlying timer `t` from `CreateTimerDialog`.
If you set the speed too high, the display will not become smoother as
it updates roughly 2-3 times per second.


*/
native TimerDialogSetSpeed              takes timerdialog whichDialog, real speedMultFactor returns nothing

/**
Show/hide the dialog for all players.

A timer dialog is displayed above a multiboard in the top-right corner.

@param whichDialog target dialog.
@param display `true` to show, `false` to hide.

@note Multiple timer dialogues stack from right to left, for example:
"Timer dialog 2  12:34:56" "Timer dialog 1  02:10:42".

@note If the timer has not been started yet, it will not show any time:
"Remaining ".

@note A dialog display can be toggled per-player by using it inside a
`GetLocalPlayer` condition.

@bug (v1.32.10) The second timerdialog's width and position is calculated and
displayed incorrectly in ultra-wide mode (beyond 1800x920, 1.95 ratio).

@bug (v1.32.10) If you toggle visibility of one dialog but not the other
in a single frame, the first dialog will appear below the second one.

```{.lua}
	tdialog = CreateTimerDialog(CreateTimer())
	TimerDialogSetTitle(tdialog, "Timer1 Dialog __ 1")
	TimerDialogDisplay(tdialog, true)
	tdialog2 = CreateTimerDialog(CreateTimer())
	TimerDialogSetTitle(tdialog2, "Timer2 Dialog")
	TimerDialogDisplay(tdialog2, true)
	-- Correct up to this point.
	-- This is buggy:
	TimerDialogDisplay(tdialog, false)
	TimerDialogDisplay(tdialog2, true)
	TimerDialogDisplay(tdialog, true)
	-- Now tdialog will appear beneath tdialog2.
```

**Workarounds:**

1. Hide *every* dialog, then show those that you need.
2. Introduce a sleep-wait before turning dialog display on.

@note See: `IsTimerDialogDisplayed`.


*/
native TimerDialogDisplay               takes timerdialog whichDialog, boolean display returns nothing

/**
Returns `true` if the dialog is shown, `false` if it is hidden.

@param whichDialog check visibility of this timer dialog.

@note See: `TimerDialogDisplay`.


*/
native IsTimerDialogDisplayed           takes timerdialog whichDialog returns boolean

/**
Sets the timer dialog countdown to specified time and decouples it from the
provided timer in `CreateTimerDialog`.

@param whichDialog target dialog.
@param timeRemaining new time in seconds.

@note For example if the dialog was created with a periodic timer, it would
reset the countdown like the timer when it reaches zero.

Once you set a custom time with this function, it will no longer follow the
timer. Once it reaches zero, it'll stay at zero.

@note There's no way to retrieve the internal timer value or to have an event
trigger.


*/
native TimerDialogSetRealTimeRemaining  takes timerdialog whichDialog, real timeRemaining returns nothing

//============================================================================
// Leaderboard API

// Create a leaderboard object

/**
Creates a leaderboard handle.
Leaderboards initially have 0 rows, 0 columns, and no label.


@bug Do not use this in a global initialisation as it crashes the game there.

*/
native CreateLeaderboard                takes nothing returns leaderboard
native DestroyLeaderboard               takes leaderboard lb returns nothing

native LeaderboardDisplay               takes leaderboard lb, boolean show returns nothing
native IsLeaderboardDisplayed           takes leaderboard lb returns boolean

native LeaderboardGetItemCount          takes leaderboard lb returns integer

native LeaderboardSetSizeByItemCount    takes leaderboard lb, integer count returns nothing
native LeaderboardAddItem               takes leaderboard lb, string label, integer value, player p returns nothing
native LeaderboardRemoveItem            takes leaderboard lb, integer index returns nothing
native LeaderboardRemovePlayerItem      takes leaderboard lb, player p returns nothing
native LeaderboardClear                 takes leaderboard lb returns nothing

native LeaderboardSortItemsByValue      takes leaderboard lb, boolean ascending returns nothing
native LeaderboardSortItemsByPlayer     takes leaderboard lb, boolean ascending returns nothing
native LeaderboardSortItemsByLabel      takes leaderboard lb, boolean ascending returns nothing

native LeaderboardHasPlayerItem         takes leaderboard lb, player p returns boolean
native LeaderboardGetPlayerIndex        takes leaderboard lb, player p returns integer
native LeaderboardSetLabel              takes leaderboard lb, string label returns nothing
native LeaderboardGetLabelText          takes leaderboard lb returns string

native PlayerSetLeaderboard             takes player toPlayer, leaderboard lb returns nothing
native PlayerGetLeaderboard             takes player toPlayer returns leaderboard

native LeaderboardSetLabelColor         takes leaderboard lb, integer red, integer green, integer blue, integer alpha returns nothing
native LeaderboardSetValueColor         takes leaderboard lb, integer red, integer green, integer blue, integer alpha returns nothing
native LeaderboardSetStyle              takes leaderboard lb, boolean showLabel, boolean showNames, boolean showValues, boolean showIcons returns nothing

native LeaderboardSetItemValue          takes leaderboard lb, integer whichItem, integer val returns nothing
native LeaderboardSetItemLabel          takes leaderboard lb, integer whichItem, string val returns nothing
native LeaderboardSetItemStyle          takes leaderboard lb, integer whichItem, boolean showLabel, boolean showValue, boolean showIcon returns nothing
native LeaderboardSetItemLabelColor     takes leaderboard lb, integer whichItem, integer red, integer green, integer blue, integer alpha returns nothing
native LeaderboardSetItemValueColor     takes leaderboard lb, integer whichItem, integer red, integer green, integer blue, integer alpha returns nothing

//============================================================================
// Multiboard API
//============================================================================

// Create a multiboard object

/**
Creates a new multiboard and returns its handle.

The new multiboard by default:

- does not have a title
- row and column count are 0
- is not displayed
- is not minimized

To display a multiboard after creation, you must use `MultiboardDisplay`.


@note Multiboards must be destroyed to prevent leaks: `DestroyMultiboard`.

@note Only one multiboard can be visible at a time.
However there's a workaround using [Frame API](https://www.hiveworkshop.com/threads/ui-showing-3-multiboards.316610/).

@note There's a bug that causes big multiboards to
[freeze/crash the game on 1.33](https://www.hiveworkshop.com/threads/maximizing-the-multiboard-leads-to-freezing-game-with-the-latest-reforged-patch.341873/#post-3550996).

@bug Do not use this in a global initialisation as it crashes the game there.

*/
native CreateMultiboard                 takes nothing returns multiboard

/**
Destroys the multiboard and frees the handle.


@bug **Fixed in 1.33:** Crash on 1.30-1.32.10 (earlier?) when a multiboard is destroyed
while `ShowInterface` is false for a player and the game crashes later, once turned on.

`ShowInterface` is used by the cinematic mode, also known as "letterbox mode" as GUI trigger.

**Workaround:** hide the multiboard before destroying it, see: `MultiboardMinimize`.

**Bug reports:**
[Cinematic mode, multiboard](https://www.hiveworkshop.com/threads/fatal-error-after-cinematics.316707/post-3358087),
[toggling letterbox mode](https://www.hiveworkshop.com/threads/1-31-1-bug-destroymultiboard-causes-crash-after-disabling-letterbox.315554/),
[multiboard](https://www.hiveworkshop.com/threads/destroying-or-hiding-timer-window-causes-game-to-crash.310883/post-3312587).

*/
native DestroyMultiboard                takes multiboard lb returns nothing


/**
Shows or hides the multiboard.

Can be used to force a multiboard update.

@param lb Target multiboard
@param show `true` to show, `false` to hide.


@note Multiboards can not be shown at map-init. Use a wait or a zero-timer to
display as soon as possible.

@note See: `IsMultiboardDisplayed`.

@bug `MultiboardDisplay(mb,false)`, where mb is an arbitrary non-null multiboard
will close any open multiboard, regardless of whether it's `mb` or not.
<http://www.wc3c.net/showthread.php?p=971681#post971681>

*/
native MultiboardDisplay                takes multiboard lb, boolean show returns nothing

/**
Returns true if multiboard is visible, false if not shown.

@param lb Target multiboard.


@note See: `MultiboardDisplay`.

*/
native IsMultiboardDisplayed            takes multiboard lb returns boolean


/**
Minimizes/maximizes the multiboard. This is equivalent to clicking the small ↑ ↓ buttons in-game.

Can be used to force a multiboard update.

A maximized multiboard shows its contents and draws the content borders, even
if it has 0 rows and columns. When minimized only the title is shown.


@note See: `IsMultiboardMinimized`.

*/
native MultiboardMinimize               takes multiboard lb, boolean minimize returns nothing

/**
Returns true if minimized, false if maximized.

@param lb Target multiboard.


@async 

@note See: `MultiboardMinimize`.

*/
native IsMultiboardMinimized            takes multiboard lb returns boolean

/**
Erases all items in a multiboard and sets row count to 0, column count to 0.
The multiboard's name is preserved.

@param lb Target multiboard.


@note *Implementation-specific:* Clearing a multiboard does not automatically invalidate
previous `multiboarditem` handles. If you expand the multiboard again, you'll be able to reuse
old handles. BUT you really shouldn't be doing this, it seems to be a buggy/undefined behavior.
When you clear or shrink a table, it's best to release old cell (item) handles with `MultiboardReleaseItem`.

@note See: `DestroyMultiboard` to remove, `MultiboardDisplay` to hide a multiboard.

*/
native MultiboardClear                  takes multiboard lb returns nothing


/**
Sets a multiboard's name.

The new text appears instantly.
The multiboard will expand as wide as necessary to display the title.

@param lb Target multiboard.
@param label New name.

@note See: `MultiboardGetTitleText`


*/
native MultiboardSetTitleText           takes multiboard lb, string label returns nothing

/**
Returns multiboard's name.


@note See: `MultiboardSetTitleText`.

*/
native MultiboardGetTitleText           takes multiboard lb returns string

/**
Sets the default color for multiboard name.

This is different than using color codes. If you use a color code in text,
it will override this color.

@param lb Target multiboard.
@param red 0-255 red color (value mod 256).
@param green 0-255 green color (value mod 256).
@param blue 0-255 blue color (value mod 256).
@param alpha (unused) 0-255 transparency, please set to 255.
A value of 0 is complete transparency, while a value of 255 is complete opacity.


@note You can use this to avoid using color tags and text manipulation in code.

@note See: `MultiboardSetItemValueColor`.

*/
native MultiboardSetTitleTextColor      takes multiboard lb, integer red, integer green, integer blue, integer alpha returns nothing


/**
Returns the number of content rows (lines, horizontal) for the multiboard.

@param lb Target multiboard.

@note See: `MultiboardSetRowCount`, `MultiboardGetColumnCount`.


*/
native MultiboardGetRowCount            takes multiboard lb returns integer

/**
Returns the number of content columns (vertical) for the multiboard.

@param lb Target multiboard.

@note See: `MultiboardSetColumnCount`.


*/
native MultiboardGetColumnCount         takes multiboard lb returns integer


/**
Sets the number of content columns (vertical) for the multiboard.

@param lb Target multiboard.

@note See: `MultiboardGetColumnCount`.


*/
native MultiboardSetColumnCount         takes multiboard lb, integer count returns nothing

/**
Sets the number of content rows (lines, horizontal) for the multiboard.

@param lb Target multiboard.

@bug It is only safe to change the row count by one. Use multiple calls for bigger values.
<http://www.hiveworkshop.com/forums/l-715/m-250775/> (has test map)
<http://www.hiveworkshop.com/forums/t-269/w-234897/> (has only code)

@note See: `MultiboardGetRowCount`.


*/
native MultiboardSetRowCount            takes multiboard lb, integer count returns nothing

// broadcast settings to all items

/**
Sets rendering properties for all cells.

@param lb Target multiboard.

@note See: `MultiboardSetItemStyle` for a detailed description.


*/
native MultiboardSetItemsStyle          takes multiboard lb, boolean showValues, boolean showIcons returns nothing

/**
Sets new text for all cells.

@param lb Target multiboard.

@note See: `MultiboardSetItemValue` for a detailed description.


*/
native MultiboardSetItemsValue          takes multiboard lb, string value returns nothing

/**
Sets the default color for text in all cell.

This is different than using color codes. If you use a color code in text,
it will override this color.

@param lb Target multiboard.
@param red 0-255 red color (value mod 256).
@param green 0-255 green color (value mod 256).
@param blue 0-255 blue color (value mod 256).
@param alpha (unused) 0-255 alpha color, please set to 255.


@note You can use this to avoid using color tags and text manipulation in code.

@note See: `MultiboardSetItemValueColor`.

*/
native MultiboardSetItemsValueColor     takes multiboard lb, integer red, integer green, integer blue, integer alpha returns nothing

/**
Sets the new width for all cells.

@param lb Target multiboard.
@param width New cell width expressed as screen width. `1.0` = 100% of screen width,
`0.05` = 5% of screen width.


@note See: `MultiboardSetItemWidth` for a detailed description.

*/
native MultiboardSetItemsWidth          takes multiboard lb, real width returns nothing

/**
Sets a new icon for all cells.

@param lb Target multiboard.
@param iconPath Path to new icon texture.

@note See: `MultiboardSetItemIcon` for a detailed description.


*/
native MultiboardSetItemsIcon           takes multiboard lb, string iconPath returns nothing


// funcs for modifying individual items

/**
Acquires and returns a new handle for the multiboard cell.

@param lb Target multiboard.
@param row In which row is the target cell (Y-coord, up-down). Starts from 0.
@param column in which column is the target cell (X-coord, left-right). Starts from 0.

@note Because a new handle is created each time, the handle must be
freed with `MultiboardReleaseItem`. The handle is different even if you
retrieve the same cell of the multiboard (v1.32.10, Lua).

@note The parameter order of `row` and `column` is (y,x) if you think of coordinates.


*/
native MultiboardGetItem                takes multiboard lb, integer row, integer column returns multiboarditem

/**
Destroys the handle previously created with `MultiboardGetItem`.

It must be used to prevent leaks. Releasing the handle does not destroy or modify the
item.


*/
native MultiboardReleaseItem            takes multiboarditem mbi returns nothing


/**
Sets rendering properties of the multiboard cell.
Hiding the icon or text does not erase it.

There is no way to get a cell's style.

@param mbi Target cell handle.
@param showValue `true` to render text, `false` to hide text.
@param showIcon `true` to render icon, `false` to hide icon.


@note See: `MultiboardSetItemsStyle`.

*/
native MultiboardSetItemStyle           takes multiboarditem mbi, boolean showValue, boolean showIcon returns nothing

/**
Sets the cell's text. It is empty by default.

@param mbi Target cell handle.
@param val New text.


@note You must make sure the new text will fit in current width by setting
`MultiboardSetItemWidth` appropriately. If the width is too small, the text will be
cut off.

@note See: `MultiboardSetItemsValue`.

*/
native MultiboardSetItemValue           takes multiboarditem mbi, string val returns nothing

/**
Sets the default color for the cell text.

This is different than using color codes. If you use a color code in text,
it will override this color.

@param red 0-255 red color (value mod 256).
@param green 0-255 green color (value mod 256).
@param blue 0-255 blue color (value mod 256).
@param alpha (unused) 0-255 alpha color, please set to 255.


@note You can use this to avoid using color tags and text manipulation in code.

@note See: `MultiboardSetItemsValueColor`.

*/
native MultiboardSetItemValueColor      takes multiboarditem mbi, integer red, integer green, integer blue, integer alpha returns nothing

/**
Sets the new text width for the cell.

Default width is `0.03` (3%), this is enough to fit 1-3 characters
(depending on font and character).

@param mbi Target cell handle.
@param width New cell width expressed as screen width. `1.0` = 100% of screen width,
`0.05` = 5% of screen width.

The multiboard is right-aligned (begins at the right) at the window border.
See Tasyen's
[The Big UI-Frame Tutorial](https://www.hiveworkshop.com/pastebin/e23909d8468ff4942ccea268fbbcafd1.20598#PosFrames)
for the explanation of screen width.


@bug **NOTE!** Multiboard's total width is calculated based on ONLY the first row's
widths.

*Example:* Your first row is very short, but second row is twice is long.

*Result:* The second row will not fit inside the table and overflow to the right,
beyond the visible area.

**Summary:** To set the multiboard width, set the width of columns in the first row.

@bug Although the column width is set immediately and items in the same row are
moved left/right, the multiboard is not redrawn to accomodate the new width.

To update the entire multiboard's width, you must manually minimize/maximize
the multiboard or call `MultiboardDisplay(udg_myMultiboard, true)`
or `MultiboardMinimize(udg_myMultiboard, false)`.

For example, if you only change the width of cell at (x=0, y=0) to
be 0.2x of screen width, then the cell (x=1, y=0) will be moved right beyond the
visible screen.

@note See: `MultiboardSetItemsWidth`.

*/
native MultiboardSetItemWidth           takes multiboarditem mbi, real width returns nothing

/**
Sets the cell's icon. It is a grey eye icon by default.

@param mbi Target cell handle.
@param iconFileName Path to new icon texture.


@note Setting an invalid texture path will result in an undefined texture (100% green).

@note See: `MultiboardSetItemsIcon`.

*/
native MultiboardSetItemIcon            takes multiboarditem mbi, string iconFileName returns nothing

// meant to unequivocally suspend display of existing and
// subsequently displayed multiboards
//

/**
While enabled, completely stops displaying any multiboards. It does not modify
any multiboards' display state. Useful for cinematics.

Once disabled, shows the last displayed (enabled) multiboard.

@param flag `true` to not render any multiboards, `false` to render multiboards.


@note See: `MultiboardDisplay` to modify an individual multiboard.

*/
native MultiboardSuppressDisplay        takes boolean flag returns nothing

//============================================================================
// Camera API
native SetCameraPosition            takes real x, real y returns nothing
native SetCameraQuickPosition       takes real x, real y returns nothing
native SetCameraBounds              takes real x1, real y1, real x2, real y2, real x3, real y3, real x4, real y4 returns nothing
native StopCamera                   takes nothing returns nothing
native ResetToGameCamera            takes real duration returns nothing
native PanCameraTo                  takes real x, real y returns nothing
native PanCameraToTimed             takes real x, real y, real duration returns nothing
native PanCameraToWithZ             takes real x, real y, real zOffsetDest returns nothing
native PanCameraToTimedWithZ        takes real x, real y, real zOffsetDest, real duration returns nothing
native SetCinematicCamera           takes string cameraModelFile returns nothing
native SetCameraRotateMode          takes real x, real y, real radiansToSweep, real duration returns nothing
native SetCameraField               takes camerafield whichField, real value, real duration returns nothing

/**
Changes one of the game camera's options whichField by offset over duration seconds.


*/
native AdjustCameraField            takes camerafield whichField, real offset, real duration returns nothing
native SetCameraTargetController    takes unit whichUnit, real xoffset, real yoffset, boolean inheritOrientation returns nothing
native SetCameraOrientController    takes unit whichUnit, real xoffset, real yoffset returns nothing


/**
Creates a new camerasetup object with the following default values.

|                   |               |
|-------------------|---------------|
|Target Coordinates |( 0.00 , 0.00 )|
|Z-Offset           | 0.00          |
|Rotation           | 90.00         |
|Angle of Attack    | 304.00        |
|Distance           | 1650.00       |
|Roll               | 0.00          |
|Field of View      | 70.00         |
|Far Clipping       | 5000.00       |



*/
native CreateCameraSetup                    takes nothing returns camerasetup

/**
Assigns a value to the specified field for a camerasetup. The input angles should be in degrees.

@param whichSetup The camera setup.

@param whichField The field of the camerasetup. 

@param value The value to assign to the field.

@param duration The duration over which the field will be set. If the duration is greater
than 0, the changes will be made gradually once the camera setup is applied.


*/
native CameraSetupSetField                  takes camerasetup whichSetup, camerafield whichField, real value, real duration returns nothing

/**
Returns the value of the specified field for a camerasetup. The angle of attack,
field of view, roll, and rotation are all returned in degrees, unlike `GetCameraField`.

@param whichSetup The camera setup.

@param whichField The field of the camerasetup.


@note The angle of attack, field of view, roll, and rotation are all returned in degrees.

*/
native CameraSetupGetField                  takes camerasetup whichSetup, camerafield whichField returns real

/**
Sets the target coordinates for a camerasetup over a duration. The coordinate
change will only be applied when `CameraSetupApply` (or some other variant) is ran.

@param whichSetup The camera setup.

@param x The target x-coordinate.

@param y The target y-coordinate.

@param duration The coordinates will be applied over this duration once the camera setup is applied.


*/
native CameraSetupSetDestPosition           takes camerasetup whichSetup, real x, real y, real duration returns nothing

/**
Returns the target location of a camerasetup.

@param whichSetup The camera setup.


*/
native CameraSetupGetDestPositionLoc        takes camerasetup whichSetup returns location

/**
Returns the target x-coordinate of a camerasetup.

@param whichSetup The camera setup.


*/
native CameraSetupGetDestPositionX          takes camerasetup whichSetup returns real

/**
Returns the target y-coordinate of a camerasetup.

@param whichSetup The camera setup.


*/
native CameraSetupGetDestPositionY          takes camerasetup whichSetup returns real

/**
Applies the camerasetup, altering the current camera's fields to match those of the camera setup.

@param whichSetup The camerasetup to apply.

@param doPan If set to true, it will move the current camera's target coordinates to the
camera setup's target coordinates. If false, the camera will not move
coordinates, but will still apply the other fields.

@param panTimed If set to true, then it will change the camera's properties over the times specified in CameraSetupSetField.



*/
native CameraSetupApply                     takes camerasetup whichSetup, boolean doPan, boolean panTimed returns nothing

/**
Applies the camerasetup with a custom z-offset, altering the current camera's
fields to match those of the camera setup. The z-offset input will override
the z-offset specified by the camerasetup through `CameraSetupSetField`.

@param whichSetup The camerasetup to apply.

@param zDestOffset The camera's z-offset will gradually change to this value over the specified duration.


@bug If a player pauses the game after the camerasetup has been applied, the
z-offset of the game camera will change to the z-offset of the camerasetup for that player. 

*/
native CameraSetupApplyWithZ                takes camerasetup whichSetup, real zDestOffset returns nothing

/**
Applies the camerasetup over a certain duration, altering the current
camera's fields to match those of the camera setup.

@param whichSetup The camerasetup to apply.

@param doPan If set to true, it will move the current camera's target coordinates to the
camera setup's target coordinates. If false, the camera will not move
coordinates, but will still apply the other fields.

@param forceDuration The duration it will take to apply all the camera fields.
It will ignore the times set by CameraSetupSetField.


*/
native CameraSetupApplyForceDuration        takes camerasetup whichSetup, boolean doPan, real forceDuration returns nothing

/**
Applies the camerasetup over a certain duration with a custom z-offset value,
altering the current camera's fields to match those of the camera setup.
The z-offset input will override the z-offset specified by `CameraSetupSetField`.

@param whichSetup The camerasetup to apply.

@param zDestOffset The camera's z-offset will gradually change to this value over the specified duration.

@param forceDuration The duration it will take to apply all the camera fields.
It will ignore the times set by CameraSetupSetField.


*/
native CameraSetupApplyForceDurationWithZ   takes camerasetup whichSetup, real zDestOffset, real forceDuration returns nothing

/**


@patch 1.32

*/
native BlzCameraSetupSetLabel               takes camerasetup whichSetup, string label returns nothing

/**


@patch 1.32

*/
native BlzCameraSetupGetLabel               takes camerasetup whichSetup returns string


/**
Causes the camera's target to sway(the camera's target, not the camera's perspective).
The higher the magnitude, the higher the range of swaying.
The higher the velocity, the more rapidly the swaying occurs.

@param mag The magnitude of the swaying.

@param velocity The speed of the swaying.



*/
native CameraSetTargetNoise             takes real mag, real velocity returns nothing

/**
Causes the camera's source to sway (the camera's perspective, not the camera's target).
The higher the magnitude, the higher the range of swaying.
The higher the velocity, the more rapidly the swaying occurs.
This will not affect the camera's target coordinates.

@param mag The magnitude of the swaying.

@param velocity The speed of the swaying.


*/
native CameraSetSourceNoise             takes real mag, real velocity returns nothing


/**
Causes the camera's target to sway, just like CameraSetTargetNoise. (the camera's target, not the camera's perspective) The higher the magnitude, the higher the range of swaying. The higher the velocity, the more rapidly the swaying occurs.

Causes the camera's source to sway (the camera's perspective, not the camera's target).
The higher the magnitude, the higher the range of swaying.
The higher the velocity, the more rapidly the swaying occurs.
This will not affect the camera's target coordinates.


@param mag The magnitude of the swaying.

@param velocity The speed of the swaying.

@param vertOnly Stands for "vertical only". If set to true, then the swaying will only modify target distance and z-offset.



*/
native CameraSetTargetNoiseEx           takes real mag, real velocity, boolean vertOnly returns nothing

/**
Causes the camera to sway in the same fashion as `CameraSetSourceNoise`.

@param mag The magnitude of the swaying.

@param velocity The speed of the swaying.

@param vertOnly Stands for "vertical only". If true, then only the angle of attack, target distance, and z-offset of the camera will be modified. (the rotation will not be modified)


*/
native CameraSetSourceNoiseEx           takes real mag, real velocity, boolean vertOnly returns nothing


/**
Sets the game camera's smoothing factor for scrolling with the mouse/keyboard. The default smoothing factor for the standard game camera is 0, where upon scrolling, the camera will immediately come to a stop. As the factor increases, the camera eases into a stop more and more gradually.

@param factor The smoothing factor. It is 0 by default.


*/
native CameraSetSmoothingFactor         takes real factor returns nothing


/**


@patch 1.32

*/
native CameraSetFocalDistance			takes real distance returns nothing

/**


@patch 1.32

*/
native CameraSetDepthOfFieldScale       takes real scale returns nothing

native SetCineFilterTexture             takes string filename returns nothing
native SetCineFilterBlendMode           takes blendmode whichMode returns nothing
native SetCineFilterTexMapFlags         takes texmapflags whichFlags returns nothing
native SetCineFilterStartUV             takes real minu, real minv, real maxu, real maxv returns nothing
native SetCineFilterEndUV               takes real minu, real minv, real maxu, real maxv returns nothing
native SetCineFilterStartColor          takes integer red, integer green, integer blue, integer alpha returns nothing
native SetCineFilterEndColor            takes integer red, integer green, integer blue, integer alpha returns nothing
native SetCineFilterDuration            takes real duration returns nothing
native DisplayCineFilter                takes boolean flag returns nothing
native IsCineFilterDisplayed            takes nothing returns boolean

native SetCinematicScene                takes integer portraitUnitId, playercolor color, string speakerTitle, string text, real sceneDuration, real voiceoverDuration returns nothing
native EndCinematicScene                takes nothing returns nothing
native ForceCinematicSubtitles          takes boolean flag returns nothing

/**


@patch 1.32

*/
native SetCinematicAudio                takes boolean cinematicAudio returns nothing

native GetCameraMargin                  takes integer whichMargin returns real

// These return values for the local players camera only...

/**
Return-value for the local players camera only.


@async 

*/
constant native GetCameraBoundMinX          takes nothing returns real

/**
Return-value for the local players camera only.


@async 

*/
constant native GetCameraBoundMinY          takes nothing returns real

/**
Return-value for the local players camera only.


@async 

*/
constant native GetCameraBoundMaxX          takes nothing returns real

/**
Return-value for the local players camera only.


@async 

*/
constant native GetCameraBoundMaxY          takes nothing returns real

/**
Return-value for the local players camera only.


@async 

*/
constant native GetCameraField              takes camerafield whichField returns real

/**
Return-value for the local players camera only.


@async 

*/
constant native GetCameraTargetPositionX    takes nothing returns real

/**
Return-value for the local players camera only.


@async 

*/
constant native GetCameraTargetPositionY    takes nothing returns real

/**
Return-value for the local players camera only.


@async 

*/
constant native GetCameraTargetPositionZ    takes nothing returns real

/**
Return-value for the local players camera only.


@async 

*/
constant native GetCameraTargetPositionLoc  takes nothing returns location

/**
Return-value for the local players camera only.


@async 

*/
constant native GetCameraEyePositionX       takes nothing returns real

/**
Return-value for the local players camera only.


@async 

*/
constant native GetCameraEyePositionY       takes nothing returns real

/**
Return-value for the local players camera only.


@async 

*/
constant native GetCameraEyePositionZ       takes nothing returns real

/**
Return-value for the local players camera only.


@async 

*/
constant native GetCameraEyePositionLoc     takes nothing returns location

//============================================================================
// Sound API
//
native NewSoundEnvironment          takes string environmentName returns nothing


/**
Creates a sound handle.

@param fileName The path to the file.

@param looping Looping sounds will restart once the sound duration has finished.

@param is3D 3D Sounds can be played on particular areas of the map. They are at
their loudest when the camera is close to the sound's coordinates.

@param fadeInRate How quickly the sound fades in. The higher the number, the
faster the sound fades in. Maximum number is 127.

@param fadeOutRate How quickly the sound fades out. The higher the number, the
faster the sound fades out. Maximum number is 127.

@param eaxSetting EAX is an acronym for environmental audio extensions. In the
sound editor, this corresponds to the "Effect" setting.
The known settings available in Warcraft III are:

| Value              |  Setting               |
|--------------------| ---------------------- |
|`"CombatSoundsEAX"` | combat                 |
|`"KotoDrumsEAX"`    | drums                  | 
|`"SpellsEAX"`       | spells                 |
|`"MissilesEAX"`     | missiles               |
|`"HeroAcksEAX"`     | hero acknowledgements  |
|`"DoodadsEAX"`      | doodads                |
|`"DefaultEAXON"`    | default                |


@note You can only play the same sound handle once.

@note You can only play the same sound filepath four times.

@note Sounds of the same filepath (on different sound handles) must have a delay
of at least 0.1 seconds inbetween them to be played.
You can overcome this by starting one earlier and then using `SetSoundPosition`.

@note You can only play 16 sounds in general.


*/
native CreateSound                  takes string fileName, boolean looping, boolean is3D, boolean stopwhenoutofrange, integer fadeInRate, integer fadeOutRate, string eaxSetting returns sound

/**
Creates a sound but applies default settings to the sound, which are found
under the label from the following SLK-files:

* UI\SoundInfo\AbilitySounds.slk
* UI\SoundInfo\AmbienceSounds.slk
* UI\SoundInfo\AnimSounds.slk
* UI\SoundInfo\DialogSounds.slk
* UI\SoundInfo\UISounds.slk
* UI\SoundInfo\UnitAckSounds.slk
* UI\SoundInfo\UnitCombatSounds.slk

@param fileName The path to the file.

@param looping Looping sounds will restart once the sound duration has finished.

@param is3D 3D Sounds can be played on particular areas of the map. They are at
their loudest when the camera is close to the sound's coordinates.

@param fadeInRate How quickly the sound fades in. The higher the number,
the faster the sound fades in. Maximum number is 127.

@param fadeOutRate How quickly the sound fades out. The higher the number,
the faster the sound fades out. Maximum number is 127.

@param SLKEntryName the label out of one of the SLK-files, whose settings should be
used, e.g. values like volume, pitch, pitch variance, priority, channel, min distance, max distance, distance cutoff or eax.


@note You can only play the same sound handle once.

@note You can only play the same sound filepath four times.

@note Sounds of the same filepath (on different sound handles) must have a delay
of at least 0.1 seconds inbetween them to be played.
You can overcome this by starting one earlier and then using `SetSoundPosition`.

@note You can only play 16 sounds in general.


*/
native CreateSoundFilenameWithLabel takes string fileName, boolean looping, boolean is3D, boolean stopwhenoutofrange, integer fadeInRate, integer fadeOutRate, string SLKEntryName returns sound
native CreateSoundFromLabel         takes string soundLabel, boolean looping, boolean is3D, boolean stopwhenoutofrange, integer fadeInRate, integer fadeOutRate returns sound
native CreateMIDISound              takes string soundLabel, integer fadeInRate, integer fadeOutRate returns sound


/**
Applies default settings to the sound, which are found under the label from the following SLK-files:

* UI\SoundInfo\AbilitySounds.slk
* UI\SoundInfo\AmbienceSounds.slk
* UI\SoundInfo\AnimSounds.slk
* UI\SoundInfo\DialogSounds.slk
* UI\SoundInfo\UISounds.slk
* UI\SoundInfo\UnitAckSounds.slk
* UI\SoundInfo\UnitCombatSounds.slk

@param soundHandle The sound to configure.
@param soundLabel the label out of one of the SLK-files, whose settings should be
used, e.g. values like volume, pitch, pitch variance, priority, channel, min distance, max distance, distance cutoff or eax.


*/
native SetSoundParamsFromLabel      takes sound soundHandle, string soundLabel returns nothing
native SetSoundDistanceCutoff       takes sound soundHandle, real cutoff returns nothing
native SetSoundChannel              takes sound soundHandle, integer channel returns nothing

/**
Sets the sounds volume.

@param soundHandle which sound.

@param volume Volume, between 0 and 127.


*/
native SetSoundVolume               takes sound soundHandle, integer volume returns nothing

/**
Tones the pitch of the sound, default value is 1. Increasing it you get the chipmunk
version and the sound becomes shorter, when decremented the sound becomes low-pitched and longer.


@bug This native has very weird behaviour.
See [this](http://www.hiveworkshop.com/threads/setsoundpitch-weirdness.215743/#post-2145419) for an explanation
and [this](http://www.hiveworkshop.com/threads/snippet-rapidsound.258991/#post-2611724) for a non-bugged implementation.

*/
native SetSoundPitch                takes sound soundHandle, real pitch returns nothing

// the following method must be called immediately after calling "StartSound"

/**


@note Must be called immediately after calling `StartSound`.

*/
native SetSoundPlayPosition         takes sound soundHandle, integer millisecs returns nothing

// these calls are only valid if the sound was created with 3d enabled

/**


@note This call is only valid if the sound was created with 3d enabled.

*/
native SetSoundDistances            takes sound soundHandle, real minDist, real maxDist returns nothing

/**


@note This call is only valid if the sound was created with 3d enabled.

*/
native SetSoundConeAngles           takes sound soundHandle, real inside, real outside, integer outsideVolume returns nothing

/**


@note This call is only valid if the sound was created with 3d enabled.

*/
native SetSoundConeOrientation      takes sound soundHandle, real x, real y, real z returns nothing

/**


@note This call is only valid if the sound was created with 3d enabled.

*/
native SetSoundPosition             takes sound soundHandle, real x, real y, real z returns nothing

/**


@note This call is only valid if the sound was created with 3d enabled.

*/
native SetSoundVelocity             takes sound soundHandle, real x, real y, real z returns nothing

/**
Attaches the sound soundHandle to unit whichUnit. Attaching sound to unit means
that the more far away the player stays from the unit to which the sound is attached, the less
loud the sound plays (the volume of the attached sound decreases with increasing distance).

@param soundHandle The 3D sound to play.
@param whichUnit The unit to attach the sound to.

@note This call is only valid if the sound was created with 3d enabled.


*/
native AttachSoundToUnit            takes sound soundHandle, unit whichUnit returns nothing


/**
Starts the sound.


@note You can only play the same sound handle once.

@note You can only play 16 sounds in general.

@note Sounds of the same filepath (on different sound handles) must have a delay
of at least 0.1 seconds inbetween them to be played.
You can overcome this by starting one earlier and then using `SetSoundPosition`.

*/
native StartSound                   takes sound soundHandle returns nothing

/**
Starts playing a sound. 


@note An officially exported native in: 1.33.0 (checked v1.33.0.18897 PTR).
Unofficially available in: 1.32 (not declared a native, but visible in Lua).

@bug The `fadeIn` parameter does nothing (unused); thus equivalent to `StartSound`.

@note The only difference to StartSound is the optional fadeIn (boolean).
@patch 1.33

*/
native StartSoundEx                 takes sound soundHandle, boolean fadeIn returns nothing

/**
Stops the sound.

@param soundHandle The sound to stop.
@param killWhenDone The sound gets destroyed if true.
@param fadeOut turns down the volume with `fadeOutRate` as stated in constructor.


*/
native StopSound                    takes sound soundHandle, boolean killWhenDone, boolean fadeOut returns nothing

/**
Destroys the handle when the sound has finished playing.


*/
native KillSoundWhenDone            takes sound soundHandle returns nothing

// Music Interface. Note that if music is disabled, these calls do nothing

/**


@note If music is disabled, these calls do nothing.
@note If musicName is a semicolon-delimited list, the whole list of music is played. The index and random parameters seem to be with respect to the list.

*/
native SetMapMusic                  takes string musicName, boolean random, integer index returns nothing

/**
Clears the map music applied via `SetMapMusic`.


*/
native ClearMapMusic                takes nothing returns nothing


/**
Sets the file as the current music for the map, and plays it.

@param musicName The path to the music file.

@note Music is on its own channel and can be toggled on and off within the Warcraft III game menu.
@bug This native may cause a short lag spike as soon as the music starts. To circumvent this lag, stop the current music without fadeout before calling this function (`call StopMusic(false)`).
@note Should work with mp3s, midis and wavs.
@note If musicName is a semicolon-delimited list, the whole list of music is played.

*/
native PlayMusic                    takes string musicName returns nothing

/**
Sets the file as the current music for the map, and plays it.

@param musicName The path to the music file.
@param frommsecs At what offset the music starts. In milliseconds.
@param fadeinmsecs How long the music is faded in. In milliseconds.

@note Music is on its own channel and can be toggled on and off within the Warcraft III game menu.
@bug This native may cause a short lag spike as soon as the music starts. To circumvent this lag, stop the current music without fadeout before calling this function (`call StopMusic(false)`).
@note Should work with mp3s, midis and wavs.

*/
native PlayMusicEx                  takes string musicName, integer frommsecs, integer fadeinmsecs returns nothing

/**
Stops the current music.


*/
native StopMusic                    takes boolean fadeOut returns nothing

/**
Resumes music.


*/
native ResumeMusic                  takes nothing returns nothing


/**
The thematic music does not play repeatedly, but interrupts the PlayMusic-music.

@param musicFileName The path to the music file.

@note Only one thematic music at a time, cancels the previous one.
@note Probably meant for boss fights and similar where the sound should go in foreground.


*/
native PlayThematicMusic            takes string musicFileName returns nothing

/**
The thematic music does not play repeatedly, but interrupts the PlayMusic-music.

@param musicFileName The path to the music file.
@param frommsecs At what offset the music starts. In milliseconds.

@note Only one thematic music at a time, cancels the previous one.
@note Probably meant for boss fights and similar where the sound should go in foreground.


*/
native PlayThematicMusicEx          takes string musicFileName, integer frommsecs returns nothing

/**
Stops thematic music.


*/
native EndThematicMusic             takes nothing returns nothing


/**
Sets the music volume.

@param volume Volume between 0 and 127.


*/
native SetMusicVolume               takes integer volume returns nothing
native SetMusicPlayPosition         takes integer millisecs returns nothing

/**


@patch 1.32.2

*/
native SetThematicMusicVolume       takes integer volume returns nothing
native SetThematicMusicPlayPosition takes integer millisecs returns nothing

// other music and sound calls
native SetSoundDuration             takes sound soundHandle, integer duration returns nothing

/**
Returns sound length in milliseconds.


@note Beweare that this might return different values for different players
if you use native wc3-sounds as these can have different length in different languages.
This can cause desyncs if you use the duration for non-local stuff.

@async 

*/
native GetSoundDuration             takes sound soundHandle returns integer

/**
Returns length of the sound file under the path in milliseconds.


@note Beweare that this might return different values for different players
if you use native wc3-sounds as these can have different length in different languages.
This can cause desyncs if you use the duration for non-local stuff.

@async 

*/
native GetSoundFileDuration         takes string musicFileName returns integer

native VolumeGroupSetVolume         takes volumegroup vgroup, real scale returns nothing
native VolumeGroupReset             takes nothing returns nothing


/**


@note If you just started the sound this still returns false.

@async 

*/
native GetSoundIsPlaying            takes sound soundHandle returns boolean
native GetSoundIsLoading            takes sound soundHandle returns boolean

native RegisterStackedSound         takes sound soundHandle, boolean byPosition, real rectwidth, real rectheight returns nothing
native UnregisterStackedSound       takes sound soundHandle, boolean byPosition, real rectwidth, real rectheight returns nothing


/**


@patch 1.32

*/
native SetSoundFacialAnimationLabel takes sound soundHandle, string animationLabel returns boolean

/**


@patch 1.32

*/
native SetSoundFacialAnimationGroupLabel takes sound soundHandle, string groupLabel returns boolean

/**


@patch 1.32

*/
native SetSoundFacialAnimationSetFilepath takes sound soundHandle, string animationSetFilepath returns boolean

//Subtitle support that is attached to the soundHandle rather than as disperate data with the legacy UI

/**


@patch 1.32

*/
native SetDialogueSpeakerNameKey    takes sound soundHandle, string speakerName returns boolean

/**


@patch 1.32

*/
native GetDialogueSpeakerNameKey    takes sound soundHandle returns string

/**


@patch 1.32

*/
native SetDialogueTextKey           takes sound soundHandle, string dialogueText returns boolean

/**


@patch 1.32

*/
native GetDialogueTextKey           takes sound soundHandle returns string

//============================================================================
// Effects API
//

/**
Creates a weather effect that is spatially limited to the specified area.

This creates a new object and returns its handle, to prevent leaks it must be destroyed
with `RemoveWeatherEffect` when no longer needed.

The weather effect is created initially disabled and must be turned on with `EnableWeatherEffect`.

**Example (Lua):** to create an "Ashenvale Heavy Rain" at map center:

```{.lua}
center = Rect(-1024, -1024, 1024, 1024)
weather = AddWeatherEffect(center, FourCC("RAhr"))
EnableWeatherEffect(weather, true)
```

@param where The rect where the weather will be visible.

@param effectID (Rawcode) Which weather preset to apply.


@note To understand more about weather effects nature, I advise to read
Ammorth's article about weather effects: <http://www.wc3c.net/showthread.php?t=91176>.

@note To get an idea on how to add your own weather effects, you may read
CryoniC's article about custom weather effects: <http://www.wc3c.net/showthread.php?t=67949>.

@note The weather effects are defined in `terrainart/weather.slk` in game files.
The [current default list is here](https://www.hiveworkshop.com/threads/how-to-create-random-weather.198658/post-1953519) (v1.32.10).

*/
native AddWeatherEffect             takes rect where, integer effectID returns weathereffect

/**
Removes the weather effect (visually instant) and frees the handle.


@note See: `AddWeatherEffect`, `EnableWeatherEffect`.

*/
native RemoveWeatherEffect          takes weathereffect whichEffect returns nothing

/**
Smoothly enables/disables the given weather effect.

@param whichEffect A handle of target weather effect.
@param enable `true` to enable, `false` to disable the effect.


@note See: `AddWeatherEffect`, `RemoveWeatherEffect`.

*/
native EnableWeatherEffect          takes weathereffect whichEffect, boolean enable returns nothing


/**
Create a crater at the given coordinates.

@param x The x coordinate of the craters center.
@param y The y coordinate of the craters center.
@param radius The radius of the crater.
@param depth The depth of the crater.
@param duration The duration in milliseconds.
@param permanent Make the deformation permanent.

@note To approximate the resulting height of a point `distance` units away from the
center point `(x, y)` you can use the following formula: `Cos(bj_PI/2 * distance / radius) * -depth`. See this [issue](https://github.com/lep/jassdoc/issues/31) for some more information.

@note Not every player might display those transformations due to graphics
settings. Thus reading data like terrain height might lead to async values.
See the other note on a way to compute an appropiate height to use instead.

@note Permanent terrain deformations are not present in saved game files.


*/
native TerrainDeformCrater          takes real x, real y, real radius, real depth, integer duration, boolean permanent returns terraindeformation

/**

@param duration The duration in milliseconds.


@note Permanent terrain deformations are not present in saved game files.

*/
native TerrainDeformRipple          takes real x, real y, real radius, real depth, integer duration, integer count, real spaceWaves, real timeWaves, real radiusStartPct, boolean limitNeg returns terraindeformation

/**


@note Permanent terrain deformations are not present in saved game files.

*/
native TerrainDeformWave            takes real x, real y, real dirX, real dirY, real distance, real speed, real radius, real depth, integer trailTime, integer count returns terraindeformation

/**

@param duration The duration in milliseconds.


@note Permanent terrain deformations are not present in saved game files.

*/
native TerrainDeformRandom          takes real x, real y, real radius, real minDelta, real maxDelta, integer duration, integer updateInterval returns terraindeformation
native TerrainDeformStop            takes terraindeformation deformation, integer duration returns nothing
native TerrainDeformStopAll         takes nothing returns nothing


/**
Creates the special effect in point with coordinates (x;y) using the model file with a path modelName.
The altitude (Z) of the newly spawned effect is at the ground level, be it terrain, some pathable destructable or on top of water.
In other words, the effect's Z coordinate does not have to be 0.


@note To create an effect with an offset in relation to the ground before 1.30 patch, see <http://www.hiveworkshop.com/forums/1561722-post10.html>
@note In case of 1.30 patch or higher, use `BlzSetSpecialEffectZ` native.

@note To create an effect only visible to one player see <https://www.hiveworkshop.com/threads/gs.300430/#post-3209073>

*/
native AddSpecialEffect             takes string modelName, real x, real y returns effect

/**
Creates the special effect in the stated location using the model file with a path modelName.
The altitude (Z) of the newly spawned effect is at the ground level, be it terrain, some pathable destructable or on top of water.
In other words, the effect's Z coordinate does not have to be 0.


@note To create an effect with a z-position not zero see <http://www.hiveworkshop.com/forums/1561722-post10.html>.

@note To create an effect only visible to one player see <https://www.hiveworkshop.com/threads/gs.300430/#post-3209073>.

*/
native AddSpecialEffectLoc          takes string modelName, location where returns effect

/**
Attaches the special effect to the attachment point attachPointName of the
target widget, using the model file with a path modelName.

Upon creation, the effect will play its "birth" animation followed by its "stand" animation (once the birth animation has finished). If the model does not have animations, it will show up the way it appears by default. The effect will last indefinitely unless it is destroyed, even if the model seems to disappear. To destroy an effect, see DestroyEffect.


@param modelName The path of the model. Use double backslashes when specifying
a directory, rather than single backslashes. See AddSpecialEffect for an example.

@param targetWidget The widget to attach the effect to.

@param attachPointName The attachment point of the widget where the effect will
be placed. Attachment points are points in a model that can be referenced to as
areas for effects to be attached, whether it be from a spell or this function.
A list of common attachment points in in-game Warcraft 3 models can be seen below.
If the attachment point does not exist, it will attach the effect to the model's origin.


@note Strings such as "Large" and "Medium" affect effects' sizes on the widget
it is attached to. You can add or remove these by going to the object editor and
modifying "Art - Required Animation Names - Attachments" for a particular unit
you are attaching effects to. 

@note To create an effect only visible to one player see <https://www.hiveworkshop.com/threads/gs.300430/#post-3209073>.


*/
native AddSpecialEffectTarget       takes string modelName, widget targetWidget, string attachPointName returns effect
native DestroyEffect                takes effect whichEffect returns nothing


/**


@note No one knows what abilityString is supposed to be.
@bug Does nothing.

*/
native AddSpellEffect               takes string abilityString, effecttype t, real x, real y returns effect

/**


@note No one knows what abilityString is supposed to be.
@bug Does nothing.

*/
native AddSpellEffectLoc            takes string abilityString, effecttype t,location where returns effect

/**
Creates the special effect in point with coordinates (x;y) with Z = 0 using the
model file from the Object Editor field of type t from the ability, unit or
buff (works with all these types, though the name states it's ability-only
function) with raw code abilityId. If this field has more than one effect
inside, it will only create the first effect stated in the field, ignoring
all others.


@note To create an effect with a z-position not zero see <http://www.hiveworkshop.com/forums/1561722-post10.html>.

*/
native AddSpellEffectById           takes integer abilityId, effecttype t,real x, real y returns effect

/**
Creates the special effect in location where with Z = 0 using the model file
from the Object Editor field of type t from the ability, unit or buff (works
with all these types, though the name states it's ability-only function) with
raw code abilityId. If this field has more than one effect inside, it will only
create the first effect stated in the field, ignoring all others.


@note To create an effect with a z-position not zero see <http://www.hiveworkshop.com/forums/1561722-post10.html>.

*/
native AddSpellEffectByIdLoc        takes integer abilityId, effecttype t,location where returns effect
native AddSpellEffectTarget         takes string modelName, effecttype t, widget targetWidget, string attachPoint returns effect

/**
Attaches the special effect to the attachment point attachPointName of the
target widget, using the model file from the Object Editor field of type t from
the ability, unit or buff (works with all these types, though the name states
it's ability-only function) with raw code abilityId. If this field has more than
one effect inside, it will only create the first effect stated in the field,
ignoring all others.


*/
native AddSpellEffectTargetById     takes integer abilityId, effecttype t, widget targetWidget, string attachPoint returns effect


/**
Creates a lightning between two points.

@param codeName 4 letter id from the LightningData.slk.
@param checkVisibility If this is true, the lightning won't be created and the function will return null unless the local player
currently has as visibility of at least one of the endpoints of the to be created lightning.
@param x1 x-coordinate (World) of source point.
@param y1 y-coordinate (World) of source point.
@param x2 x-coordinate (World) of target point.
@param y2 y-coordinate (World) of target point.


@note The source z value of the new lightning is set to match the current terrain height of the source point, analogously, the target z value
matches the current terrain height of the target point. Later changes to the terrain height do not affect herewith created existing lightnings anymore.

*/
native AddLightning                 takes string codeName, boolean checkVisibility, real x1, real y1, real x2, real y2 returns lightning

/**
Creates a lightning between two points.

@param codeName 4 letter id from the LightningData.slk.
@param checkVisibility If this is true, the lightning won't be created and the function will return null unless the local player
currently has visibility of at least one of the endpoints of the to be created lightning.
@param x1 x-coordinate (World) of source point.
@param y1 y-coordinate (World) of source point.
@param z1 z-coordinate (World) of source point.
@param x2 x-coordinate (World) of target point.
@param y2 y-coordinate (World) of target point.
@param z2 z-coordinate (World) of target point.


*/
native AddLightningEx               takes string codeName, boolean checkVisibility, real x1, real y1, real z1, real x2, real y2, real z2 returns lightning

/**
Destroys a lightning.

@param whichBolt The lightning to be destroyed.


*/
native DestroyLightning             takes lightning whichBolt returns boolean

/**
Moves a lightning.

@param whichBolt The lightning to be moved.
@param checkVisibility If this is true, the lightning won't be moved (at all) unless the local player
currently has visibility of at least one of the new endpoints.
@param x1 x-coordinate (World) of the new source point.
@param y1 y-coordinate (World) of the new source point.
@param x2 x-coordinate (World) of the new target point.
@param y2 y-coordinate (World) of the new target point.


*/
native MoveLightning                takes lightning whichBolt, boolean checkVisibility, real x1, real y1, real x2, real y2 returns boolean

/**
Moves a lightning.

@param whichBolt The lightning to be moved.
@param checkVisibility If this is true, the lightning won't be moved (at all) unless the local player
currently has visibility of at least one of the new endpoints.
@param x1 x-coordinate (World) of the new source point.
@param y1 y-coordinate (World) of the new source point.
@param z1 z-coordinate (World) of the new source point.
@param x2 x-coordinate (World) of the new target point.
@param y2 y-coordinate (World) of the new target point.
@param z2 z-coordinate (World) of the new target point.


*/
native MoveLightningEx              takes lightning whichBolt, boolean checkVisibility, real x1, real y1, real z1, real x2, real y2, real z2 returns boolean

/**
Gets the alpha value of a lightning.


*/
native GetLightningColorA           takes lightning whichBolt returns real

/**
Gets the red color value of a lightning.


*/
native GetLightningColorR           takes lightning whichBolt returns real

/**
Gets the green color value of a lightning.


*/
native GetLightningColorG           takes lightning whichBolt returns real

/**
Gets the blue color value of a lightning.


*/
native GetLightningColorB           takes lightning whichBolt returns real

/**
Sets the coloring of a lightning.

@param whichBolt The lightning to be colored.
@param r 0-1 visibility of red channel (value mod 1)
@param g 0-1 visibility of green channel (value mod 1)
@param b 0-1 visibility of blue channel (value mod 1)
@param a 0-1 alpha value/overall visibility multiplier (value mod 1)


@note The default is 1, 1, 1, 1.

@bug This native is inaccurate. The modulo is not exactly 1 and even setting a color value to e.g. 0.1 yields 0.098.

*/
native SetLightningColor            takes lightning whichBolt, real r, real g, real b, real a returns boolean


/**


@note No one knows what abilityString is supposed to be.
@bug Does nothing.
@pure 

*/
native GetAbilityEffect             takes string abilityString, effecttype t, integer index returns string

/**


@pure 

*/
native GetAbilityEffectById         takes integer abilityId, effecttype t, integer index returns string

/**


@note No one knows what abilityString is supposed to be.
@bug Does nothing.
@pure 

*/
native GetAbilitySound              takes string abilityString, soundtype t returns string

/**


@pure 

*/
native GetAbilitySoundById          takes integer abilityId, soundtype t returns string

//============================================================================
// Terrain API
//

/**
Gets the cliff level at a point.

@param x x-coordinate (World) of the point.
@param y y-coordinate (World) of the point.


@note Walkable destructables add their effective cliff level to the terrain across their pathing maps, i.e., if the terrain at some point
without destructables has a cliff height of a and the destructable covering that point has an effective cliff height of b, this function returns a + b. If there
are multiple walkable destructables intersecting at the requested point, the function returns a + max(b1, b2, ...). If the declared cliff height of a destructable
is negative, it will have an effective cliff height of 0.

*/
native GetTerrainCliffLevel         takes real x, real y returns integer

/**
Sets the tint of the water.

@param red 0-255 red color (value mod 256).
@param green 0-255 green color (value mod 256).
@param blue 0-255 blue color (value mod 256).
@param alpha 0-255 opaqueness (value mod 256).


@note The default is 255 for all parameters.

@bug In HD, the alpha setting seems to only affect the water surface.

*/
native SetWaterBaseColor            takes integer red, integer green, integer blue, integer alpha returns nothing

/**
Sets whether terrain deformations also affect the water mesh on top of it.

@param val If this is true, terrain deformations will affect the water mesh.


@note This is only during transitions of terrain deformations, i.e., for temporary terrain deformations and during the transition
of permanent terrain deformations. After a permanent terrain deformation has completed, the water deformation will revert instantly.

@note The default is false.

*/
native SetWaterDeforms              takes boolean val returns nothing
native GetTerrainType               takes real x, real y returns integer
native GetTerrainVariance           takes real x, real y returns integer
native SetTerrainType               takes real x, real y, integer terrainType, integer variation, integer area, integer shape returns nothing

/**
Returns if a specific pathingtype is set at the location.

@note Returns true if the pathingtype is *not* set, false if it *is* set.

*/
native IsTerrainPathable            takes real x, real y, pathingtype t returns boolean
native SetTerrainPathable           takes real x, real y, pathingtype t, boolean flag returns nothing

//============================================================================
// Image API
//

/**
This returns a new image, the first ID given being 0 and then counting upwards (0, 1, 2, 3, ...).

@param file The path to the image. The image itself should have its border alpha-ed out
completely. If an invalid path is specified CreateImage returns image(-1).

@param sizeX The x-dimensions of the image.

@param sizeY The y-dimensions of the image.

@param sizeZ The z-dimensions of the image.

@param posX The x-cooridnate of where to create the image. This is the bottom left corner of the image.

@param posY The y-cooridnate of where to create the image. This is the bottom left corner of the image.

@param posZ The z-cooridnate of where to create the image.

@param originX Moves the origin (bottom left corner) of the image from posX in negative X-direction.

@param originY Moves the origin (bottom left corner) of the image from posY in negative Y-direction.

@param originZ Moves the origin (bottom left corner) of the image from posZ in negative Z-direction.

@param imageType Working values range from 1 to 4 (4 and 1 included).
Using 0 causes CreateImage to return image(-1). Every other value will simply
cause WC3 to not display the image.
imageTypes also influence the order in which images are drawn above one another:

| Value | Name           | Description |
|-------|----------------|-------------|
| 1     | Selection      | Drawn above all other imageTypes. |
| 2     | Indicator      | Drawn above imageType 4, but below 1 and 3. |
| 3     | Occlusion Mask | Drawn above imageType 4 and 2 and below imageType 1. |
| 4     | Ubersplat      | Drawn below every other type. Images of this type are additionally affected by time of day and the fog of war (only for tinting). |



Multiple images with the same type are drawn in their order of creation,
meaning that the image created first is drawn below the image created after.


*/
native CreateImage                  takes string file, real sizeX, real sizeY, real sizeZ, real posX, real posY, real posZ, real originX, real originY, real originZ, integer imageType returns image

/**
This function destroys the image specified and recycles the handle ID of that
image instantly (no ref counting for images).

@param whichImage Which image to destroy.


@bug May crash the game if an invalid image is used (null, before the first image is created).

*/
native DestroyImage                 takes image whichImage returns nothing

/**
It shows/hides image whichImage, depending on boolean flag (true shows, false hides).
Seems like a redundant function in the light of SetImageRender(Always).


*/
native ShowImage                    takes image whichImage, boolean flag returns nothing

/**
Untested, but if its decription can account for anthing, it locks the Z position
to the given height, if the flag is true. After a bit of testing i concluded
that this is the only function thats able to modify an images Z offset.


*/
native SetImageConstantHeight       takes image whichImage, boolean flag, real height returns nothing

/**
Sets the X/Y position of the provided image.
This is the bottom left corner of the image, unless you used values
form originX/Y/Z in the constructor other than 0, in which case the bottom
left corner is moved further into negative X/Y/Z direction.


*/
native SetImagePosition             takes image whichImage, real x, real y, real z returns nothing

/**
Valid values for all channels range from 0 to 255.


*/
native SetImageColor                takes image whichImage, integer red, integer green, integer blue, integer alpha returns nothing

/**


@bug Does not work. Use `SetImageRenderAlways` instead.

*/
native SetImageRender               takes image whichImage, boolean flag returns nothing

/**
Since `SetImageRender` is non-functional, this should be used to
enable/disable rendering of the image.


*/
native SetImageRenderAlways         takes image whichImage, boolean flag returns nothing

/**
Draws the specified image above the water if the flag is true. The second
boolean (useWaterAlpha) doesnt seem to do much. Every imagetype other than 1
doesnt seem to appear above water.


*/
native SetImageAboveWater           takes image whichImage, boolean flag, boolean useWaterAlpha returns nothing

/**
Changes the specified images type.

@param imageType Influence the order in which images are drawn above one another:

| Value | Name           | Description |
|-------|----------------|-------------|
| 1     | Selection      | Drawn above all other imageTypes. |
| 2     | Indicator      | Drawn above imageType 4, but below 1 and 3. |
| 3     | Occlusion Mask | Drawn above imageType 4 and 2 and below imageType 1. |
| 4     | Ubersplat      | Drawn below every other type. Images of this type are additionally affected by time of day and the fog of war (only for tinting). |

Multiple images with the same type are drawn in their order of creation,
meaning that the image created first is drawn below the image created after.


*/
native SetImageType                 takes image whichImage, integer imageType returns nothing

//============================================================================
// Ubersplat API
//
native CreateUbersplat              takes real x, real y, string name, integer red, integer green, integer blue, integer alpha, boolean forcePaused, boolean noBirthTime returns ubersplat
native DestroyUbersplat             takes ubersplat whichSplat returns nothing

/**


@bug Does nothing.

*/
native ResetUbersplat               takes ubersplat whichSplat returns nothing

/**


@bug Does nothing.

*/
native FinishUbersplat              takes ubersplat whichSplat returns nothing
native ShowUbersplat                takes ubersplat whichSplat, boolean flag returns nothing
native SetUbersplatRender           takes ubersplat whichSplat, boolean flag returns nothing
native SetUbersplatRenderAlways     takes ubersplat whichSplat, boolean flag returns nothing

//============================================================================
// Blight API
//
native SetBlight                takes player whichPlayer, real x, real y, real radius, boolean addBlight returns nothing
native SetBlightRect            takes player whichPlayer, rect r, boolean addBlight returns nothing
native SetBlightPoint           takes player whichPlayer, real x, real y, boolean addBlight returns nothing
native SetBlightLoc             takes player whichPlayer, location whichLocation, real radius, boolean addBlight returns nothing

/**
Creates a new, undead blighted gold mine unit at the specified coordinates for the player. The haunted gold mine will create blight around the area, and will become a normal gold mine when destroyed. The amount of gold in the mine is determined by the Data - Max Gold field for the ability Gold Mine ability ('Agld').

@param id The player to create the goldmine for.

@param x The x-coordinate of the goldmine.

@param y The y-coordinate of the goldmine.

@param face The facing of the goldmine in degrees.


*/
native CreateBlightedGoldmine   takes player id, real x, real y, real face returns unit
native IsPointBlighted          takes real x, real y returns boolean

//============================================================================
// Doodad API
//

/**
Makes doodads in the vicinity of a point play an animation.

@param x x-coordinate (world units) of the point.
@param y y-coordinate (world units) of the point.
@param radius Maximum pick distance from the point.
@param doodadID The type of the doodad that should be affected.
@param nearestOnly If true, only the single doodad (of the given type) closest to the point will be affected, otherwise all in the vicinity (of the given type).
@param animName String identifier of the animation that should be played.
@param animRandom If true, the animation to be played will be picked from an extended set including different variations of the animName, e.g., if animName is "walk", it can also be "walk defend".


@note Only doodads whose origin is within the radius distance of the point are considered.

@note There are the special values "hide" and "show" for animName, which will hide respectively show the doodad. When a doodad is hidden this way, its animation will pause at the current time frame. Re-showing the doodad resumes the animation.

@note If a target does not have an animation identified by animName (and it's not one of the special animation names either), it will play its first declared animation instead.

@bug If animName is null and there is at least one target, the game will crash.

@note If animRandom is true and the picked animation is looped, it will freshly re-pick from the set when an animation ends.

*/
native SetDoodadAnimation       takes real x, real y, real radius, integer doodadID, boolean nearestOnly, string animName, boolean animRandom returns nothing

/**
Makes doodads within a rect play an animation.

@param r The rect wherein doodads should play an animation.


@note Only doodads whose origin is in the rect are considered targets.

@note The animation won't play for an observer until they have sight visibility of the doodad, at which point it will play.

See `SetDoodadAnimation` for other parameters and notes.

*/
native SetDoodadAnimationRect   takes rect r, integer doodadID, string animName, boolean animRandom returns nothing

//============================================================================
// Computer AI interface
//
native StartMeleeAI         takes player num, string script                 returns nothing
native StartCampaignAI      takes player num, string script                 returns nothing
native CommandAI            takes player num, integer command, integer data returns nothing
native PauseCompAI          takes player p,   boolean pause                 returns nothing
native GetAIDifficulty      takes player num                                returns aidifficulty

native RemoveGuardPosition  takes unit hUnit                                returns nothing
native RecycleGuardPosition takes unit hUnit                                returns nothing
native RemoveAllGuardPositions takes player num                             returns nothing

//============================================================================

/**
Applies the specified cheat, but only if the game is single player. There are a
few cheats that can be toggled on or off. If the cheat is enabled, entering the
cheat again will disable it. If the cheat is disabled, entering the cheat will
enable it again. Upon entering, the text "Cheat Enabled!" will be displayed.

@param cheatStr The cheat to enter. 


@note For a list of all cheats see <http://classic.battle.net/war3/cheatcodes.shtml>.

*/
native Cheat            takes string cheatStr returns nothing

/**
Returns true if "ItVexesMe" aka "no victory" cheat is enabled.


*/
native IsNoVictoryCheat takes nothing returns boolean

/**
Returns true if "StrengthAndHonor" aka "no defeat" cheat is enabled.


*/
native IsNoDefeatCheat  takes nothing returns boolean


/**
It does two things:

1. Try to read the file, if "Allow Local Files" is enabled then also searches in the game folder
2. Append filename to preload buffer

@param filename Text string, supposed to be a file path to be preloaded. Max length: 259 characters (see Windows MAX_PATH).


@note The game only reads these files, does not load them. The reading is done in a separate thread and does not freeze the game. One file is not read twice, no matter how often you call Preload().

@note Trick: It does not escape double-quotes " on purpose (approved not a bug, it's a feature).
It is possible to inject custom code in Preload files this way (Lua):

```{.lua}
PreloadGenClear()
PreloadGenStart()
Preload(' ")\ncall otherFunction("123")\n//')
PreloadGenEnd("its-a-feature.txt")
```
	
Results in the following preload file code (Jass):

    function PreloadFiles takes nothing returns nothing
     
            call PreloadStart()
            call Preload( " ")
    call otherFunction("123")
    //" )
            call PreloadEnd( 754.6 )
     
    endfunction


@note **Game folder:**
Reforged: `Warcraft III\_retail_\somefile.txt`, instead of `_retail_` there's also a `_ptr_` game version currently.
Classic: ?

@note **Mini tutorial:**

**What are Preload files?**

Preload files instruct the game to pre-read a file/resources to avoid freezes/stutter during gameplay. It's done to move the file into OS cache. Blizzard used preload files to load all required files at map init. See blizzard.j or campaign maps.

Create a preload file (Lua)

```{.lua}
PreloadGenClear()
PreloadGenStart()
-- call Preload("filename.ext") as often as you need, one call per file you add
Preload("Textures\\Knight.blp")
PreloadGenEnd("MyPreloadFile.txt")
```

**How to run a preload file**

This must be done manually:

```{.lua}
Preloader("MyPreloadFile.txt")
```
	
**Lua code in preload files?**

It is possible although in a very hacky way, [described here](https://www.hiveworkshop.com/threads/blizzards-hidden-jass2lua-transpiler.337281/).
You need to use "//! beginusercode" to start a section containing Lua code and end it using "//! endusercode".
It works because the code is compiled on the fly with Jass2Lua.


@note See: `PreloadEnd`, `PreloadStart`, `PreloadRefresh`, `PreloadEndEx`, `PreloadGenClear`, `PreloadGenStart`, `PreloadGenEnd`, `Preloader`.
@note Also see the documentation for `Preloader` for more info on the generated files.

*/
native Preload          takes string filename returns nothing

/**
Unknown. It's always generated at the end of a preload file, timeout represents the time between calls to `PreloadStart` and `PreloadGenEnd`.


@note See: `Preload`, `PreloadStart`, `PreloadRefresh`, `PreloadEndEx`, `PreloadGenClear`, `PreloadGenStart`, `PreloadGenEnd`, `Preloader`.

*/
native PreloadEnd       takes real timeout returns nothing


/**
Clears the preload buffer and starts the timer. (Anything else?)


@note See: `Preload`, `PreloadEnd`, `PreloadRefresh`, `PreloadEndEx`, `PreloadGenClear`, `PreloadGenStart`, `PreloadGenEnd`, `Preloader`.

*/
native PreloadStart     takes nothing returns nothing

/**
Unknown. It does not reset the timer or clear the buffer.


@note See: `Preload`, `PreloadEnd`, `PreloadStart`, `PreloadEndEx`, `PreloadGenClear`, `PreloadGenStart`, `PreloadGenEnd`, `Preloader`.

*/
native PreloadRefresh   takes nothing returns nothing

/**
Unknown


@note See: `Preload`, `PreloadEnd`, `PreloadStart`, `PreloadRefresh`, `PreloadGenClear`, `PreloadGenStart`, `PreloadGenEnd`, `Preloader`.

*/
native PreloadEndEx     takes nothing returns nothing


/**
Clears all added file paths from the current preload buffer. Does not reset the timer.


@note See: `Preload`, `PreloadEnd`, `PreloadStart`, `PreloadRefresh`, `PreloadEndEx`, `PreloadGenStart`, `PreloadGenEnd`, `Preloader`.

*/
native PreloadGenClear  takes nothing returns nothing

/**
Starts an internal timer for preloads. The timer will be used and recorded by `PreloadGenEnd`. The timer represents the wall clock time (in seconds) spent between the calls `PreloadStart()` and `PreloadGenEnd()`.

This function does not clear the previous buffer.

The recorded time will be output as `call PreloadEnd( 0.123 )` in the saved preload file.


@note See: `Preload`, `PreloadEnd`, `PreloadStart`, `PreloadRefresh`, `PreloadEndEx`, `PreloadGenClear`, `PreloadGenStart`, `PreloadGenEnd`, `Preloader`.

*/
native PreloadGenStart  takes nothing returns nothing

/**
Writes the current Preload buffer to specified file.
The first and final preload directives are `call PreloadStart()` and `call PreloadEnd( realTime )`. The value represents the time in seconds between the calls `PreloadStart` and `PreloadGenEnd`. There's no way to get this value with the API.

Does not clear the buffer or timer after flushing. The file is overwritten. It's possible to specify subfolders: "myMapFolder/file.txt". Reforged: Any other tricks such as relative paths, UNC or drive letters will not write any files. Classic: possible to write to any path (verify?)

**Example preload file:*

	function PreloadFiles takes nothing returns nothing

		call PreloadStart()
		call Preload( "units\\human\\Knight\\Knight.mdx" )
		call PreloadEnd( 2.5 )

	endfunction

@param filename The filepath to be written to. Max length for filename is 259 characters (see: Windows MAX_PATH).


@note Before Reforged (which version?) you needed to enable "Allow Local Files" in registry.

@note **Save Path:**

Reforged: `%USERPROFILE%\Documents\Warcraft III\CustomMapData\`

Classic: ?

@note See: `Preload`, `PreloadEnd`, `PreloadStart`, `PreloadRefresh`, `PreloadEndEx`, `PreloadGenClear`, `PreloadGenStart`, `Preloader`.

*/
native PreloadGenEnd    takes string filename returns nothing

/**
Runs the filename as a preload script, only if the filename has an extension. For Jass, the capabilities are very restricted.

**Example (from blizzard.j)**:

    if (doPreload) then
            call Preloader( "scripts\\HumanMelee.pld" )
    endif

@param filename The file to execute.


@note There're no restrictions for Lua code if you add it to Preload files (which are supposed to be in Jass), that's only possible with [dirty hacks or manual editing](https://www.hiveworkshop.com/threads/blizzards-hidden-jass2lua-transpiler.337281/). If the map runs in Lua mode, the Jass code is compiled using Jass2Lua before execution.

@note On pre-Reforged (version?) this only works if you have enabled the usage of local files in your registry.
The registry key is `HKEY_CURRENT_USER\Software\Blizzard Entertainment\Warcraft III\Allow Local Files\`

@note Here are some ways to get the data out of the preload file into your map:
To store multiple integers you can use `SetPlayerTechMaxAllowed` to have a good
2d-array. Read via `GetPlayerTechMaxAllowed`.

For strings `SetPlayerName` is suited. To read use `GetPlayerName`.

Inside the preload script you can also use `ExecuteFunc` to call your map-defined
functions and interleave the preload script with your functions.

@note If you use `Preloader` to load some values into your map, these values
are very likely to be different for each player (since the player might not 
even have local files enabled), so treat them as async values.

@note Also see the documentation of `Preload` to see how to properly get the data
into the preload script.

@bug 1.33.0 and above: Due to aggressive file caching by the game, the preload file is only loaded and read once.
This means, updates to the saved preload file cannot be reloaded and old contents will be executed.

@note See: `Preload`, `PreloadEnd`, `PreloadStart`, `PreloadRefresh`, `PreloadEndEx`, `PreloadGenClear`, `PreloadGenStart`, `PreloadGenEnd`.

*/
native Preloader        takes string filename returns nothing


//============================================================================
//Machinima API
//============================================================================

/**


@bug (v1.32.10, Lua)
Enabling this mode without cinematic mode produces no visible differences. (TODO)

However, it shifts the rendered view towards north (without changing the
camera position) until disabled again.

See [test code](https://github.com/Luashine/wc3-test-maps/blob/master/BlzHideCinematicPanels.md)

@patch 1.32

*/
native BlzHideCinematicPanels                     takes boolean enable returns nothing


// Automation Test

/**


@patch 1.29

*/
native AutomationSetTestType                    takes string testType returns nothing

/**


@patch 1.29

*/
native AutomationTestStart                      takes string testName returns nothing

/**


@patch 1.30

*/
native AutomationTestEnd                        takes nothing returns nothing

/**


@patch 1.30

*/
native AutomationTestingFinished                takes nothing returns nothing

// JAPI Functions

/**
It is used inside a mouse event trigger’s action/condition it will return only the X coordinate (Cartesian System) of the current location of the mouse (ground) at the moment of the event trigger.


@event EVENT_PLAYER_MOUSE_MOVE
@patch 1.29

*/
native BlzGetTriggerPlayerMouseX                   takes nothing returns real

/**
It is used inside a mouse event trigger’s action/condition it will return only the Y coordinate (Cartesian System) of the current location of the mouse (ground) at the moment of the event trigger.


@event EVENT_PLAYER_MOUSE_MOVE
@patch 1.29

*/
native BlzGetTriggerPlayerMouseY                   takes nothing returns real

/**
It is used inside a mouse event trigger’s action/condition it will return a location (type, based on the ground not screen) of the mouse at the moment of the event trigger.


@event EVENT_PLAYER_MOUSE_MOVE
@patch 1.29

*/
native BlzGetTriggerPlayerMousePosition            takes nothing returns location

/**
It is used inside a mouse event trigger’s action/condition it will return the mousebuttontype (type) used at the moment of the event trigger.


@event EVENT_PLAYER_MOUSE_UP
@event EVENT_PLAYER_MOUSE_DOWN
@patch 1.29

*/
native BlzGetTriggerPlayerMouseButton              takes nothing returns mousebuttontype

/**
Set the ability tooltip (basic) of an ability at runtime.


@patch 1.29

*/
native BlzSetAbilityTooltip                        takes integer abilCode, string tooltip, integer level returns nothing

/**
Set the activated ability tooltip (for abilities such as defend which have an “active” state) of an ability at runtime.


@patch 1.29

*/
native BlzSetAbilityActivatedTooltip               takes integer abilCode, string tooltip, integer level returns nothing

/**
Set the ability tooltip (extended) of an ability at runtime.


@patch 1.29

*/
native BlzSetAbilityExtendedTooltip                takes integer abilCode, string extendedTooltip, integer level returns nothing

/**
Set the activated ability tooltip (Extended state for abilities such as defend which have an “active” state) of an ability at runtime.


@patch 1.29

*/
native BlzSetAbilityActivatedExtendedTooltip       takes integer abilCode, string extendedTooltip, integer level returns nothing

/**
Set the research ability tooltip (For abilities that can be learned (all abilities have this, but only hero abilities show it on the object editor, you can still change it with these natives)) of an ability at runtime.


@patch 1.29

*/
native BlzSetAbilityResearchTooltip                takes integer abilCode, string researchTooltip, integer level returns nothing

/**
Set the research ability tooltip (Extended state for abilities that can be learned (all abilities have this, but only hero abilities show it on the object editor, you can still change it with these natives)) of an ability at runtime.


@patch 1.29

*/
native BlzSetAbilityResearchExtendedTooltip        takes integer abilCode, string researchExtendedTooltip, integer level returns nothing

/**
Get the ability tooltip of an ability.
Supports Unit/Item/Ability/Tech Codes.


@async 
@patch 1.29

*/
native BlzGetAbilityTooltip                        takes integer abilCode, integer level returns string

/**
Get the ability activated tooltip (for abilities that have an “activated” state) of an ability.


@async 
@patch 1.29

*/
native BlzGetAbilityActivatedTooltip               takes integer abilCode, integer level returns string

/**
Get the extended ability tooltip of an ability.
Supports Unit/Item/Ability/Tech Codes.


@async 
@patch 1.29

*/
native BlzGetAbilityExtendedTooltip                takes integer abilCode, integer level returns string

/**
Get the extended ability activated tooltip (for abilities that have an “activated” state such as defend, Avatar, etc.) of an ability.


@async 
@patch 1.29

*/
native BlzGetAbilityActivatedExtendedTooltip       takes integer abilCode, integer level returns string

/**
Get the ability research tooltip (for abilities that can be researched/learned such as defend, hero abilities, etc) of an ability.


@async 
@patch 1.29

*/
native BlzGetAbilityResearchTooltip                takes integer abilCode, integer level returns string

/**
Get the extended ability research tooltip (for abilities that can be researched/learned such as defend, hero abilities, etc) of an ability.


@async 
@patch 1.29

*/
native BlzGetAbilityResearchExtendedTooltip        takes integer abilCode, integer level returns string

/**
Change(set) an ability’s icon at runtime.


@patch 1.29

*/
native BlzSetAbilityIcon                           takes integer abilCode, string iconPath returns nothing

/**
Get an ability’s icon at runtime, returns the icon path.
Supports Unit/Item/Ability/Tech Codes.


@patch 1.29

*/
native BlzGetAbilityIcon                           takes integer abilCode returns string

/**
Change(set) an ability’s activated icon (this is for abilities that have an activated state such as defend, avatar, etc) at runtime.


@patch 1.29

*/
native BlzSetAbilityActivatedIcon                  takes integer abilCode, string iconPath returns nothing

/**
Get an ability’s activated icon (this is for abilities that have an activated state such as defend, avatar, etc) at runtime, returns icon path.


@patch 1.29

*/
native BlzGetAbilityActivatedIcon                  takes integer abilCode returns string

/**
Get the ability X coordinate (Cartesian System) of the ability icon in the default 4x3 grid.


@patch 1.29

*/
native BlzGetAbilityPosX                           takes integer abilCode returns integer

/**
Get the ability Y coordinate (Cartesian System) of the ability icon in the default 4x3 grid.


@patch 1.29

*/
native BlzGetAbilityPosY                           takes integer abilCode returns integer

/**
Set the ability X coordinate (Cartesian System) of the ability icon in the default 4x3 grid.
As of the 1.31 PTR while you can specify the position of abilities such as “Build” directly in the object editor, you cannot do it with this native.


@patch 1.29

*/
native BlzSetAbilityPosX                           takes integer abilCode, integer x returns nothing

/**
Set the ability Y coordinate (Cartesian System) of the ability icon in the default 4x3 grid.


@note As of the 1.31 PTR while you can specify the position of abilities such as “Build” directly in the object editor, you cannot do it with this native.

@patch 1.29

*/
native BlzSetAbilityPosY                           takes integer abilCode, integer y returns nothing

/**
Get the ability X coordinate (Cartesian System) of the activated ability icon in the default 4x3 grid.


@patch 1.29

*/
native BlzGetAbilityActivatedPosX                  takes integer abilCode returns integer

/**
Get the ability Y coordinate (Cartesian System) of the activated ability icon in the default 4x3 grid.


@patch 1.29

*/
native BlzGetAbilityActivatedPosY                  takes integer abilCode returns integer

/**
Change(Set) the ability X coordinate (Cartesian System) of the activated ability icon in the default 4x3 grid.


@patch 1.29

*/
native BlzSetAbilityActivatedPosX                  takes integer abilCode, integer x returns nothing

/**
Change(Set) the ability Y coordinate (Cartesian System) of the activated ability icon in the default 4x3 grid.


@patch 1.29

*/
native BlzSetAbilityActivatedPosY                  takes integer abilCode, integer y returns nothing

/**
Get the max HP (hit points) of a unit.


@patch 1.29

*/
native BlzGetUnitMaxHP                             takes unit whichUnit returns integer

/**
Change(set) the max HP (hit points) of a unit.


@patch 1.29

*/
native BlzSetUnitMaxHP                             takes unit whichUnit, integer hp returns nothing

/**
Get the max mana of a unit.


@patch 1.29

*/
native BlzGetUnitMaxMana                           takes unit whichUnit returns integer

/**
Change(set) the max mana of a unit.


@patch 1.29

*/
native BlzSetUnitMaxMana                           takes unit whichUnit, integer mana returns nothing

/**
Change(set) the item name at runtime.


@patch 1.29
@bug Doesn't work.

*/
native BlzSetItemName                              takes item whichItem, string name returns nothing

/**
Change(set) the item description at runtime.


@patch 1.29

*/
native BlzSetItemDescription                       takes item whichItem, string description returns nothing

/**
Get the item description.


@async 
@patch 1.29

*/
native BlzGetItemDescription                       takes item whichItem returns string

/**
Change(set) the item tooltip at runtime.


@patch 1.29
@bug Doesn't work.

*/
native BlzSetItemTooltip                           takes item whichItem, string tooltip returns nothing

/**
Get the item tooltip.


@async 
@patch 1.29

*/
native BlzGetItemTooltip                           takes item whichItem returns string

/**
Change(set) the extended item tooltip at runtime.


@patch 1.29

*/
native BlzSetItemExtendedTooltip                   takes item whichItem, string extendedTooltip returns nothing

/**
Get the extended item tooltip.


@async 
@patch 1.29

*/
native BlzGetItemExtendedTooltip                   takes item whichItem returns string

/**
Change(set) the item icon path at runtime.


@patch 1.29

*/
native BlzSetItemIconPath                          takes item whichItem, string iconPath returns nothing

/**
Get the item icon path.


@patch 1.29

*/
native BlzGetItemIconPath                          takes item whichItem returns string

/**
Change(set) the unit name at runtime.


@patch 1.29

*/
native BlzSetUnitName                              takes unit whichUnit, string name returns nothing

/**
Change(set) the hero proper name at runtime. A "proper name" is the multiple names a hero can get at random, in this case it forces a specific proper name.


@patch 1.29

*/
native BlzSetHeroProperName                        takes unit whichUnit, string heroProperName returns nothing

/**
Get a unit’s base damage, weapon index can be either 0 and 1 (a unit can have two different attacks).


@patch 1.29

*/
native BlzGetUnitBaseDamage                        takes unit whichUnit, integer weaponIndex returns integer

/**
Change(set) a unit’s base damage, weapon index can be either 0 and 1 (a unit can have two different attacks) at runtime.


@patch 1.29

*/
native BlzSetUnitBaseDamage                        takes unit whichUnit, integer baseDamage, integer weaponIndex returns nothing

/**
Get a unit’s dice number (damage), weapon index can be either 0 and 1 (a unit can have two different attacks).


@patch 1.29

*/
native BlzGetUnitDiceNumber                        takes unit whichUnit, integer weaponIndex returns integer

/**
Change(set) a unit’s dice number (damage), weapon index can be either 0 and 1 (a unit can have two different attacks) at runtime.


@patch 1.29

*/
native BlzSetUnitDiceNumber                        takes unit whichUnit, integer diceNumber, integer weaponIndex returns nothing

/**
Get a unit’s dice sides (damage), weapon index can be either 0 and 1 (a unit can have two different attacks).


@patch 1.29

*/
native BlzGetUnitDiceSides                         takes unit whichUnit, integer weaponIndex returns integer

/**
Changes(set) unit’s dice sides (damage), weapon index can be either 0 and 1 (a unit can have two different attacks) at runtime.


@patch 1.29

*/
native BlzSetUnitDiceSides                         takes unit whichUnit, integer diceSides, integer weaponIndex returns nothing

/**
Get a unit’s Attack Cooldown, weapon index can be either 0 and 1 (a unit can have two different attacks).
Returns base attack cooldown (from the unit editor) in seconds, without any items, agility or buff bonuses.


@patch 1.29

*/
native BlzGetUnitAttackCooldown                    takes unit whichUnit, integer weaponIndex returns real

/**
Set a unit’s base Attack Cooldown, weapon index can be either 0 and 1 (a unit can have two different attacks) at runtime.


@patch 1.29

*/
native BlzSetUnitAttackCooldown                    takes unit whichUnit, real cooldown, integer weaponIndex returns nothing

/**
Changes(Set) the color of a special effect (tinting), using the specific player’s color, it will tint the effect on every part that it can be tinted.


@patch 1.29

*/
native BlzSetSpecialEffectColorByPlayer            takes effect whichEffect, player whichPlayer returns nothing

/**
Changes(Set) the color of a special effect (tinting), using R (RED) G (GREEN) B (BLUE) values, it will tint the effect on every part that it can be tinted.


@patch 1.29

*/
native BlzSetSpecialEffectColor                    takes effect whichEffect, integer r, integer g, integer b returns nothing

/**
Changes(Set) the alpha (transparency) of a special effect, the entire model will be made transparent based on the integer value.
*Integer Alpha goes from 0 to 100 (it equals percentage).*


@patch 1.29

*/
native BlzSetSpecialEffectAlpha                    takes effect whichEffect, integer alpha returns nothing

/**
Changes(Set) the scale of a special effect, the entire model will be scaled based on the scale value.

*Even though scale is a real (allows negative and positive numbers with decimals), it should be logically deduced that it shouldn’t be a negative value, object editor forces the minimum to be 0.10 (10% of the original size), it is not yet tested if it supports up to 0.01(1% of the original size).*


@patch 1.31

*/
native BlzSetSpecialEffectScale                    takes effect whichEffect, real scale returns nothing

/**
Changes(set) the X, Y and Z (altitude) coordinate (Cartesian System) of the current location of the special effect.


@note Z is not relative to terrain, it is absolute.

@patch 1.29

*/
native BlzSetSpecialEffectPosition                 takes effect whichEffect, real x, real y, real z returns nothing

/**
Sets the effect's absolute Z position (height). This native is functionally identical to `BlzSetSpecialEffectZ`.


@patch 1.29

*/
native BlzSetSpecialEffectHeight                   takes effect whichEffect, real height returns nothing

/**
Changes(set) the TimeScale (animation speed) of the passed special effect.

*TimeScale is a real, which means that it can be both negative and positive numbers with decimals, if you see the animation speed at 100.0 it will go at 100% speed, if you however set it to -100.0 it will go backwards and reset towards the beginning, however it can’t start at a negative value, if you want to reset the animation, you must pass it a negative value mid animation, else it will stand still.*


@patch 1.29

*/
native BlzSetSpecialEffectTimeScale                takes effect whichEffect, real timeScale returns nothing

/**
Changes(set) the time (how long the special effect lasts) of the passed special effect.

*TimeScale is a real, which means that it could be both negative and positive numbers with decimals, however it can’t be a negative value in this case.*


@patch 1.29

*/
native BlzSetSpecialEffectTime                     takes effect whichEffect, real time returns nothing

/**
Changes(set) the yaw, pitch and roll of the passed special effect.

*Yaw, pitch and roll are reals, which means that they can be both negative and positive numbers with decimals.*


@patch 1.29

*/
native BlzSetSpecialEffectOrientation              takes effect whichEffect, real yaw, real pitch, real roll returns nothing

/**


@patch 1.29

*/
native BlzSetSpecialEffectYaw                      takes effect whichEffect, real yaw returns nothing

/**


@patch 1.29

*/
native BlzSetSpecialEffectPitch                    takes effect whichEffect, real pitch returns nothing

/**


@patch 1.29

*/
native BlzSetSpecialEffectRoll                     takes effect whichEffect, real roll returns nothing

/**


@bug In 1.29 this native is bugged, it will set the X coordinate, but reset the Y and Z to where it was spawned in.

@patch 1.29

*/
native BlzSetSpecialEffectX                        takes effect whichEffect, real x returns nothing

/**


@bug In 1.29 this native is bugged, it will set the Y coordinate, but reset the X and Z to where it was spawned in.

@patch 1.29

*/
native BlzSetSpecialEffectY                        takes effect whichEffect, real y returns nothing

/**
Sets the effect's absolute Z position (height). 


@note Before 1.29 there was no direct way to set a special effect's height. The following trick was used as a workaround:

    // Creates a temporary platform in the air, the special effect will be put on top of it:
    set tempDestr = CreateDestructableZ('OTis', x, y, z, 0, 1, 0)
    // Effect spawns on top of platform
    call DestroyEffect(AddSpecialEffect(effectPath, x, y))
    // Remove platform immediately, only the effect will remain visible for its life duration
    call RemoveDestructable(tempDestr)

@bug In 1.29 this native is bugged, it will set the Z coordinate, but reset the X and Y to where it was spawned in.

@patch 1.29

*/
native BlzSetSpecialEffectZ                        takes effect whichEffect, real z returns nothing

/**
Changes(set) the current location of the special effect into the passed location.


@patch 1.29

*/
native BlzSetSpecialEffectPositionLoc              takes effect whichEffect, location loc returns nothing

/**
Get the X coordinate (Cartesian System) of the current location of the special effect.


@async 
@patch 1.29

*/
native BlzGetLocalSpecialEffectX                   takes effect whichEffect returns real

/**
Get the Y coordinate (Cartesian System) of the current location of the special effect.


@async 
@patch 1.29

*/
native BlzGetLocalSpecialEffectY                   takes effect whichEffect returns real

/**
Get the absolute Z coordinate (altitude)(Cartesian System) of the current location of the special effect.


@async 
@patch 1.29

*/
native BlzGetLocalSpecialEffectZ                   takes effect whichEffect returns real

/**
Clears all subanimations (tags) of the special effect. It does not affect normal animations.

**Example usage of subanimations:**

    // if you play anim attack it becomes attack slam:
    call BlzSpecialEffectAddSubAnimation(fx, SUBANIM_TYPE_SLAM)
    call BlzPlaySpecialEffect(fx, ANIM_TYPE_SPELL)
    call BlzSpecialEffectRemoveSubAnimation(fx, SUBANIM_TYPE_SLAM)

**Examples of animations, animation names:**

    stand | birth | death | decay | dissipate | walk | attack | morph | sleep | spell | portrait

**Examples of subanimations (tags), subanimation names:**

    first | second | third | fourth | fifth | defend | channel | slam | victory | throw | spin |
    ready | upgrade | lumber | gold | work | talk | swim | flesh | entangle | chainlightning | rooted |
    eattree | berserk | spiked | light | moderate | severe | critical | small | medium | large | alternateex |
    looping | wounded | fast | turn | left | right | fire | one | two | three | four | five | fill |
    puke | drain | flail | hit | off | complete


@patch 1.30

*/
native BlzSpecialEffectClearSubAnimations          takes effect whichEffect returns nothing

/**
Clears a specific subanimation (tag) of a specified special effect. (It does not affect normal animations).


@patch 1.30

*/
native BlzSpecialEffectRemoveSubAnimation          takes effect whichEffect, subanimtype whichSubAnim returns nothing

/**
Adds(set) a specific subanimation (tag) to a specified special effect.


@patch 1.30

*/
native BlzSpecialEffectAddSubAnimation             takes effect whichEffect, subanimtype whichSubAnim returns nothing

/**
Plays a specific subanimation (tag) on a specified special effect.


@patch 1.30

*/
native BlzPlaySpecialEffect                        takes effect whichEffect, animtype whichAnim returns nothing

/**
Plays a specific subanimation (tag) on a specified special effect at a specific timeScale (speed).

1. *Overrides the currently playing animation/subanimation.*
2. *TimeScale is a real, meaning that it can be both negative and positive numbers with decimals, there are examples in which you can use negative numbers mid animation to make it go backwards, however in this case it starts at 0 meaning that it can’t be negative.*


@patch 1.30

*/
native BlzPlaySpecialEffectWithTimeScale           takes effect whichEffect, animtype whichAnim, real timeScale returns nothing

/**
Returns the string representation of the name of the animation. `animtype` is a handle of the animation type.


@patch 1.30

*/
native BlzGetAnimName                              takes animtype whichAnim returns string

/**
Get the current unit armor of a specific unit (real value).

*Returns TOTAL amount of armor a unit has, including bonus (green) armor from  auras, buffs, agility and items. If you need just base or bonus armor, you need to calculate base armor yourself (for heroes: -2 + agility (excluding bonuses) * 0.3). Agility bonus also counts as bonus armor, e.g. +1 agility will be displayed as + 0.3 armor with default gameplay constants.*


@patch 1.29

*/
native BlzGetUnitArmor                             takes unit whichUnit returns real

/**
Changes(set) the unit armor of a specific unit, you pass it a real value, can be negative.

*Changes TOTAL amount of armor a unit has. If unit has a bonus (green) armor from an aura or item, base armor will be reduced to achieve total amount of armor you specified. E.g. a unit has 1+3 armor, if you set armor to 1.00, unit’s armor will be changed to -2+3*


@patch 1.29

*/
native BlzSetUnitArmor                             takes unit whichUnit, real armorAmount returns nothing

/**
Hides or unhides an ability for a unit.

@param whichUnit Unit to apply this to

@param abilId Rawcode of ability.

@param flag isHidden: true to hide, false to show.


@bug The boolean flag doesn't work as expected, it acts more like an integer counter: <https://www.hiveworkshop.com/threads/blzunithideability-and-blzunitdisableability-dont-work.312477/>.

@patch 1.29

*/
native BlzUnitHideAbility                          takes unit whichUnit, integer abilId, boolean flag returns nothing

/**
Enables/disables and hides/unhides an ability for a unit. A visible disabled ability is shown as deactivated, an invisible ability disappears from the grid.

**Example (Lua)**:

```{.lua}
-- assume u is Human Peasant, AHbu is ability for Human building.
-- keep enabled, but hide icon
BlzUnitDisableAbility(u, FourCC"AHbu", false, true)
```

@param whichUnit Unit to apply this to.

@param abilId Rawcode of ability.

@param flag isDisabled: true to disable (cannot click), false to enable ability.

@param hideUI isHidden: true to completely hide the icon, false to show icon. Icons are different for disabled/enabled abilities.


@bug (1.32.10 confirmed) The game counts isDisabled and hideUI internally as integers(?) If you called 5 times "hideUI = true" to hide an icon then you'll need to multiple times "hideUI = false" to show it again. I do not exactly understand how it's counted.
https://www.hiveworkshop.com/threads/blzunithideability-and-blzunitdisableability-dont-work.312477/

@patch 1.29

*/
native BlzUnitDisableAbility                       takes unit whichUnit, integer abilId, boolean flag, boolean hideUI returns nothing

/**
Makes a specific summoned unit permanent.


@patch 1.29

*/
native BlzUnitCancelTimedLife                      takes unit whichUnit returns nothing

/**
Returns true if the unit is selectable.


@patch 1.29

*/
native BlzIsUnitSelectable                         takes unit whichUnit returns boolean

/**
Returns true if unit is invulnerable.


@patch 1.29

*/
native BlzIsUnitInvulnerable                       takes unit whichUnit returns boolean

/**
Interrupts unit's current attack being casted.


@patch 1.29

*/
native BlzUnitInterruptAttack                      takes unit whichUnit returns nothing

/**
Get a real which is the collision size of the specific unit being passed. For reference, a peasant returns 16 and a MG returns 48.


@patch 1.29

*/
native BlzGetUnitCollisionSize                     takes unit whichUnit returns real

/**
Requires an ability ID and the ability level and returns the ability’s (at the level passed) mana cost. 


@note Since 1.31: use Level 0 to read manacosts of Level 1.

@patch 1.29

*/
native BlzGetAbilityManaCost                       takes integer abilId, integer level returns integer

/**
Requires an ability ID and the ability level and returns the ability’s (at the level passed) cooldown. *Since 1.31: use Level 0 to read cooldown from Level 1.*


@patch 1.29

*/
native BlzGetAbilityCooldown                       takes integer abilId, integer level returns real

/**
Changes(set) an ability’s cooldown at runtime for a specific unit.

@param whichUnit Target unit (handle).
@param abilId Rawcode of ability.
@param level Ability level.
@param cooldown New cooldown.


@note Cooldown is a real, which means that it supports negative and positive numbers with decimals, in this case setting it to negative allows you to reduce an ability’s cooldown.
@note It does not reduce the cooldown if the ability is currently on CD, it will have its new cooldown after the CD is over though.

@patch 1.29

*/
native BlzSetUnitAbilityCooldown                   takes unit whichUnit, integer abilId, integer level, real cooldown returns nothing

/**
Get a specific unit’s specific ability cooldown from a specific level.


@note It does not return the remaining cooldown when you use an ability but the max cooldown of that ability of that unit at that level.

@patch 1.29

*/
native BlzGetUnitAbilityCooldown                   takes unit whichUnit, integer abilId, integer level returns real

/**
Get a specific unit’s remaining ability cooldown.


@bug Sometimes it may return 0 for abilities based on Channel even when they are on cooldown.

@patch 1.29

*/
native BlzGetUnitAbilityCooldownRemaining          takes unit whichUnit, integer abilId returns real

/**
Reduces the current ability cooldown of a specific ability to 0.


@patch 1.29

*/
native BlzEndUnitAbilityCooldown                   takes unit whichUnit, integer abilCode returns nothing

/**


@patch 1.32

*/
native BlzStartUnitAbilityCooldown                 takes unit whichUnit, integer abilCode, real cooldown returns nothing

/**
Get a specific unit’s specific ability’s mana cost at a specific level.


@patch 1.29

*/
native BlzGetUnitAbilityManaCost                   takes unit whichUnit, integer abilId, integer level returns integer

/**
Set manacost of an ability (at ability level) for a unit.
Works as expected, so you can dynamically calculate the mana cost.


@patch 1.29

*/
native BlzSetUnitAbilityManaCost                   takes unit whichUnit, integer abilId, integer level, integer manaCost returns nothing

/**
Return unit's (altitude) Z map coordinate ([Cartesian System](https://en.wikipedia.org/wiki/Cartesian_coordinate_system)). Unit may be alive or dead.

Returns 0.0 if unit was removed or is null.

Retrieving Z is desync prone, this version might cause desyncs, but (unconfirmed) should be faster than `BlzGetUnitZ`, hence why both exist. In case that you are doing a single player map (campaign), you might decide to use this one instead of `BlzGetUnitZ`.


@note Terrain height is not synced between clients in multiplayer.

@note Since unit extends from widget, you can use widget-related functions too.
See: `BlzGetUnitZ`, `GetUnitX`, `GetUnitY`, `GetWidgetX`, `GetWidgetY`.

@async 
@patch 1.29

*/
native BlzGetLocalUnitZ                            takes unit whichUnit returns real

/**
Decreases (reduces) a specific player’s specific upgrade by a specific amount of levels.


@note Even though this native takes an integer and integers can be both negatives and positive numbers, in this specific case this native does not allow for an increment by setting the integer to negative.

@patch 1.29

*/
native BlzDecPlayerTechResearched                  takes player whichPlayer, integer techid, integer levels returns nothing

/**
Set the damage amount of a damage event.

In 1.31 PTR there’s currently 3 new damage events:

1. `EVENT_UNIT_DAMAGED` - old classic event for a specific unit;
2. `EVENT_PLAYER_UNIT_DAMAGED` - Same as 1, but for all units of a specific player on the map;

        // This seems to work fine anyway:
        call TriggerRegisterAnyUnitEventBJ(gg_trg_a, EVENT_PLAYER_UNIT_DAMAGING)

3. `EVENT_UNIT_DAMAGING` - triggers before any armor, armor type and other resistances. Event for a specific unit like 1.
4. `EVENT_PLAYER_UNIT_DAMAGING` - triggers before any armor, armor type and other resistances. Useful to modify either damage amount, attack type or damage type before any reductions done by game.

1 and 2 - modify the damage after any reduction.
3 and 4 - changes damage before reduction. Amount you set will be reduced later according to target’s resistance, armor etc.

If set to <=0 during 3 or 4, then 1 or 2 will never fire.
Misses don’t trigger any damage events.
Set to 0.00 to completely block the damage.
Set to negative value to heal the target instead of damaging.


@note Tip: calling `GetEventDamage` after you set it with this function will return the value you set.
@note If you’ll call `UnitDamageTarget` from within a trigger, which reacts to a damage event or triggered by one, it will cause infinite loop and game will crash, so you should handle such scenarios with additional logic.

@event EVENT_UNIT_DAMAGED
@patch 1.29

*/
native BlzSetEventDamage                           takes real damage returns nothing

/**
The target unit of the damage event.
If damage is AoE, your trigger will run separately for each target without known issues.
This returns the same result as `GetTriggerUnit`.


@event EVENT_UNIT_DAMAGED
@patch 1.31

*/
native BlzGetEventDamageTarget 	                   takes nothing returns unit

/**
Returns attacktype of the damage being taken.
Spell-damage is `ATTACK_TYPE_NORMAL`.


@event EVENT_UNIT_DAMAGED
@patch 1.31

*/
native BlzGetEventAttackType  	                   takes nothing returns attacktype

/**
Returns damagetype of the damage being taken.
Regular attack is `DAMAGE_TYPE_NORMAL`.


@event EVENT_UNIT_DAMAGED
@patch 1.31

*/
native BlzGetEventDamageType                       takes nothing returns damagetype

/**
Returns weapontype of a damage being taken. This only affects the sound of impact.


@event EVENT_UNIT_DAMAGED
@patch 1.31

*/
native BlzGetEventWeaponType  	                   takes nothing returns weapontype

/**
Set the attacktype of a damage being taken. 
Can be only used to change attacktype before armor reduction.


@event EVENT_UNIT_DAMAGED
@patch 1.31

*/
native BlzSetEventAttackType                       takes attacktype attackType returns boolean

/**
Set the damagetype of a damage being taken. 
Can be only used to change damagetype before armor reduction.


@event EVENT_UNIT_DAMAGED
@patch 1.31

*/
native BlzSetEventDamageType                       takes damagetype damageType returns boolean

/**
Set the weapontype of a damage being taken. 
Can be used to modify the sound of impact in the event before armor reduction.


@event EVENT_UNIT_DAMAGED
@patch 1.31

*/
native BlzSetEventWeaponType                       takes weapontype weaponType returns boolean

/**


@patch 1.32

*/
native BlzGetEventIsAttack                         takes nothing returns boolean

/**
"They do nothing of interest, just a compatibility thing" - MindWorX (Blizzard Developer), on [The Hive Discord](https://discord.com/channels/178569180625240064/311662737015046144/572101913349193738).


@patch 1.30

*/
native RequestExtraIntegerData                     takes integer dataType, player whichPlayer, string param1, string param2, boolean param3, integer param4, integer param5, integer param6 returns integer

/**
According to MindWorX (Blizzard Developer): //They do nothing of interest //Just a compatibility thing


@patch 1.30

*/
native RequestExtraBooleanData                     takes integer dataType, player whichPlayer, string param1, string param2, boolean param3, integer param4, integer param5, integer param6 returns boolean

/**
According to MindWorX (Blizzard Developer): //They do nothing of interest //Just a compatibility thing


@patch 1.30

*/
native RequestExtraStringData                      takes integer dataType, player whichPlayer, string param1, string param2, boolean param3, integer param4, integer param5, integer param6 returns string

/**
According to MindWorX (Blizzard Developer): //They do nothing of interest //Just a compatibility thing


@patch 1.30

*/
native RequestExtraRealData                        takes integer dataType, player whichPlayer, string param1, string param2, boolean param3, integer param4, integer param5, integer param6 returns real
// Add this function to follow the style of GetUnitX and GetUnitY, it has the same result as BlzGetLocalUnitZ

/**


@note Returns the same result as `BlzGetLocalUnitZ`.
@note Since unit extends from widget, you can use widget-related functions too.
See: `GetUnitX`, `GetUnitY`, `GetWidgetX`, `GetWidgetY`.

@async 
@patch 1.30

*/
native BlzGetUnitZ                                 takes unit whichUnit returns real

/**
Controls selection settings globally: enables/disables selection of units, and visibility of selection circles.

@param enableSelection true to enable, false to disable selection.
@param enableSelectionCircle true to show, false to hide selection circles on units and doodads.


@note 
Acts exactly the same as `EnableSelect`

@patch 1.31

*/
native BlzEnableSelections                         takes boolean enableSelection, boolean enableSelectionCircle returns nothing

/**
Returns whether unit selection is enabled (a global setting, see `BlzEnableSelections` and `EnableSelect`).


@note 
Does not account for `EnablePreSelect` or `EnableDragSelect` settings.

@patch 1.31

*/
native BlzIsSelectionEnabled                       takes nothing returns boolean

/**
Returns whether unit selection circles are shown (a global setting, see `BlzEnableSelections` and `EnableSelect`).


@note 
Does not account for `EnablePreSelect` or `EnableDragSelect` settings.

@patch 1.31

*/
native BlzIsSelectionCircleEnabled                 takes nothing returns boolean

/**


@patch 1.31

*/
native BlzCameraSetupApplyForceDurationSmooth      takes camerasetup whichSetup, boolean doPan, real forcedDuration, real easeInDuration, real easeOutDuration, real smoothFactor returns nothing

/**
Enable or disable the three green arrows when right-clicking on ground.


@patch 1.31

*/
native BlzEnableTargetIndicator                    takes boolean enable returns nothing

/**
Check if the the three green arrows when right-clicking on ground is shown or not.


@patch 1.31

*/
native BlzIsTargetIndicatorEnabled                 takes nothing returns boolean

/**
Toggles the rendering of terrain.

@param show `true` to render terrain, `false` no terrain (black background by default)


@note See: `BlzShowSkyBox`

@patch 1.32

*/
native BlzShowTerrain                              takes boolean show returns nothing

/**


@note See: `BlzShowTerrain`

@patch 1.32

*/
native BlzShowSkyBox                               takes boolean show returns nothing

/**
Does nothing (v1.32.10 without Battle.net App running), no files are created.


@note See: `BlzEndRecording`

@patch 1.32

*/
native BlzStartRecording                           takes integer fps returns nothing

/**
Does nothing (v1.32.10 without Battle.net App running), no files are created.


@note See: `BlzStartRecording`

@patch 1.32

*/
native BlzEndRecording                             takes nothing returns nothing

/**
Toggle team glow on whichUnit.
Will remove Hero glowing team color when set to false.

@param whichUnit Target unit (handle).
@param show Boolean to show/hide the team glow.

@patch 1.32

*/
native BlzShowUnitTeamGlow                         takes unit whichUnit, boolean show returns nothing


/**
Get a `framehandle` by specifying a specific `originframetype` and index (in most cases it should be 0 (first index), however it can go above 0 when using originframetypes such as `ORIGIN_FRAME_HERO_BUTTON`)

The one with indices above 0 are:

	// The ability buttons at the right bottom corner
    ORIGIN_FRAME_COMMAND_BUTTON <0 to 11>
	// The clickable hero icons at the left of the screen
    ORIGIN_FRAME_HERO_BUTTON <0 to 6>
	// See above for the following:
    ORIGIN_FRAME_HERO_HP_BAR <0 to 6>
    ORIGIN_FRAME_HERO_MANA_BAR <0 to 6>
    ORIGIN_FRAME_HERO_BUTTON_INDICATOR <0 to 6>
	// Item inventory buttons
    ORIGIN_FRAME_ITEM_BUTTON <0 to 5>
	// The buttons altering the minimap
    ORIGIN_FRAME_MINIMAP_BUTTON
	// Indices:
	// 0 = Menu
	// 1 = Allies
	// 2 = Log
	// 3 = Quest
    ORIGIN_FRAME_SYSTEM_BUTTON <0 to 3> 

Here is a basic example that creates a custom timerdialog window:

    set GameUI = BlzGetOriginFrame(ORIGIN_FRAME_GAME_UI, 0)
    set UIMain = BlzCreateFrame("TimerDialog", GameUI, 0, 0)
    call BlzFrameSetPoint(UIMain, FRAMEPOINT_CENTER, GameUI, FRAMEPOINT_CENTER, 0.25, 0.055)
    call BlzFrameSetSize(UIMain, 0.3, 0.7)

*Take a look at the .fdf files in the game’s CASC or point 1.3 (refer to this document’s table of contents for reference) it should give you some ideas.*

@param index to high values will return the frame from the last valid Index.

@note The first time a Frame enters the map's script it takes a handleId.

@note This is up for edition, this native is lacking a more in-depth explanation. For example a list of all of the originframetypes, and their possible indexes.

@patch 1.31

*/
native BlzGetOriginFrame                           takes originframetype frameType, integer index returns framehandle

/**
Disabling Auto Position will prevent the game using default positions for changed hidden frames as soon they reappear/their state is changed.


@patch 1.31

*/
native BlzEnableUIAutoPosition                     takes boolean enable returns nothing

/**
Hides/Shows most of the default in-game UI.
Unaffected: Mouse, Command Buttons, Chat, Messages, TimerDialog, Multiboard, Leaderboard and ConsoleUIBackdrop.


(De)Activades some auto-repositioning of default frames (see: `BlzEnableUIAutoPosition`).


@patch 1.31

*/
native BlzHideOriginFrames                         takes boolean enable returns nothing

/**


@pure 
@patch 1.31

*/
native BlzConvertColor                             takes integer a, integer r, integer g, integer b returns integer

/**
Loads in a TOCFile, to add/define Frame-Blueprints or Localized Strings
A TOC file contains a list, Each line is a path to a fdf (not case sensitve).


@bug The TOC needs to end with one or two empty lines.

@patch 1.31

*/
native BlzLoadTOCFile                              takes string TOCFile returns boolean

/**
Create a new Frame using a Frame-BluePrint name (fdf) as child of owner.
BluePrint needs to be loaded over TOC & fdf.
Owner and BluePrint have to be from the Frame family.
Can only create rootFrames (not subFrames).
Created Frames are stored into the game's Frame-Storage, `BlzGetFrameByName(name, createContext)`. Overwrites occupied slots.


@patch 1.31

*/
native BlzCreateFrame                              takes string name, framehandle owner, integer priority, integer createContext returns framehandle

/**
Like `BlzCreateFrame` but for the SimpleFrame family, Frame "SIMPLExxxx".


@note Only Frames loaded by used tocs are valid names.

@patch 1.31

*/
native BlzCreateSimpleFrame                        takes string name, framehandle owner, integer createContext returns framehandle

/**
Create & Define a new (Simple)Frame.
Can use a root-(Simple)Frame-BluePrint with inherits, when that is done it needs to be a loaded BluePrint.


@patch 1.31

*/
native BlzCreateFrameByType                        takes string typeName, string name, framehandle owner, string inherits, integer createContext returns framehandle

/**


@patch 1.31

*/
native BlzDestroyFrame                             takes framehandle frame returns nothing

/**
Unbinds a point of FrameA and places it relative to a point of FrameB.
When FrameB moves FrameA's point will keep this rule and moves with it.

Each point of a frame can be placed to one point.
By placing multiple points of one Frame a Size is enforced.


@patch 1.31

*/
native BlzFrameSetPoint                            takes framehandle frame, framepointtype point, framehandle relative, framepointtype relativePoint, real x, real y returns nothing

/**
Set frame absolute x,y position with framepointtype.
Coords are for the 4:3 Screen

    |0.0/0.6           0.8/0.6|
    |                         |
    |         0.4/0.3         |
    |                         |
    |0.0/0.0           0.8/0.0|

0.0/0.0 is bottomLeft (Minimap)
0.8/0.6 is TopRight (UpkeepCost)
In widescreen format one can go further left with -x or further right with x > 0.8
Only some Frames and their Children/Offspring can leave 4:3.
SimpleFrames, Leaderboard, TimerDialog, Multiboard, ConsoleUIBackdrop

@param point framepointtype is a point, position of which you set to move the frame relatively to it.


@patch 1.31

*/
native BlzFrameSetAbsPoint                         takes framehandle frame, framepointtype point, real x, real y returns nothing

/**
Unbinds all points of frame.
Useful to move frames with the next SetPoint.


@patch 1.31

*/
native BlzFrameClearAllPoints                      takes framehandle frame returns nothing

/**
Example:

    BlzHideOriginFrames(true)
    BlzFrameSetAllPoints(BlzGetOriginFrame(ORIGIN_FRAME_WORLD_FRAME, 0), BlzGetOriginFrame(ORIGIN_FRAME_GAME_UI, 0))

@param frame the frame moved/resized.

@patch 1.31

*/
native BlzFrameSetAllPoints                        takes framehandle frame, framehandle relative returns nothing

/**
Sets visibility of a frame and its children.

@param visible true is visible, false is invisible.


@patch 1.31

*/
native BlzFrameSetVisible                          takes framehandle frame, boolean visible returns nothing

/**
Returns visibility status of frame.

@param frame Target frame.


@async 
@patch 1.31

*/
native BlzFrameIsVisible                           takes framehandle frame returns boolean

/**
Requires a string for the frame name that you want to retrieve (get), and an integer (which in most cases should be 0) that specifies the index of the frame that you want to get (for example for inventory slots you have 6, from 0-5).

Read from the internal Frame-Storage.
The first time a Frame enters the map's script it takes a handleId.

Example: `BlzGetFrameByName("SimpleHeroLevelBar", 0)`.


@note Refer to fdf files for frame names.

@patch 1.31

*/
native BlzGetFrameByName                           takes string name, integer createContext returns framehandle

/**
Returns the string representation of frame name.

Inherited Frames lose their Name.
SimpleFrames return an empty String.

@param frame A handle to frame.


@patch 1.31

*/
native BlzFrameGetName                             takes framehandle frame returns string

/**
Ignores visibility. Triggers `FRAMEEVENT_CONTROL_CLICK`.


@patch 1.31

*/
native BlzFrameClick                               takes framehandle frame returns nothing

/**
Supports Warcraft 3 formatting codes:

* Colors (`|cffffcc00`)
* Multiple lines (`|n`, `\n`)


@patch 1.31

*/
native BlzFrameSetText                             takes framehandle frame, string text returns nothing

/**
Returns(Get) the text of that frame. For user input frames this text probably differs between them. For some frames the child contains the Text.


@async 
@patch 1.31

*/
native BlzFrameGetText                             takes framehandle frame returns string

/**
Start a NewLine and add text (TEXTAREA).

@patch 1.31

*/
native BlzFrameAddText                             takes framehandle frame, string text returns nothing

/**


@patch 1.31

*/
native BlzFrameSetTextSizeLimit                    takes framehandle frame, integer size returns nothing

/**


@patch 1.31

*/
native BlzFrameGetTextSizeLimit                    takes framehandle frame returns integer

/**
Changes text color of the frame. SimpleFrames only.

@param color Four byte integer of the form 0xaarrggbb. You can also use
`BlzConvertColor` to create such an integer.


@patch 1.31

*/
native BlzFrameSetTextColor                        takes framehandle frame, integer color returns nothing

/**


@patch 1.31

*/
native BlzFrameSetFocus                            takes framehandle frame, boolean flag returns nothing

/**


@patch 1.31

*/
native BlzFrameSetModel                            takes framehandle frame, string modelFile, integer cameraIndex returns nothing

/**
Turns on/off Interactivity/Events of frame.
A disabled frame is transparent to the mouse (can click on things behind it) and can have a different color/texture/frame than in enabled state.
The frame's Tooltip is still shown on hover.
(false) Removes KeyboardFocus.


@patch 1.31

*/
native BlzFrameSetEnable                           takes framehandle frame, boolean enabled returns nothing

/**


@async 
@patch 1.31

*/
native BlzFrameGetEnable                           takes framehandle frame returns boolean

/**
Affects child-Frames, when they don't have an own Alpha.

@param alpha 0 to 255.

@patch 1.31

*/
native BlzFrameSetAlpha                            takes framehandle frame, integer alpha returns nothing

/**


@async 
@patch 1.31

*/
native BlzFrameGetAlpha                            takes framehandle frame returns integer

/**


@patch 1.31

*/
native BlzFrameSetSpriteAnimate                    takes framehandle frame, integer primaryProp, integer flags returns nothing

/**
Overwrittes some fdf setup.

@param flag texture fill setting: 0 to stretch, 1 to tile (BACKDROP).
@param blend Use transparency.


@patch 1.31


*/
native BlzFrameSetTexture                          takes framehandle frame, string texFile, integer flag, boolean blend returns nothing

/**
Affects child-Frames, when they don't have an own Scale.

@patch 1.31

*/
native BlzFrameSetScale                            takes framehandle frame, real scale returns nothing

/**
Frame tooltip is visible when hovered with the mouse. Otherwise tooltip will be hidden.

tooltip is limited to 4:3, but not it's children.
SimpleFrame tooltips are not hidden with this call.
frame and tooltip have to be from the same Family (Frames/SimpleFrames).
tooltip can only serve one frame.
It's not possible to undo this.


@bug Crashes the game, on hover, when done twice (same pair).
@bug Frames should not be used as tooltips for simple Frames (Crash on PTR 1.31).

@patch 1.31

*/
native BlzFrameSetTooltip                          takes framehandle frame, framehandle tooltip returns nothing

/**
The mouse cursor is forced into the frame and can not leave it. New cages (true) will overwrite old ones. Some frames can not be used to imprison the mouse.

@param enable Enable mouse cage.


@patch 1.31

*/
native BlzFrameCageMouse                           takes framehandle frame, boolean enable returns nothing

/**
Sets the current Frame Value. Only for FrameType that use this feature:
POPUPMENU, SLIDER, SIMPLESTATUSBAR, STATUSBAR.


@patch 1.31

*/
native BlzFrameSetValue                            takes framehandle frame, real value returns nothing

/**
Gets the current Frame Value.


@async 
@patch 1.31

*/
native BlzFrameGetValue                            takes framehandle frame returns real

/**


@patch 1.31

*/
native BlzFrameSetMinMaxValue                      takes framehandle frame, real minValue, real maxValue returns nothing

/**
SLIDER accuracy for User.

@patch 1.31

*/
native BlzFrameSetStepSize                         takes framehandle frame, real stepSize returns nothing

/**


@patch 1.31

*/
native BlzFrameSetSize                             takes framehandle frame, real width, real height returns nothing

/**
SimpleFrames only.
@param color Four byte integer of the form 0xaarrggbb. You can also use `BlzConvertColor` to create such an integer.

@patch 1.31

*/
native BlzFrameSetVertexColor                      takes framehandle frame, integer color returns nothing

/**
Used to reorder the children of a Frame.
SimpleFrames have fixed internal Layers. Which only contain String/Textures.
For SimpleFrames Level sets them higher/lower to all other SimpleFrames.

@param level bigger number gives a higher position.

@patch 1.31

*/
native BlzFrameSetLevel                            takes framehandle frame, integer level returns nothing

/**


@patch 1.31

*/
native BlzFrameSetParent                           takes framehandle frame, framehandle parent returns nothing

/**


@patch 1.31

*/
native BlzFrameGetParent                           takes framehandle frame returns framehandle

/**


@async 
@patch 1.31

*/
native BlzFrameGetHeight                           takes framehandle frame returns real

/**


@async 
@patch 1.31

*/
native BlzFrameGetWidth                            takes framehandle frame returns real

/**
Only works for String (SimpleFrames).

@patch 1.31

*/
native BlzFrameSetFont                             takes framehandle frame, string fileName, real height, integer flags returns nothing

/**


@patch 1.31

*/
native BlzFrameSetTextAlignment                    takes framehandle frame, textaligntype vert, textaligntype horz returns nothing

/**
Ignores String/Texture.


@patch 1.32.6

*/
native BlzFrameGetChildrenCount                    takes framehandle frame returns integer

/**
Valid Indexes are 0 to `BlzFrameGetChildrenCount` - 1.
Ignores String/Texture.
Breaks `BlzGetOriginFrame` when the same frame is first get using `BlzFrameGetChild`.


@patch 1.32.6

*/
native BlzFrameGetChild                            takes framehandle frame, integer index returns framehandle

/**
The event starts for all players when one player triggers it.

The Event Getter functions. 

* `BlzGetTriggerFrame`
* `BlzGetTriggerFrameEvent`
* `BlzGetTriggerFrameValue`
* `BlzGetTriggerFrameText`
* `GetTriggerPlayer`

`BlzGetTriggerFrameValue` & `BlzGetTriggerFrameText` are only set when the
FrameEventEvent has use of them.


@patch 1.31

*/
native BlzTriggerRegisterFrameEvent                takes trigger whichTrigger, framehandle frame, frameeventtype eventId returns event

/**


@patch 1.31

*/
native BlzGetTriggerFrame                          takes nothing returns framehandle

/**


@patch 1.31

*/
native BlzGetTriggerFrameEvent                     takes nothing returns frameeventtype

/**
Returns the user input value of the triggered frame. (Slider, popupmenu, scrollbar...)
One has to use this native to sync user input, if that is needed.


@note This is a hidden native in PTR 1.31 (has to be declared to be usable in Jass).

@patch 1.31

*/
native BlzGetTriggerFrameValue                     takes nothing returns real

/**
Returns the user input text of the triggered frame. (EditBox)
One has to use this native to sync user input, if that is needed.

Limited to something like ~255 bytes.


@note This is a hidden native in PTR 1.31 (has to be declared to be usable in Jass).

@patch 1.31

*/
native BlzGetTriggerFrameText                      takes nothing returns string

/**
Create an event that listens to messages sent by player with prefix. (see: `BlzSendSyncData`).

One can create a player SyncEvent for any prefix with `TriggerRegisterPlayerEvent(whichTrigger, whichPlayer, EVENT_PLAYER_SYNC_DATA)`.

`GetTriggerPlayer()` is the message source.

@param fromServer "should be false".


@patch 1.31

*/
native BlzTriggerRegisterPlayerSyncEvent           takes trigger whichTrigger, player whichPlayer, string prefix, boolean fromServer returns event

/**
The player running this function sends a string message to all players.
See also `BlzTriggerRegisterPlayerSyncEvent`.

@param prefix Limited to something like 255 bytes.

@param data Limited to something like 255 bytes.


@patch 1.31

*/
native BlzSendSyncData                             takes string prefix, string data returns boolean

/**


@patch 1.31

*/
native BlzGetTriggerSyncPrefix                     takes nothing returns string

/**


@patch 1.31

*/
native BlzGetTriggerSyncData                       takes nothing returns string

/**
Registers event to call trigger when player presses a key + metakey.
Key presses are synced by the game between players automatically.

Meta keys are modifier keys like CTRL, SHIFT, ALT. See `BlzGetTriggerPlayerMetaKey`. If you just want a key press without them, use 0.

**Example (Lua):**

    trg_key = CreateTrigger()
    -- prints oskey as object, metakey as integer
    TriggerAddAction(trg_key, function() print(BlzGetTriggerPlayerKey(),  BlzGetTriggerPlayerMetaKey()) end)
     
    -- register key press ESCAPE
    BlzTriggerRegisterPlayerKeyEvent(trg_key, Player(0), OSKEY_ESCAPE, 0, false)
     
    -- register key press CTRL+1
    BlzTriggerRegisterPlayerKeyEvent(trg_key, Player(0), OSKEY_1, 2, false)

@param metaKey Bitfield. MetaKeys are "none"(0), "shift"(1), "control"(2), "alt"(4) and "META"(8) (Windows key). They can be combined 2 + 4 = 6.
The player needs to hold all specified metakeys to trigger the event.

@param keyDown If keyDown = false: trigger is called once when key is released (unpressed).
If keyDown = true: calls trigger repeatedly while key is being held down. In V1.31.1 this happens once. In V1.32.10 repeats until released at approximately 30 times per second and fluctuating.


@patch 1.31

*/
native BlzTriggerRegisterPlayerKeyEvent            takes trigger whichTrigger, player whichPlayer, oskeytype key, integer metaKey, boolean keyDown returns event

/**
Returns the key that was pressed during current event.

**Example:** `if BlzGetTriggerPlayerKey() == OSKEY_F then ...`


@note See: `BlzTriggerRegisterPlayerKeyEvent`.

@patch 1.31

*/
native BlzGetTriggerPlayerKey                      takes nothing returns oskeytype

/**
Returns the meta keys that were pressed (aka [modifier keys](https://en.wikipedia.org/wiki/Modifier_key)).

**Example:** if player pressed CTRL+W then metakey=2 and oskeytype=OSKEY_W

**Meta keys:**

* 0 = None
* 1 = Shift
* 2 = Control (CTRL)
* 4 = ALT
* 8 = Meta aka Windows key aka Super

Meta keys can be pressed simultaneously (CTRL+SHIFT+W) in that case, you need to add the numbers or use bit OR/AND/XOR.
CTRL+SHIFT = 2+1 = 3. CTRL+SHIFT+ALT = 2+1+4 = 7.


@note See: `BlzTriggerRegisterPlayerKeyEvent`.

@patch 1.31

*/
native BlzGetTriggerPlayerMetaKey                  takes nothing returns integer

/**


@patch 1.31

*/
native BlzGetTriggerPlayerIsKeyDown                takes nothing returns boolean

/**
Sets cursor visibility.

@param enable true to show, false to hide cursor.


@patch 1.31

*/
native BlzEnableCursor                             takes boolean enable returns nothing

/**
x & y are px upto the used resolution `BlzGetLocalClientWidth()` `BlzGetLocalClientHeight()`.


@patch 1.31

*/
native BlzSetMousePos                              takes integer x, integer y returns nothing

/**
Gets the width (pixels) of the Warcraft 3 window.


@async 
@patch 1.31

*/
native BlzGetLocalClientWidth                      takes nothing returns integer

/**
Gets the height (pixels) of the Warcraft 3 window.


@async 
@patch 1.31

*/
native BlzGetLocalClientHeight                     takes nothing returns integer

/**
Returns true if Warcraft 3 window is in focus.


@async 
@patch 1.31

*/
native BlzIsLocalClientActive                      takes nothing returns boolean

/**
Returns the unit that is currently hovered by the mouse of the local player.

@async 
@patch 1.31

*/
native BlzGetMouseFocusUnit                        takes nothing returns unit

/**
Uses a new Texture for the minimap.

@patch 1.31

*/
native BlzChangeMinimapTerrainTex                  takes string texFile returns boolean

/**
Returns the used warcraft 3 Lcid.

    //  English (US)            = 'enUS' 
    //  English (UK)            = 'enGB' 
    //  French                  = 'frFR' 
    //  German                  = 'deDE' 
    //  Spanish                 = 'esES' 
    //  Italian                 = 'itIT' 
    //  Czech                   = 'csCZ' 
    //  Russian                 = 'ruRU' 
    //  Polish                  = 'plPL' 
    //  Portuguese (Brazilian)  = 'ptBR' 
    //  Portuguese (Portugal)   = 'ptPT' 
    //  Turkish                 = 'tkTK' 
    //  Japanese                = 'jaJA' 
    //  Korean                  = 'koKR' 
    //  Chinese (Traditional)   = 'zhTW' 
    //  Chinese (Simplified)    = 'zhCN' 
    //  Thai                    = 'thTH'
	

@note Warcraft 3 Lcids can be found in `config.ini` inside the CASC.

@async 
@patch 1.31

*/
native BlzGetLocale                                takes nothing returns string

/**


@patch 1.31

*/
native BlzGetSpecialEffectScale                    takes effect whichEffect returns real

/**


@patch 1.31

*/
native BlzSetSpecialEffectMatrixScale              takes effect whichEffect, real x, real y, real z returns nothing

/**


@patch 1.31

*/
native BlzResetSpecialEffectMatrix                 takes effect whichEffect returns nothing

/**


@patch 1.31

*/
native BlzGetUnitAbility                           takes unit whichUnit, integer abilId returns ability

/**
Returns a handle to specific unit's ability instance.


@note Last added ability is at index 0, older abilities are pushed up.

@patch 1.31

*/
native BlzGetUnitAbilityByIndex                    takes unit whichUnit, integer index returns ability

/**


@patch 1.33

*/
native BlzGetAbilityId                             takes ability whichAbility returns integer

/**
Displays the message in chat as if it were sent by the specified player. The message does not appear in log (F12).

@param whichPlayer The target player will be shown as sender of the message.

@param recipient Changes the type of chat channel prefix shown. It has no effect on the message's visibility.

* 0: "All" chat prefix
* 1: "Allies"
* 2: "Observers"
* 3: "Private"
* 4+: Defaults to private too.

@param message Text to show.


@patch 1.31

*/
native BlzDisplayChatMessage                       takes player whichPlayer, integer recipient, string message returns nothing

/**
This does not update `IsUnitPaused` and keeps the command card visible. Otherwise identical to `PauseUnit()`.


@patch 1.31

*/
native BlzPauseUnitEx                              takes unit whichUnit, boolean flag returns nothing
// native BlzFourCC2S                                 takes integer value returns string
// native BlzS2FourCC                                 takes string value returns integer

/**


@patch 1.32

*/
native BlzSetUnitFacingEx                          takes unit whichUnit, real facingAngle returns nothing


/**


@patch 1.32

*/
native CreateCommandButtonEffect                   takes integer abilityId, string order returns commandbuttoneffect

/**


@patch 1.32

*/
native CreateUpgradeCommandButtonEffect            takes integer whichUprgade returns commandbuttoneffect

/**


@patch 1.32

*/
native CreateLearnCommandButtonEffect              takes integer abilityId returns commandbuttoneffect

/**


@patch 1.32

*/
native DestroyCommandButtonEffect                  takes commandbuttoneffect whichEffect returns nothing

// Bit Operations

/**
Returns the result of connecting all bits of both numbers using OR (in regards of binary numeral system). It returns a number with bits, being set in at least one of the numbers.

3v1 => 3 (0011 v 0001 => 0011)
2v5 => 7 (0010 v 0101 => 0111)



@pure 
@patch 1.31

*/
native BlzBitOr                                    takes integer x, integer y returns integer

/**
Returns the result of connecting all bits of both numbers using AND (in regards of binary numeral system). It tells which bits are set for both integers.

3&1 => 1 (0011 & 0001 => 0001)
2&1 => 0 (0010 & 0001 => 0000)
11&7 => 3 (1011 & 0111 => 0011)
13&5 => 5 (1101 & 0101 => 0101)
12&6 => 4 (1100 & 0100 => 0100)



@pure 
@patch 1.31

*/
native BlzBitAnd                                   takes integer x, integer y returns integer

/**
Returns the result of connecting all bits of both numbers using XOR (Difference) (in regards of binary numeral system). Each Bit being different between x and y becomes 1; every bit being equal becomes 0.

2 xor 5 => 7 (0010 xor 0101 => 0111)
6 xor 8 => 14  (0110 xor 1000 => 1110)



@pure 
@patch 1.31

*/
native BlzBitXor                                   takes integer x, integer y returns integer 

// Intanced Object Operations
// Ability

/**


@patch 1.31

*/
native BlzGetAbilityBooleanField                   takes ability whichAbility, abilitybooleanfield whichField returns boolean

/**


@patch 1.31

*/
native BlzGetAbilityIntegerField                   takes ability whichAbility, abilityintegerfield whichField returns integer

/**


@patch 1.31

*/
native BlzGetAbilityRealField                      takes ability whichAbility, abilityrealfield whichField returns real

/**


@patch 1.31

*/
native BlzGetAbilityStringField                    takes ability whichAbility, abilitystringfield whichField returns string

/**


@patch 1.31
@bug Should not be used (crash): Use `BlzGetAbilityIntegerLevelField`.

*/
native BlzGetAbilityBooleanLevelField              takes ability whichAbility, abilitybooleanlevelfield whichField, integer level returns boolean

/**


@patch 1.31

*/
native BlzGetAbilityIntegerLevelField              takes ability whichAbility, abilityintegerlevelfield whichField, integer level returns integer

/**


@patch 1.31

*/
native BlzGetAbilityRealLevelField                 takes ability whichAbility, abilityreallevelfield whichField, integer level returns real

/**


@patch 1.31

*/
native BlzGetAbilityStringLevelField               takes ability whichAbility, abilitystringlevelfield whichField, integer level returns string

/**


@patch 1.31

*/
native BlzGetAbilityBooleanLevelArrayField         takes ability whichAbility, abilitybooleanlevelarrayfield whichField, integer level, integer index returns boolean

/**


@patch 1.31

*/
native BlzGetAbilityIntegerLevelArrayField         takes ability whichAbility, abilityintegerlevelarrayfield whichField, integer level, integer index returns integer

/**


@patch 1.31

*/
native BlzGetAbilityRealLevelArrayField            takes ability whichAbility, abilityreallevelarrayfield whichField, integer level, integer index returns real

/**


@patch 1.31

*/
native BlzGetAbilityStringLevelArrayField          takes ability whichAbility, abilitystringlevelarrayfield whichField, integer level, integer index returns string

/**


@patch 1.31

*/
native BlzSetAbilityBooleanField                   takes ability whichAbility, abilitybooleanfield whichField, boolean value returns boolean

/**


@patch 1.31

*/
native BlzSetAbilityIntegerField                   takes ability whichAbility, abilityintegerfield whichField, integer value returns boolean

/**


@patch 1.31

*/
native BlzSetAbilityRealField                      takes ability whichAbility, abilityrealfield whichField, real value returns boolean

/**


@patch 1.31

*/
native BlzSetAbilityStringField                    takes ability whichAbility, abilitystringfield whichField, string value returns boolean

/**


@patch 1.31
@bug Should not be used (crash): Use `BlzSetAbilityIntegerLevelField`.

*/
native BlzSetAbilityBooleanLevelField              takes ability whichAbility, abilitybooleanlevelfield whichField, integer level, boolean value returns boolean

/**


@patch 1.31

*/
native BlzSetAbilityIntegerLevelField              takes ability whichAbility, abilityintegerlevelfield whichField, integer level, integer value returns boolean

/**


@patch 1.31

*/
native BlzSetAbilityRealLevelField                 takes ability whichAbility, abilityreallevelfield whichField, integer level, real value returns boolean

/**


@patch 1.31

*/
native BlzSetAbilityStringLevelField               takes ability whichAbility, abilitystringlevelfield whichField, integer level, string value returns boolean

/**


@patch 1.31

*/
native BlzSetAbilityBooleanLevelArrayField         takes ability whichAbility, abilitybooleanlevelarrayfield whichField, integer level, integer index, boolean value returns boolean

/**


@patch 1.31

*/
native BlzSetAbilityIntegerLevelArrayField         takes ability whichAbility, abilityintegerlevelarrayfield whichField, integer level, integer index, integer value returns boolean

/**


@patch 1.31

*/
native BlzSetAbilityRealLevelArrayField            takes ability whichAbility, abilityreallevelarrayfield whichField, integer level, integer index, real value returns boolean

/**


@patch 1.31

*/
native BlzSetAbilityStringLevelArrayField          takes ability whichAbility, abilitystringlevelarrayfield whichField, integer level, integer index, string value returns boolean

/**


@patch 1.31

*/
native BlzAddAbilityBooleanLevelArrayField         takes ability whichAbility, abilitybooleanlevelarrayfield whichField, integer level, boolean value returns boolean

/**


@patch 1.31

*/
native BlzAddAbilityIntegerLevelArrayField         takes ability whichAbility, abilityintegerlevelarrayfield whichField, integer level, integer value returns boolean

/**


@patch 1.31

*/
native BlzAddAbilityRealLevelArrayField            takes ability whichAbility, abilityreallevelarrayfield whichField, integer level, real value returns boolean

/**


@patch 1.31

*/
native BlzAddAbilityStringLevelArrayField          takes ability whichAbility, abilitystringlevelarrayfield whichField, integer level, string value returns boolean

/**


@patch 1.31

*/
native BlzRemoveAbilityBooleanLevelArrayField      takes ability whichAbility, abilitybooleanlevelarrayfield whichField, integer level, boolean value returns boolean

/**


@patch 1.31

*/
native BlzRemoveAbilityIntegerLevelArrayField      takes ability whichAbility, abilityintegerlevelarrayfield whichField, integer level, integer value returns boolean

/**


@patch 1.31

*/
native BlzRemoveAbilityRealLevelArrayField         takes ability whichAbility, abilityreallevelarrayfield whichField, integer level, real value returns boolean

/**


@patch 1.31

*/
native BlzRemoveAbilityStringLevelArrayField       takes ability whichAbility, abilitystringlevelarrayfield whichField, integer level, string value returns boolean

// Item 

/**


@patch 1.31

*/
native BlzGetItemAbilityByIndex                    takes item whichItem, integer index returns ability

/**


@patch 1.31

*/
native BlzGetItemAbility                           takes item whichItem, integer abilCode returns ability

/**


@note The item has to be carried by a unit for this to work.

@patch 1.31

*/
native BlzItemAddAbility                           takes item whichItem, integer abilCode returns boolean

/**


@patch 1.31

*/
native BlzGetItemBooleanField                      takes item whichItem, itembooleanfield whichField returns boolean

/**


@patch 1.31

*/
native BlzGetItemIntegerField                      takes item whichItem, itemintegerfield whichField returns integer

/**


@patch 1.31

*/
native BlzGetItemRealField                         takes item whichItem, itemrealfield whichField returns real

/**


@patch 1.31

*/
native BlzGetItemStringField                       takes item whichItem, itemstringfield whichField returns string

/**


@patch 1.31

*/
native BlzSetItemBooleanField                      takes item whichItem, itembooleanfield whichField, boolean value returns boolean

/**


@patch 1.31

*/
native BlzSetItemIntegerField                      takes item whichItem, itemintegerfield whichField, integer value returns boolean

/**


@patch 1.31

*/
native BlzSetItemRealField                         takes item whichItem, itemrealfield whichField, real value returns boolean

/**


@patch 1.31

*/
native BlzSetItemStringField                       takes item whichItem, itemstringfield whichField, string value returns boolean

/**


@patch 1.31

*/
native BlzItemRemoveAbility                        takes item whichItem, integer abilCode returns boolean

// Unit 

/**


@note Many fields don't work at all.
@patch 1.31

*/
native BlzGetUnitBooleanField                      takes unit whichUnit, unitbooleanfield whichField returns boolean

/**


@note Many fields don't work at all.
@patch 1.31

*/
native BlzGetUnitIntegerField                      takes unit whichUnit, unitintegerfield whichField returns integer

/**


@note Many fields don't work at all.
@patch 1.31

*/
native BlzGetUnitRealField                         takes unit whichUnit, unitrealfield whichField returns real

/**


@note Many fields don't work at all.
@patch 1.31

*/
native BlzGetUnitStringField                       takes unit whichUnit, unitstringfield whichField returns string

/**


@note Many fields don't work at all.
@patch 1.31

*/
native BlzSetUnitBooleanField                      takes unit whichUnit, unitbooleanfield whichField, boolean value returns boolean

/**
Changes a unit's stats integer field.

There're quirks when changing stats, some values don't apply immediately and some don't work at all, likely due to how the game engine uses them. Example:

`BlzSetUnitIntegerField(unit, UNIT_IF_HIT_POINTS_REGENERATION_TYPE)`

Regeneration type values are as follows:

* 0 - Never
* 1 - Always
* 2 - Only on blight
* 4 - Only at night

Changing the regeneration type at runtime WILL NOT work, even if true is returned (false positive).

For vision, it appears changing them to a specific value does not immediately change it. Instead, it will change over time to approach and reach said value. However, if one wishes to decrease the vision range, and the initial vision range is greater than 1800, the vision will remain at 1800. Thus, one must change it first to 1800, then to the desired value. Otherwise, vision change works as intended. One cannot increase vision beyond 1800.

Going into a fountain of life will not increase a unit's hp regeneration rate. Modifying regeneration rate is instant.


@note Many fields don't work at all.
@patch 1.31

*/
native BlzSetUnitIntegerField                      takes unit whichUnit, unitintegerfield whichField, integer value returns boolean

/**


@note Many fields don't work at all.
@patch 1.31

*/
native BlzSetUnitRealField                         takes unit whichUnit, unitrealfield whichField, real value returns boolean

/**


@note Many fields don't work at all.
@patch 1.31

*/
native BlzSetUnitStringField                       takes unit whichUnit, unitstringfield whichField, string value returns boolean

// Unit Weapon

/**


@bug Might crash the game when called on a unit with no attack.
@patch 1.31

*/
native BlzGetUnitWeaponBooleanField                takes unit whichUnit, unitweaponbooleanfield whichField, integer index returns boolean

/**


@bug Might crash the game when called on a unit with no attack.
@patch 1.31

*/
native BlzGetUnitWeaponIntegerField                takes unit whichUnit, unitweaponintegerfield whichField, integer index returns integer

/**


@bug Might crash the game when called on a unit with no attack.
@patch 1.31

*/
native BlzGetUnitWeaponRealField                   takes unit whichUnit, unitweaponrealfield whichField, integer index returns real

/**


@bug Might crash the game when called on a unit with no attack.
@patch 1.31

*/
native BlzGetUnitWeaponStringField                 takes unit whichUnit, unitweaponstringfield whichField, integer index returns string

/**


@patch 1.31

*/
native BlzSetUnitWeaponBooleanField                takes unit whichUnit, unitweaponbooleanfield whichField, integer index, boolean value returns boolean

/**


@patch 1.31

*/
native BlzSetUnitWeaponIntegerField                takes unit whichUnit, unitweaponintegerfield whichField, integer index, integer value returns boolean

/**
Problems:
unitweaponfields `UNIT_WEAPON_RF_ATTACK_RANGE` and `UNIT_WEAPON_RF_ATTACK_PROJECTILE_SPEED` do not appear to change in value, even if the operation is reported successful (returns a false positive). This was tested at indices 0 - 3.

The getter equivalent of the native above does not work too (returns 0).


@patch 1.31

*/
native BlzSetUnitWeaponRealField                   takes unit whichUnit, unitweaponrealfield whichField, integer index, real value returns boolean

/**


@patch 1.31

*/
native BlzSetUnitWeaponStringField                 takes unit whichUnit, unitweaponstringfield whichField, integer index, string value returns boolean

// Skin

/**


@patch 1.32

*/
native BlzGetUnitSkin                                 takes unit whichUnit returns integer

/**


@patch 1.32

*/
native BlzGetItemSkin                                 takes item whichItem returns integer
// native BlzGetDestructableSkin                         takes destructable whichDestructable returns integer

/**
Replaces a unit's model with the unit's model referenced by the skinId.
`BlzSetUnitSkin(whichUnit, 'hfoo')` will replace whichUnit model with the footman one.
Scale from the unit referenced by the skinId is applied to whichUnit.
SoundSet from the unit referenced by the skinId is applied to whichUnit.

@param whichUnit The function will modify this unit's model.
@param skinId The function will apply the skinId model to whichUnit.


@note Upon function call, all attachment visual effect are removed from whichUnit.
@patch 1.32

*/
native BlzSetUnitSkin                                 takes unit whichUnit, integer skinId returns nothing

/**


@patch 1.32

*/
native BlzSetItemSkin                                 takes item whichItem, integer skinId returns nothing
// native BlzSetDestructableSkin                         takes destructable whichDestructable, integer skinId returns nothing


/**


@patch 1.32

*/
native BlzCreateItemWithSkin                       takes integer itemid, real x, real y, integer skinId returns item

/**
Creates a unit with the model from the unit referenced by the skinId.
`BlzCreateUnitWithSkin(players[0], 'hpea', 0, 0, 270, 'hfoo')` will create a peasant with a footman model.
Scale from the unit referenced by the skinId is applied to whichUnit.
SoundSet from the unit referenced by the skinId is applied to whichUnit.

@param id The owner of the unit.
@param unitid The rawcode of the unit.
@param x The x-coordinate of the unit.
@param y The y-coordinate of the unit.
@param face Unit facing in degrees.
@param skinId The function will apply the skinId model to the unit created.

@patch 1.32

*/
native BlzCreateUnitWithSkin                       takes player id, integer unitid, real x, real y, real face, integer skinId returns unit

/**


@patch 1.32

*/
native BlzCreateDestructableWithSkin               takes integer objectid, real x, real y, real face, real scale, integer variation, integer skinId returns destructable

/**


@patch 1.32

*/
native BlzCreateDestructableZWithSkin              takes integer objectid, real x, real y, real z, real face, real scale, integer variation, integer skinId returns destructable

/**


@patch 1.32

*/
native BlzCreateDeadDestructableWithSkin           takes integer objectid, real x, real y, real face, real scale, integer variation, integer skinId returns destructable

/**


@patch 1.32

*/
native BlzCreateDeadDestructableZWithSkin          takes integer objectid, real x, real y, real z, real face, real scale, integer variation, integer skinId returns destructable

/**


@patch 1.32

*/
native BlzGetPlayerTownHallCount                   takes player whichPlayer returns integer


/**


@patch 1.33

*/
native BlzQueueImmediateOrderById      takes unit whichUnit, integer order returns boolean

/**

@note If the order is to build a structure and the unit can build that structure in principle but the player lacks the resources for it, then the unit
will be pinged on the minimap in yellow for its owning player.

@bug If the order is to build a structure, this function will return `false` even if the unit accepts the order.

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

@note If the order is to build a structure and the unit can build that structure in principle but the player lacks the resources for it, then the unit
will be pinged on the minimap in yellow for its owning player.

@note If the order is to build a structure and the unit can build that structure in principle (and the spot is not blocked, either),
this function will still return `true` even if the player lacks the resources for it and the unit has no other orders.

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

// returns the number of orders the unit currently has queued up

/**
Returns the number of orders the unit currently has queued up.

@patch 1.33

*/
native BlzGetUnitOrderCount takes unit whichUnit returns integer
// clears either all orders or only queued up orders

/**
Clears either all orders or only queued up orders.

@patch 1.33

*/
native BlzUnitClearOrders takes unit whichUnit, boolean onlyQueued returns nothing
// stops the current order and optionally clears the queue

/**
Stops the current order and optionally clears the queue.

@patch 1.33

*/
native BlzUnitForceStopOrder takes unit whichUnit, boolean clearQueue returns nothing

