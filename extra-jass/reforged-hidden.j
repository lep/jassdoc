// These are native functions that are exported by the game's API,
// but left undeclared for modding use for some reason.
// Be cautious if you decide to use them. Their removal from the game will break your map completely.
// Sorted alphabetically

/**
Removes an ability like `UnitRemoveAbility`: level is set to 0 and skill points are not refunded.

@note Jass: (v2.0.2) This hidden API native must be defined first.
The native definition must come before any other function definitions:

`native BlzDeleteHeroAbility takes unit unitHandle, integer abilID returns nothing`

@note **Example (Lua, 2.0.2):**

```{.lua}
-- Test hero ability remove on a hero vs unit
heroStr = CreateUnit(Player(0), FourCC("Otch"), -100, 0, 90)
heroStrAbilRawcode = FourCC("AOws") -- tauren stomp (max lvl 3)
UnitAddAbility(heroStr, heroStrAbilRawcode)

-- Ability is removed instantly from a hero
BlzDeleteHeroAbility(heroStr, heroStrAbilRawcode)

-- Now with a non-hero footman:
footman = CreateUnit(Player(0), FourCC("hfoo"), 200, 0, 90)
footmanManaAuraRawcode = FourCC("AHab")
UnitAddAbility(footman, footmanManaAuraRawcode)

-- UnitRemoveAbility works immediately
UnitRemoveAbility(footman, footmanManaAuraRawcode)

-- When used on a regular unit: removes the ability immediately,
-- but the ability icon is not removed until you click again
-- or until the entire UI is refreshed (Alt+Tab)
BlzDeleteHeroAbility(footman, footmanManaAuraRawcode)
```

@bug Intended only for heroes. While it works to remove abilities from regular units too,
the UI is not instantly updated to remove the icon too.

@param unitHandle hero unit
@param abilID ability rawcode
*/
native BlzDeleteHeroAbility takes unit unitHandle, integer abilID returns nothing // "(Hunit;I)V");


/**
Returns the current skin ID (the rawcode of the object by default).

@note Jass: (v2.0.2) This hidden API native must be defined first.
The native definition must come before any other function definitions:

`native BlzGetDestructableSkin takes destructable destHandle returns integer`

@note Lua: Exported as an API function.

**Example (Lua, v2.0.2):**

```{.lua}
tree0 = CreateDestructable(FourCC("ATtr"), -256, 256, 180.0, 1.0, 0)
print(BlzGetDestructableSkin(tree0)) --> returns 1096053874 aka FourCC("ATtr")
```

@note See: `BlzSetDestructableSkin`, `BlzCreateDestructableWithSkin`, `BlzCreateDestructableZWithSkin`,
`BlzCreateDeadDestructableWithSkin`, `BlzCreateDeadDestructableZWithSkin`;
`BlzCreateUnitWithSkin`, `BlzSetUnitSkin`, `BlzCreateItemWithSkin`, `BlzSetItemSkin`.
*/
native BlzGetDestructableSkin takes destructable destHandle returns integer


/**
Returns index representing hero's attribute:

- 0 = not a hero / invalid
- 1 = strength
- 2 = intelligence
- 3 = agility

Note, in-game UI has a different order: 1. str, 2. agi 3. int

@note Jass: (v2.0.2) This hidden API native must be defined first.
The native definition must come before any other function definitions:

`native BlzGetHeroPrimaryStat takes unit whichHero returns integer`

@note **Test code (Lua, 2.0.2):**

```{.lua}
heroStr = CreateUnit(Player(0), FourCC("Otch"), -100, 0, 90)
heroAgi = CreateUnit(Player(0), FourCC("Obla"), 0, 0, 90)
heroInt = CreateUnit(Player(0), FourCC("Ofar"), 100, 0, 90)

footman = CreateUnit(Player(0), FourCC("hfoo"), 200, 0, 90)

print(BlzGetHeroPrimaryStat(heroStr))
print(BlzGetHeroPrimaryStat(heroAgi))
print(BlzGetHeroPrimaryStat(heroInt))
print(BlzGetHeroPrimaryStat(footman))
```

@param whichHero hero unit
*/
native BlzGetHeroPrimaryStat takes unit whichHero returns integer


/**
Lua, 2.0.2: always returns zero. Apparently, "whichHero" parameter was supposed to be
a "unitTypeId" (a rawcode) but is wrongly defined in the game API as `unit`.

@note Jass: (v2.0.2) This hidden API native must be defined first.
The native definition must come before any other function definitions:

`native BlzGetHeroPrimaryStatById takes unit whichHero returns integer`
*/
native BlzGetHeroPrimaryStatById takes unit whichHero returns integer


/**
Returns hero's attribute value, otherwise returns 0 (unit is null or not a hero, invalid stat index).

Equivalent to `GetHeroStr`/`GetHeroAgi`/`GetHeroInt`.
Item bonuses and 'Aamk' Attribute Bonus ability count correctly.

@note Jass: (v2.0.2) This hidden API native must be defined first.
The native definition must come before any other function definitions:

`native BlzGetHeroStat takes unit whichHero, integer whichStat, boolean includeBonuses returns integer`

@note **Test code (Lua, 2.0.2):**

```{.lua}
heroStr = CreateUnit(Player(0), FourCC("Otch"), -100, 0, 90)
heroAgi = CreateUnit(Player(0), FourCC("Obla"), 0, 0, 90)
heroInt = CreateUnit(Player(0), FourCC("Ofar"), 100, 0, 90)

itemAll = UnitAddItemById(heroStr, FourCC("ckng"))
itemAgi = UnitAddItemById(heroAgi, FourCC("rag1"))
itemInt = UnitAddItemById(heroInt, FourCC("ciri"))

UnitAddAbility(heroStr, FourCC("Aamk"))
UnitAddAbility(heroAgi, FourCC("Aamk"))
UnitAddAbility(heroInt, FourCC("Aamk"))

for _, hero in pairs({heroStr, heroAgi, heroInt}) do
	local str,agi,int = BlzGetHeroStat(hero, 1, false), BlzGetHeroStat(hero, 3, false), BlzGetHeroStat(hero, 2, false)
	local strB,agiB,intB = BlzGetHeroStat(hero, 1, true), BlzGetHeroStat(hero, 3, true), BlzGetHeroStat(hero, 2, true)
	local strOld, agiOld, intOld = GetHeroStr(hero, false), GetHeroAgi(hero, false), GetHeroInt(hero, false)
	local strOldB, agiOldB, intOldB = GetHeroStr(hero, true), GetHeroAgi(hero, true), GetHeroInt(hero, true)
	local name = GetUnitName(hero)
	print(name .." blz base:  ".. str     .."/".. agi     .."/".. int)
	print(name .." blz total: ".. strB    .."/".. agiB    .."/".. intB)
	print(name .." old base:  ".. strOld  .."/".. agiOld  .."/".. intOld)
	print(name .." old total: ".. strOldB .."/".. agiOldB .."/".. intOldB)
end
```

@param whichHero target unit
@param whichStat attribute index:

- 1 = strength
- 2 = intelligence
- 3 = agility

Note, in-game UI has a different order: 1. str, 2. agi 3. int

@param includeBonuses true - count bonuses; false - base stat
*/
native BlzGetHeroStat takes unit whichHero, integer whichStat, boolean includeBonuses returns integer


/**
Gets unit's armor type aka "Combat - Armor Type" aka unit integer field 'uarm'.

Equivalent to `BlzGetUnitIntegerField(targetUnit, UNIT_IF_ARMOR_TYPE)`

DO NOT CONFUSE with "Combat - Defense Type" 'udty', which shows up as icon in unit UI.

Returns an index from 0 to 5:

- 0 aka `ARMOR_TYPE_WHOKNOWS` - also returned on error
- 1 aka `ARMOR_TYPE_FLESH`
- 2 aka `ARMOR_TYPE_METAL`
- 3 aka `ARMOR_TYPE_WOOD`
- 4 aka `ARMOR_TYPE_ETHREAL`
- 5 aka `ARMOR_TYPE_STONE`

@note Jass: (v2.0.2) This hidden API native must be defined first.
The native definition must come before any other function definitions:

`native BlzGetUnitArmorType takes unit whichUnit returns integer`

@note **Test code (Lua, 2.0.2):**

```{.lua}
heroStr = CreateUnit(Player(0), FourCC("Otch"), 300, 0, 90) -- Tauren hero; we: flesh
peasant = CreateUnit(Player(0), FourCC("hpea"), 400, 0, 90) -- human builder; we: flesh
footman = CreateUnit(Player(0), FourCC("hfoo"), 500, 0, 90) -- footman; we: metal
catapult = CreateUnit(Player(0), FourCC("ocat"), 600, 0, 90) -- orc catapult; we: wood
farmhouse = CreateUnit(Player(0), FourCC("hhou"), 600, 256, 90) -- human farmhouse; we: wood
magebuilding = CreateUnit(Player(0), FourCC("hars"), 600, 512, 90) -- human mage building; we: stone
portal = CreateUnit(Player(0), FourCC("hprt"), 600, 768, 90) -- scenario portal; we: ethereal


print(BlzGetUnitArmorType(nil)) -- 0 aka `ARMOR_TYPE_WHOKNOWS`
print(BlzGetUnitArmorType(heroStr)) -- 1 aka `ARMOR_TYPE_FLESH`
print(BlzGetUnitArmorType(peasant)) -- 1 aka `ARMOR_TYPE_FLESH`
print(BlzGetUnitArmorType(footman)) -- 2 aka `ARMOR_TYPE_METAL`
print(BlzGetUnitArmorType(catapult)) -- 3 aka `ARMOR_TYPE_WOOD`
print(BlzGetUnitArmorType(farmhouse)) -- 3 aka `ARMOR_TYPE_WOOD`
print(BlzGetUnitArmorType(magebuilding)) -- 5 aka `ARMOR_TYPE_STONE`
print(BlzGetUnitArmorType(portal)) -- 4 aka `ARMOR_TYPE_ETHREAL`

print(BlzGetUnitIntegerField(magebuilding, UNIT_IF_ARMOR_TYPE))
print(BlzGetUnitIntegerField(portal, UNIT_IF_ARMOR_TYPE))
```

@note See: `armortype`, `ConvertArmorType`. For items' armor type see: `ITEM_IF_ARMOR_TYPE` aka 'iarm', `itemintegerfield`.

@param whichUnit target unit
*/
native BlzGetUnitArmorType takes unit whichUnit returns integer


/**
Gets unit's movement type aka "Movement - Type" aka unit integer field 'umvt'.

Equivalent to `BlzGetUnitIntegerField(targetUnit, UNIT_IF_MOVE_TYPE)`

Returns number corresponding to the enabled type:

- 0 aka `MOVE_TYPE_UNKNOWN`
- 1 aka `MOVE_TYPE_FOOT`
- 2 aka `MOVE_TYPE_FLY`
- 4 aka `MOVE_TYPE_HORSE`
- 8 aka `MOVE_TYPE_HOVER`
- 16 aka `MOVE_TYPE_FLOAT`
- 32 aka `MOVE_TYPE_AMPHIBIOUS`
- 64 aka `MOVE_TYPE_UNBUILDABLE`

@note Jass: (v2.0.2) This hidden API native must be defined first.
The native definition must come before any other function definitions:

`native BlzGetUnitMovementType takes unit whichUnit returns integer`

@note **Test code (Lua, 2.0.2):**

```{.lua}
tower = CreateUnit(Player(0), FourCC("hwtw"), 100, 0, 90) -- watchtower, none
peasant = CreateUnit(Player(0), FourCC("hpea"), 300, 0, 90) -- peasant, foot
hippogryph = CreateUnit(Player(0), FourCC("hgry"), 400, 0, 90) -- hippo, fly
knight = CreateUnit(Player(0), FourCC("hkni"), 500, 0, 90) -- human knight, horse
sorceress = CreateUnit(Player(0), FourCC("hsor"), 600, 0, 90) -- human female mage, hover
boat = CreateUnit(Player(0), FourCC("hbot"), 700, 0, 90) -- trading vessel, float
shipyard = CreateUnit(Player(0), FourCC("hshy"), 900, 0, 90) -- shipyard building, float
naga = CreateUnit(Player(0), FourCC("nnsw"), 1000, 0, 90) -- naga sirene, amphibious
-- TODO: unbuildable type = CreateUnit(Player(0), FourCC(""), 1100, 0, 90) -- 

moveTest = {tower, 0, peasant, 1, hippogryph, 2, knight, 4, sorceress, 8,
boat, 16, shipyard, 16, naga, 32}
for i = 1, #moveTest, 2 do
	local unit, expectedType = moveTest[i], moveTest[i+1]
	local getMoveType, getIntField = BlzGetUnitMovementType(unit), BlzGetUnitIntegerField(unit, UNIT_IF_MOVE_TYPE)
	print(i ..": move type native/intfield=".. getMoveType .."/".. getIntField ..", expected=".. expectedType)
end
```

@param whichUnit target unit
*/
native BlzGetUnitMovementType takes unit whichUnit returns integer


/**
Unknown (v2.0.2)

@note Jass: (v2.0.2) This hidden API native must be defined first.
The native definition must come before any other function definitions:

`native BlzSetCameraGuardBand takes real minx, real maxx, real miny, real maxy returns boolean`
*/
native BlzSetCameraGuardBand takes real minx, real maxx, real miny, real maxy returns boolean // "(RRRR)B");


/**
Broken.

@note Jass: (v2.0.2) This hidden API native must be defined first.
The native definition must come before any other function definitions:

Lua: Exported as an API function. (v2.0.2.22796): I couldn't get it to work with the test code below

```{.lua}
-- Spawns 4 trees with 4 different skin variations
tree0 = CreateDestructable(FourCC("ATtr"), -256, 256, 180, 1, 0)
tree1 = CreateDestructable(FourCC("ATtr"), 0, 256, 180, 1, 1)
tree2 = CreateDestructable(FourCC("ATtr"), 256, 256, 180, 1, 2)
tree3 = CreateDestructable(FourCC("ATtr"), 512, 256, 180, 1, 3)

-- Does not change the returned skin ID:
BlzSetDestructableSkin(tree1, FourCC("WTst")) -- another random tree destructable
```

@note See: `BlzGetDestructableSkin`, `BlzCreateDestructableWithSkin`, `BlzCreateDestructableZWithSkin`,
`BlzCreateDeadDestructableWithSkin`, `BlzCreateDeadDestructableZWithSkin`;
`BlzCreateUnitWithSkin`, `BlzSetUnitSkin`, `BlzCreateItemWithSkin`, `BlzSetItemSkin`.
*/
native BlzSetDestructableSkin takes destructable destHandle, integer skinID returns nothing


/**
Sets a hero's main attribute (strength, agility or intelligence).

Does nothing on a regular unit/null.

@note Jass: (v2.0.2) This hidden API native must be defined first.
The native definition must come before any other function definitions:

`native BlzSetHeroPrimaryStat takes unit whichHero, integer whichStat returns nothing`

@note Example (Lua, v2.0.2):**

```{.lua}
heroStr = CreateUnit(Player(0), FourCC("Otch"), -100, 0, 90)
heroAgi = CreateUnit(Player(0), FourCC("Obla"), 0, 0, 90)
heroInt = CreateUnit(Player(0), FourCC("Ofar"), 100, 0, 90)

-- Set the main attribute for each hero
BlzSetHeroPrimaryStat(heroStr, 42)
BlzSetHeroPrimaryStat(heroAgi, 42)
BlzSetHeroPrimaryStat(heroInt, 42)
```

@bug v2.0.2.22796: Technically, can be set to math.maxinteger, math.mininteger.
However, this messes up internal data and causes accumulation errors,
inability to reset to 0 or default etc.

```{.lua}
heroAgi = CreateUnit(Player(0), FourCC("Obla"), 0, 0, 90)

-- Max/Min int32 bug:
-- Set to max integer: max armor, atk etc.
BlzSetHeroPrimaryStat(heroAgi, math.maxinteger)
-- Set to min integer:
BlzSetHeroPrimaryStat(heroAgi, math.mininteger)

-- This no longer resets correctly: attack can be 2-4k, armor at 0
-- Call it twice and armor is set to -2:
BlzSetHeroPrimaryStat(heroAgi, 0)

-- Repeated invocations may still increase attack, 
BlzSetHeroPrimaryStat(heroAgi, 42)
```

@param whichHero target hero unit
@param whichStat set main attribute to this value
*/
native BlzSetHeroPrimaryStat takes unit whichHero, integer whichStat returns nothing


/**
Sets hero's attribute value by attribute index.

@note See: `SetHeroStr`, `SetHeroAgi`, `SetHeroInt`, `ModifyHeroStat`, `SetHeroStat`.

@note Jass: (v2.0.2) This hidden API native must be defined first.
The native definition must come before any other function definitions:

`native BlzSetHeroStatEx takes unit whichHero, integer whichStat, integer statValue, boolean permanent returns nothing`

@note **Example (Lua, v2.0.2):**

```{.lua}
heroStr = CreateUnit(Player(0), FourCC("Otch"), -100, 0, 90)
heroAgi = CreateUnit(Player(0), FourCC("Obla"), 0, 0, 90)
heroInt = CreateUnit(Player(0), FourCC("Ofar"), 100, 0, 90)

itemAll = UnitAddItemById(heroStr, FourCC("ckng"))
itemAgi = UnitAddItemById(heroAgi, FourCC("rag1"))
itemInt = UnitAddItemById(heroInt, FourCC("ciri"))

for index, hero in pairs({heroStr, heroInt, heroAgi}) do
	for i = 1, 3 do
		BlzSetHeroStatEx(hero, i, i, false)
	end
	for i = 1, 3 do
		BlzSetHeroStatEx(hero, i, i*4, true)
	end
end
```

@param whichHero target hero unit
@param whichStat attribute index: 1 = strength, 2 = intelligence, 3 = agility (note different order in game)
@param statValue new value
@param permanent unknown
*/
native BlzSetHeroStatEx takes unit whichHero, integer whichStat, integer statValue, boolean permanent returns nothing


/**
Sets unit's movement type aka "Movement - Type" aka unit integer field 'umvt'.

Equivalent to `BlzSetUnitIntegerField(targetUnit, UNIT_IF_MOVE_TYPE, movementTypeNumber)`

@bug v2.0.2.22796: Changes the field value, but it has no effect.

@note Jass: (v2.0.2) This hidden API native must be defined first.
The native definition must come before any other function definitions:

`native BlzSetUnitMovementType takes unit whichUnit, integer movementType returns nothing`

@note **Example (Lua, v2.0.2):**

```{.lua}
heroStr = CreateUnit(Player(0), FourCC("Otch"), -100, 0, 90)
BlzSetUnitMovementType(heroStr, 32)
```

@note See: `BlzGetUnitMovementType`
*/
native BlzSetUnitMovementType takes unit whichUnit, integer movementType returns nothing


/**
Unknown

@note Jass: (v2.0.2) This hidden API native must be defined first.
The native definition must come before any other function definitions:

`native ClearStackedSound takes string soundLabel, real x, real y returns nothing`
*/
native ClearStackedSound takes string soundLabel, real x, real y returns nothing // "(SRR)V");


/**
Unknown

@note Jass: (v2.0.2) This hidden API native must be defined first.
The native definition must come before any other function definitions:

`native ClearStackedSoundRect takes string soundLabel, rect hRect returns nothing`
*/
native ClearStackedSoundRect takes string soundLabel, rect hRect returns nothing // "(SHrect;)V");


/**
Does nothing in release builds.

@note Jass: (v2.0.2) This hidden API native must be defined first.
The native definition must come before any other function definitions:

`native DebugBreak takes integer unused returns nothing`
*/
native DebugBreak takes integer unused returns nothing // "(I)V");


/**
Unknown

@note Jass: (v2.0.2) This hidden API native must be defined first.
The native definition must come before any other function definitions:

`native DialogSetAsync takes dialog dialogHandle returns nothing`
*/
native DialogSetAsync takes dialog dialogHandle returns nothing // "(Hdialog;)V");


/**
Returns X map coordinate for player's starting location.

This native is equivalent to the regular Jass function `GetPlayerStartLocationX` in "Blizzard.j".

@note Jass: (v2.0.2) This hidden API native must be defined first.
The native definition must come before any other function definitions:

`native GetPlayerStartLocationX takes player whichPlayer returns real`

However, it will conflict due to the first declaration in Blizzard.j.

@note Lua (v2.0.2.22796): Must override Blizzard.j first: either remove the declaration there
or replace it with the Jass' `native` declaration (it works, because the file is transpiled
to Lua on the fly).

This is due to the loading order:
1. common.j
2. Blizzard.j (overshadows the native function)
3. war3map.j/war3map.lua (your map script)

Then insert the modified "Blizzard.j" inside map's MPQ root or,
if "Allow Local Files" is enabled, inside the game folder.

@note **Test code (Lua, v2.0.2.22796):**

In a modified map with removed BJ functions, the function pointers still exist and are
pointing close to the real natives, distinct from BJ functions with consecutive memory pointers.

Expected memory pointer order for functions (for reference only):

- Natives:
    1. GetPlayerStartLocation
    2. GetPlayerStartLocationX
    3. GetPlayerStartLocationY
    4. GetPlayerColor
- BJ Functions:
    1. CopySaveGameBJ
    2. GetPlayerStartLocationX
    3. GetPlayerStartLocationY
    4. GetPlayerStartLocationLoc

Run the following code and see for yourself where the pointers go:

```{.lua}
-- string.format is needed to gracefully bypass DebugUtils' fancy pointer renaming
print(string.format("%s=%s", CopySaveGameBJ, "(BJ)CopySaveGameBJ"))
print(string.format("%s=%s", GetPlayerStartLocationX, "(BJ/native?)GetPlayerStartLocationX"))
print(string.format("%s=%s", GetPlayerStartLocationY, "(BJ/native?)GetPlayerStartLocationY"))
print(string.format("%s=%s", GetPlayerStartLocationLoc, "(BJ)GetPlayerStartLocationLoc"))
print(string.format("%s=%s", GetStartLocationLoc, "(native)GetStartLocationLoc"))
```
*/
native GetPlayerStartLocationX takes player whichPlayer returns real


/**
Returns Y map coordinate for player's starting location.

This native is equivalent to the regular Jass function `GetPlayerStartLocationY` in "Blizzard.j".

@note Jass: (v2.0.2) This hidden API native must be defined first.
The native definition must come before any other function definitions:

`native GetPlayerStartLocationY takes player whichPlayer returns real`

However, it will conflict due to the first declaration in Blizzard.j.

Lua: Declaration in Blizzard.j must be removed.

See `GetPlayerStartLocationX` for a full explanation.
*/
native GetPlayerStartLocationY takes player whichPlayer returns real


/**
Unknown, probably be related to Reforged dialogue scenes.

@note Jass: (v2.0.2) This hidden API native must be defined first.
The native definition must come before any other function definitions:

`native SetCinematicSceneWithSkinId takes integer skinId, playercolor playerColor, string speaker, string dialogue, real sceneDuration, real dialogueDuration returns nothing`
*/
native SetCinematicSceneWithSkinId takes integer skinId, playercolor playerColor, string speaker, string dialogue, real sceneDuration, real dialogueDuration returns nothing // "(IHplayercolor;SSRR)V");


/**
Unknown, must be related to Reforged FaceFX animations. Maybe some of the campaign maps use this?

@note Jass: (v2.0.2) This hidden API native must be defined first.
The native definition must come before any other function definitions:

`native SetSoundFacialAnimationPlaybackMode takes sound soundHandle, integer facialAnimationPlaybackMode returns boolean`
*/
native SetSoundFacialAnimationPlaybackMode takes sound soundHandle, integer facialAnimationPlaybackMode returns boolean // "(Hsound;I)B");


/**
Unknown

@note Jass: (v2.0.2) This hidden API native must be defined first.
The native definition must come before any other function definitions:

`native SetStackedSound takes string soundLabel, real x, real y returns nothing`
*/
native SetStackedSound takes string soundLabel, real x, real y returns nothing // "(SRR)V");


/**
Unknown

@note Jass: (v2.0.2) This hidden API native must be defined first.
The native definition must come before any other function definitions:

`native SetStackedSoundRect takes string soundLabel, rect hRect returns nothing`
*/
native SetStackedSoundRect takes string soundLabel, rect hRect returns nothing // "(SHrect;)V");
