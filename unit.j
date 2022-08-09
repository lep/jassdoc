// Unit API

/**
@param id The owner of the unit.
@param unitid The rawcode of the unit.
@param x The x-coordinate of the unit.
@param y The y-coordinate of the unit.
@param face Unit facing in degrees.
*/
native CreateUnit takes player id, integer unitid, real x, real y, real face returns unit

/**
@param face Unit facing in degrees
*/
native CreateUnitByName takes player whichPlayer, string unitname, real x, real y, real face returns unit

/**
@param id The owner of the unit.
@param unitid The rawcode of the unit.
@param whichLocation The position of the unit.
@param face Unit facing in degrees.
*/
native CreateUnitAtLoc takes player id, integer unitid, location whichLocation, real face returns unit

/**
@param face Unit facing in degrees
*/
native CreateUnitAtLocByName takes player id, string unitname, location whichLocation, real face returns unit

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
native CreateCorpse takes player whichPlayer, integer unitid, real x, real y, real face returns unit



native KillUnit takes unit whichUnit returns nothing

native RemoveUnit takes unit whichUnit returns nothing

native ShowUnit takes unit whichUnit, boolean show returns nothing



native SetUnitState takes unit whichUnit, unitstate whichUnitState, real newVal returns nothing

/**
@note If the unit has movementspeed of zero the unit will be moved but the model
of the unit will not move.
@note This does not cancel orders of the unit. `SetUnitPosition` does cancel orders.
*/
native SetUnitX takes unit whichUnit, real newX returns nothing

/**
@note If the unit has movementspeed of zero the unit will be moved but the model
of the unit will not move.
@note This does not cancel orders of the unit. `SetUnitPosition` does cancel orders.
*/
native SetUnitY takes unit whichUnit, real newY returns nothing

/**
@note This cancels the orders of the unit. If you want to move a unit without
canceling its orders use `SetUnitX`/`SetUnitY`.
*/
native SetUnitPosition takes unit whichUnit, real newX, real newY returns nothing

native SetUnitPositionLoc takes unit whichUnit, location whichLocation returns nothing

native SetUnitFacing takes unit whichUnit, real facingAngle returns nothing

native SetUnitFacingTimed takes unit whichUnit, real facingAngle, real duration returns nothing

native SetUnitMoveSpeed takes unit whichUnit, real newSpeed returns nothing

native SetUnitFlyHeight takes unit whichUnit, real newHeight, real rate returns nothing

native SetUnitTurnSpeed takes unit whichUnit, real newTurnSpeed returns nothing

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
native SetUnitPropWindow takes unit whichUnit, real newPropWindowAngle returns nothing

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
native SetUnitAcquireRange takes unit whichUnit, real newAcquireRange returns nothing

native SetUnitCreepGuard takes unit whichUnit, boolean creepGuard returns nothing



native GetUnitAcquireRange takes unit whichUnit returns real

native GetUnitTurnSpeed takes unit whichUnit returns real

/**
Returns a unit's propulsion window angle in radians.

@param whichUnit The function will return this unit's propulsion window angle.
*/
native GetUnitPropWindow takes unit whichUnit returns real

native GetUnitFlyHeight takes unit whichUnit returns real



native GetUnitDefaultAcquireRange takes unit whichUnit returns real

native GetUnitDefaultTurnSpeed takes unit whichUnit returns real

/**
Returns a unit's default propulsion window angle in degrees.

@param whichUnit
The unit of which to return the default prop window

@note This native is the odd case in the asymmetric prop window API, since the
other prop window natives use radians. Therefore, to reset a unit's prop window
you need the explicit conversion, i.e.
`SetUnitPropWindow(u, GetUnitDefaultPropWindow(u) * bj_DEGTORAD)`
*/
native GetUnitDefaultPropWindow takes unit whichUnit returns real

native GetUnitDefaultFlyHeight takes unit whichUnit returns real



native SetUnitOwner takes unit whichUnit, player whichPlayer, boolean changeColor returns nothing

native SetUnitColor takes unit whichUnit, playercolor whichColor returns nothing


/**
@bug Only takes scaleX into account and uses scaleX for all three dimensions.
@param scaleX This is actually the scale for *all* dimensions
@param scaleY This parameter is not taken into account
@param scaleZ This parameter is not taken into account
*/
native SetUnitScale takes unit whichUnit, real scaleX, real scaleY, real scaleZ returns nothing

native SetUnitTimeScale takes unit whichUnit, real timeScale returns nothing

native SetUnitBlendTime takes unit whichUnit, real blendTime returns nothing


/**
Sets the unit's color to the color defined by (red,green,blue,alpha).

@param whichUnit The unit the indicator will be applied to.
@param red An integer from 0-255 determining the amount of red color.
@param green An integer from 0-255 determining the amount of green color.
@param blue An integer from 0-255 determining the amount of blue color.
@param alpha An integer from 0-255 determining the transparency. A value of 0 is complete transparency while a value of 255 is complete opacity.
*/
native SetUnitVertexColor takes unit whichUnit, integer red, integer green, integer blue, integer alpha returns nothing



native QueueUnitAnimation takes unit whichUnit, string whichAnimation returns nothing

native SetUnitAnimation takes unit whichUnit, string whichAnimation returns nothing

native SetUnitAnimationByIndex takes unit whichUnit, integer whichAnimation returns nothing

native SetUnitAnimationWithRarity takes unit whichUnit, string whichAnimation, raritycontrol rarity returns nothing

native AddUnitAnimationProperties takes unit whichUnit, string animProperties, boolean add returns nothing



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

@note [How to instantly set a unit's facing](http://www.wc3c.net/showthread.php?t=105830)
*/
native SetUnitLookAt takes unit whichUnit, string whichBone, unit lookAtTarget, real offsetX, real offsetY, real offsetZ returns nothing

/**
Unlocks the bone oriented by `SetUnitLookAt`, allowing it to move in accordance
to the unit's regular animations.

@param whichUnit The unit that will have its bone unlocked.
*/
native ResetUnitLookAt takes unit whichUnit returns nothing



native SetUnitRescuable takes unit whichUnit, player byWhichPlayer, boolean flag returns nothing

native SetUnitRescueRange takes unit whichUnit, real range returns nothing


/**
Sets the hero's strength property. If the new strength property is less than current value, the hero will lose HP.

@note Hero cannot lose HP below 1.0, which means that removing X strength and then adding X strength back can result in healing.
*/
native SetHeroStr takes unit whichHero, integer newStr, boolean permanent returns nothing

native SetHeroAgi takes unit whichHero, integer newAgi, boolean permanent returns nothing

native SetHeroInt takes unit whichHero, integer newInt, boolean permanent returns nothing



native GetHeroStr takes unit whichHero, boolean includeBonuses returns integer

native GetHeroAgi takes unit whichHero, boolean includeBonuses returns integer

native GetHeroInt takes unit whichHero, boolean includeBonuses returns integer



native UnitStripHeroLevel takes unit whichHero, integer howManyLevels returns boolean



native GetHeroXP takes unit whichHero returns integer

native SetHeroXP takes unit whichHero, integer newXpVal, boolean showEyeCandy returns nothing



/**
Returns the units available skill points.
*/
native GetHeroSkillPoints takes unit whichHero returns integer

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
native UnitModifySkillPoints takes unit whichHero, integer skillPointDelta returns boolean



/**
Adds the input value of experience to the hero unit specified.

If the experience added exceeds the amount required for the hero to gain a level,
then it will force the unit to gain a level and the remaining experience will
spill over for the next level.

@bug Adding negative value to experience will decrease it
by the stated value, but won't lower the level even if the experience value
after deduction is lower than the lower bound of the experience required to get
the stated level.

@bug If the value will become lower than zero, the experience won't
be negative, instead of it it'll be equal
to `4294967296+(supposed_negative_experience_value)` which actually proves
that WarCraft III uses unsigned int type for storing experience points.

@param whichHero The hero unit to add experience to.

@param xpToAdd The amount of experience to add to the hero unit.

@param showEyeCandy If the boolean input is true, then the hero-level-gain
effect will be shown if the hero gains a level from the added experience.
*/
native AddHeroXP takes unit whichHero, integer xpToAdd, boolean showEyeCandy returns nothing

native SetHeroLevel takes unit whichHero, integer level, boolean showEyeCandy returns nothing

constant native GetHeroLevel takes unit whichHero returns integer

constant native GetUnitLevel takes unit whichUnit returns integer

/**
Returns the hero's "Proper Name", which is the name displayed above the level bar.

@note Will return 'null' on non-hero units.
*/
native GetHeroProperName takes unit whichHero returns string

native SuspendHeroXP takes unit whichHero, boolean flag returns nothing

native IsSuspendedXP takes unit whichHero returns boolean

native SelectHeroSkill takes unit whichHero, integer abilcode returns nothing

/**
Returns the level of the ability for the unit.
*/
native GetUnitAbilityLevel takes unit whichUnit, integer abilcode returns integer

/**
Decreases the level of a unit's ability by 1. The level will not go below 1.
Returns the new ability level.

@param whichUnit The unit with the ability.
@param abilcode The four digit rawcode representation of the ability.
*/
native DecUnitAbilityLevel takes unit whichUnit, integer abilcode returns integer

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
native IncUnitAbilityLevel takes unit whichUnit, integer abilcode returns integer

native SetUnitAbilityLevel takes unit whichUnit, integer abilcode, integer level returns integer

native ReviveHero takes unit whichHero, real x, real y, boolean doEyecandy returns boolean

native ReviveHeroLoc takes unit whichHero, location loc, boolean doEyecandy returns boolean

native SetUnitExploded takes unit whichUnit, boolean exploded returns nothing

/**
Renders a unit invulnerable/lifts that specific invulnerability.

@note The native seems to employ the `'Avul'` ability, which is defined in the
default AbilityData.slk.
If there is no `'Avul'` defined, this will crash the game.
*/
native SetUnitInvulnerable takes unit whichUnit, boolean flag returns nothing

/**
Pauses a unit. A paused unit has the following properties:

  * Buffs/effects are suspended
  * Orders are stored when paused and fired on unpause
  * The paused unit does not accept powerups. `UnitAddItem` returns true but
    the item is not picked up
*/
native PauseUnit takes unit whichUnit, boolean flag returns nothing

native IsUnitPaused takes unit whichHero returns boolean

native SetUnitPathing takes unit whichUnit, boolean flag returns nothing


/**
Clears all widget selections for all players.

@note Use `ClearSelectionForPlayer` to clear selection for only one player.
*/
native ClearSelection takes nothing returns nothing

native SelectUnit takes unit whichUnit, boolean flag returns nothing



native GetUnitPointValue takes unit whichUnit returns integer

native GetUnitPointValueByType takes integer unitType returns integer

//native SetUnitPointValueByType takes integer unitType, integer newPointValue returns nothing



native UnitAddItem takes unit whichUnit, item whichItem returns boolean

native UnitAddItemById takes unit whichUnit, integer itemId returns item

native UnitAddItemToSlotById takes unit whichUnit, integer itemId, integer itemSlot returns boolean

/**
The item is removed from the Hero and placed on the ground at the Hero's feed.
*/
native UnitRemoveItem takes unit whichUnit, item whichItem returns nothing

/**
If an item exists in the given slot, it is removed from the Hero and placed on
the ground at the Hero's feed
*/
native UnitRemoveItemFromSlot takes unit whichUnit, integer itemSlot returns item

native UnitHasItem takes unit whichUnit, item whichItem returns boolean

native UnitItemInSlot takes unit whichUnit, integer itemSlot returns item

native UnitInventorySize takes unit whichUnit returns integer



native UnitDropItemPoint takes unit whichUnit, item whichItem, real x, real y returns boolean

native UnitDropItemSlot takes unit whichUnit, item whichItem, integer slot returns boolean

native UnitDropItemTarget takes unit whichUnit, item whichItem, widget target returns boolean



native UnitUseItem takes unit whichUnit, item whichItem returns boolean

native UnitUseItemPoint takes unit whichUnit, item whichItem, real x, real y returns boolean

native UnitUseItemTarget takes unit whichUnit, item whichItem, widget target returns boolean



/**
@bug If the unit is loaded into a zeppelin this will not return the position
of the zeppelin but the last position of the unit before it was loaded into
the zeppelin.
*/
constant native GetUnitX takes unit whichUnit returns real

/**
@bug If the unit is loaded into a zeppelin this will not return the position
of the zeppelin but the last position of the unit before it was loaded into
the zeppelin.
*/
constant native GetUnitY takes unit whichUnit returns real

/**
@bug If the unit is loaded into a zeppelin this will not return the position
of the zeppelin but the last position of the unit before it was loaded into
the zeppelin.
*/
constant native GetUnitLoc takes unit whichUnit returns location

/**
Returns the units facing in degrees.
*/
constant native GetUnitFacing takes unit whichUnit returns real

constant native GetUnitMoveSpeed takes unit whichUnit returns real

constant native GetUnitDefaultMoveSpeed takes unit whichUnit returns real

constant native GetUnitState takes unit whichUnit, unitstate whichUnitState returns real

constant native GetOwningPlayer takes unit whichUnit returns player

constant native GetUnitTypeId takes unit whichUnit returns integer

constant native GetUnitRace takes unit whichUnit returns race

/**
@async
*/
constant native GetUnitName takes unit whichUnit returns string

constant native GetUnitFoodUsed takes unit whichUnit returns integer

constant native GetUnitFoodMade takes unit whichUnit returns integer

constant native GetFoodMade takes integer unitId returns integer

constant native GetFoodUsed takes integer unitId returns integer

native SetUnitUseFood takes unit whichUnit, boolean useFood returns nothing



constant native GetUnitRallyPoint takes unit whichUnit returns location

constant native GetUnitRallyUnit takes unit whichUnit returns unit

constant native GetUnitRallyDestructable takes unit whichUnit returns destructable



constant native IsUnitInGroup takes unit whichUnit, group whichGroup returns boolean

constant native IsUnitInForce takes unit whichUnit, force whichForce returns boolean

constant native IsUnitOwnedByPlayer takes unit whichUnit, player whichPlayer returns boolean

constant native IsUnitAlly takes unit whichUnit, player whichPlayer returns boolean

constant native IsUnitEnemy takes unit whichUnit, player whichPlayer returns boolean

constant native IsUnitVisible takes unit whichUnit, player whichPlayer returns boolean

constant native IsUnitDetected takes unit whichUnit, player whichPlayer returns boolean

constant native IsUnitInvisible takes unit whichUnit, player whichPlayer returns boolean

constant native IsUnitFogged takes unit whichUnit, player whichPlayer returns boolean

constant native IsUnitMasked takes unit whichUnit, player whichPlayer returns boolean

constant native IsUnitSelected takes unit whichUnit, player whichPlayer returns boolean

constant native IsUnitRace takes unit whichUnit, race whichRace returns boolean

/**
@note This native returns a boolean, which when typecasted to integer might
be greater than 1. It's probably implemented via a bitset.

@note In past patches this native bugged when used in conditionfuncs.
The fix back then was to compare with true (`==true`).
I cannot reproduce the faulty behaviour in patch 1.27 so this is only a note.
*/
constant native IsUnitType takes unit whichUnit, unittype whichUnitType returns boolean

/**
@note Useless. Use operator== instead.
@pure
*/
constant native IsUnit takes unit whichUnit, unit whichSpecifiedUnit returns boolean

constant native IsUnitInRange takes unit whichUnit, unit otherUnit, real distance returns boolean

constant native IsUnitInRangeXY takes unit whichUnit, real x, real y, real distance returns boolean

constant native IsUnitInRangeLoc takes unit whichUnit, location whichLocation, real distance returns boolean

/**
Returns `true` if `whichUnit` is hidden, for example by means of `ShowUnit`.
*/
constant native IsUnitHidden takes unit whichUnit returns boolean

constant native IsUnitIllusion takes unit whichUnit returns boolean



constant native IsUnitInTransport takes unit whichUnit, unit whichTransport returns boolean

constant native IsUnitLoaded takes unit whichUnit returns boolean



constant native IsHeroUnitId takes integer unitId returns boolean

constant native IsUnitIdType takes integer unitId, unittype whichUnitType returns boolean



native UnitShareVision takes unit whichUnit, player whichPlayer, boolean share returns nothing

native UnitSuspendDecay takes unit whichUnit, boolean suspend returns nothing

native UnitAddType takes unit whichUnit, unittype whichUnitType returns boolean

native UnitRemoveType takes unit whichUnit, unittype whichUnitType returns boolean



native UnitAddAbility takes unit whichUnit, integer abilityId returns boolean

/**
@bug Removing non-interrupt abilities like divine shile while they're being
cast (at the EVENT_PLAYER_UNIT_SPELL_EFFECT point), and while the caster is
moving, will cause the caster to become unresponsive to new commands until
they reach their ordered move point. 
*/
native UnitRemoveAbility takes unit whichUnit, integer abilityId returns boolean

/**
This native is used to keep abilities when morphing units
*/
native UnitMakeAbilityPermanent takes unit whichUnit, boolean permanent, integer abilityId returns boolean

native UnitRemoveBuffs takes unit whichUnit, boolean removePositive, boolean removeNegative returns nothing

native UnitRemoveBuffsEx takes unit whichUnit, boolean removePositive, boolean removeNegative, boolean magic, boolean physical, boolean timedLife, boolean aura, boolean autoDispel returns nothing

native UnitHasBuffsEx takes unit whichUnit, boolean removePositive, boolean removeNegative, boolean magic, boolean physical, boolean timedLife, boolean aura, boolean autoDispel returns boolean

native UnitCountBuffsEx takes unit whichUnit, boolean removePositive, boolean removeNegative, boolean magic, boolean physical, boolean timedLife, boolean aura, boolean autoDispel returns integer

native UnitAddSleep takes unit whichUnit, boolean add returns nothing

native UnitCanSleep takes unit whichUnit returns boolean

native UnitAddSleepPerm takes unit whichUnit, boolean add returns nothing

native UnitCanSleepPerm takes unit whichUnit returns boolean

native UnitIsSleeping takes unit whichUnit returns boolean

native UnitWakeUp takes unit whichUnit returns nothing

native UnitApplyTimedLife takes unit whichUnit, integer buffId, real duration returns nothing

native UnitIgnoreAlarm takes unit whichUnit, boolean flag returns boolean

native UnitIgnoreAlarmToggled takes unit whichUnit returns boolean

native UnitResetCooldown takes unit whichUnit returns nothing

native UnitSetConstructionProgress takes unit whichUnit, integer constructionPercentage returns nothing

native UnitSetUpgradeProgress takes unit whichUnit, integer upgradePercentage returns nothing

native UnitPauseTimedLife takes unit whichUnit, boolean flag returns nothing

native UnitSetUsesAltIcon takes unit whichUnit, boolean flag returns nothing



/**
@bug Has been known to cause crashes in battle.net
*/
native UnitDamagePoint takes unit whichUnit, real delay, real radius, real x, real y, real amount, boolean attack, boolean ranged, attacktype attackType, damagetype damageType, weapontype weaponType returns boolean

/**
Deals damage to target widget from a source unit.

@note For some insight about the different configurations of the different
types see [this post](http://www.wc3c.net/showpost.php?p=1030046&postcount=19).

@param whichUnit The source of the damage. To actual deal damage it should be
not `null`.

@param target The target being damaged.

@param amount How much damage is being dealt.

@param attack Consider the damage dealt as being an attack.

@param ranged Consider the damage dealt as being from a ranged source.

@param attackType
@param damageType
@param weaponType
*/
native UnitDamageTarget takes unit whichUnit, widget target, real amount, boolean attack, boolean ranged, attacktype attackType, damagetype damageType, weapontype weaponType returns boolean



native IssueImmediateOrder takes unit whichUnit, string order returns boolean

native IssueImmediateOrderById takes unit whichUnit, integer order returns boolean

native IssuePointOrder takes unit whichUnit, string order, real x, real y returns boolean

native IssuePointOrderLoc takes unit whichUnit, string order, location whichLocation returns boolean

native IssuePointOrderById takes unit whichUnit, integer order, real x, real y returns boolean

native IssuePointOrderByIdLoc takes unit whichUnit, integer order, location whichLocation returns boolean

native IssueTargetOrder takes unit whichUnit, string order, widget targetWidget returns boolean

native IssueTargetOrderById takes unit whichUnit, integer order, widget targetWidget returns boolean

native IssueInstantPointOrder takes unit whichUnit, string order, real x, real y, widget instantTargetWidget returns boolean

native IssueInstantPointOrderById takes unit whichUnit, integer order, real x, real y, widget instantTargetWidget returns boolean

native IssueInstantTargetOrder takes unit whichUnit, string order, widget targetWidget, widget instantTargetWidget returns boolean

native IssueInstantTargetOrderById takes unit whichUnit, integer order, widget targetWidget, widget instantTargetWidget returns boolean

native IssueBuildOrder takes unit whichPeon, string unitToBuild, real x, real y returns boolean

native IssueBuildOrderById takes unit whichPeon, integer unitId, real x, real y returns boolean



native IssueNeutralImmediateOrder takes player forWhichPlayer, unit neutralStructure, string unitToBuild returns boolean

native IssueNeutralImmediateOrderById takes player forWhichPlayer,unit neutralStructure, integer unitId returns boolean

native IssueNeutralPointOrder takes player forWhichPlayer,unit neutralStructure, string unitToBuild, real x, real y returns boolean

native IssueNeutralPointOrderById takes player forWhichPlayer,unit neutralStructure, integer unitId, real x, real y returns boolean

native IssueNeutralTargetOrder takes player forWhichPlayer,unit neutralStructure, string unitToBuild, widget target returns boolean

native IssueNeutralTargetOrderById takes player forWhichPlayer,unit neutralStructure, integer unitId, widget target returns boolean



native GetUnitCurrentOrder takes unit whichUnit returns integer


/**
Sets the amount of available gold of a gold mine. The amount can be negative, which is practically the same as 0.

@bug If the final value, after adding a negative amount, will be less than zero, then it
will display the correct negative amount, but mining won't yield any gold.
If peasant enters a mine with 0 gold, it's destroyed and he stops next to mine.
If peasant enters a mine with <0 gold, it's destroyed and he runs back to the castle.

@param whichUnit Change amount of this gold mine unit.

@param amount The new gold amount.

@note See: `AddResourceAmount`, `GetResourceAmount`
*/
native SetResourceAmount takes unit whichUnit, integer amount returns nothing


/**
Adds the amount of available gold to a gold mine. The amount can be negative, which is practically the same as 0.

@param whichUnit Add gold to this gold mine unit.

@param amount The amount of gold to add to the unit.

@note See `SetResourceAmount` for edge-case descriptions. Also: `SetResourceAmount`, `GetResourceAmount`
*/
native AddResourceAmount takes unit whichUnit, integer amount returns nothing


/**
Returns the amount of available gold in a gold mine. The amount can be negative, which is practically the same as 0.

@param whichUnit Add gold to this gold mine unit.

@note See `SetResourceAmount` for edge-case descriptions. Also: `SetResourceAmount`, `AddResourceAmount`
*/
native GetResourceAmount takes unit whichUnit returns integer

native WaygateGetDestinationX takes unit waygate returns real

native WaygateGetDestinationY takes unit waygate returns real

native WaygateSetDestination takes unit waygate, real x, real y returns nothing

native WaygateActivate takes unit waygate, boolean activate returns nothing

native WaygateIsActive takes unit waygate returns boolean


/**
Adds an item of the type itemId with current stock of currentStock and max stock
of stockMax to all shops in game.

@note Some issues with default Blizzard initialization and that function were met.
See <http://www.hiveworkshop.com/forums/l-715/a-251815/> for details.

@note Adding an item which already is in stock for a building will replace it
and refresh the interval and stock count.



@param itemId The item to add to the stock.

@param currentStock Determines the amount of that item in stock upon being added
to the buildings.

@param stockMax The item will grow in stock count up to the value of stockMax.
The rate at which the item grows in stock is determined by its stock replenish
interval, which can be modified in the object editor.
*/
native AddItemToAllStock takes integer itemId, integer currentStock, integer stockMax returns nothing

/**
Adds an item of the type itemId with current stock of currentStock and max stock
of stockMax to the specific shop whichUnit.

@note Some issues with default Blizzard initialization and that function were met.
See <http://www.hiveworkshop.com/forums/l-715/a-251815/> for details.
*/
native AddItemToStock takes unit whichUnit, integer itemId, integer currentStock, integer stockMax returns nothing

native AddUnitToAllStock takes integer unitId, integer currentStock, integer stockMax returns nothing

native AddUnitToStock takes unit whichUnit, integer unitId, integer currentStock, integer stockMax returns nothing



native RemoveItemFromAllStock takes integer itemId returns nothing

native RemoveItemFromStock takes unit whichUnit, integer itemId returns nothing

native RemoveUnitFromAllStock takes integer unitId returns nothing

native RemoveUnitFromStock takes unit whichUnit, integer unitId returns nothing



native SetAllItemTypeSlots takes integer slots returns nothing

native SetAllUnitTypeSlots takes integer slots returns nothing

native SetItemTypeSlots takes unit whichUnit, integer slots returns nothing

native SetUnitTypeSlots takes unit whichUnit, integer slots returns nothing



native GetUnitUserData takes unit whichUnit returns integer

/**
Sets a single custom integer for a unit.

@note This value is not used by any standard mechanisms in Warcraft III nor
in the blizzard.j, so it is free to be harnessed.
Besides `GetHandleId`, this is an excellent possibility to assign a unique
integer id to a unit, which can serve as an index in other data structures.
*/
native SetUnitUserData takes unit whichUnit, integer data returns nothing

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
Get the current unit armor of a specific unit (real value).

*Returns TOTAL amount of armor a unit has, including bonus (green) armor from  auras, buffs, agility and items. If you need just base or bonus armor, you need to calculate base armor yourself (for heroes: -2 + agility (excluding bonuses) * 0.3). Agility bonus also counts as bonus armor, e.g. +1 agility will be displayed as + 0.3 armor with default gameplay constants.*

@patch 1.29
*/
native BlzGetUnitArmor                             takes unit whichUnit returns real

/**
Changes(set) the unit armor of a specific unit, you pass it a real value, can be negative

*Changes TOTAL amount of armor a unit has. If unit has a bonus (green) armor from an aura or item, base armor will be reduced to achieve total amount of armor you specified. E.g. a unit has 1+3 armor, if you set armor to 1.00, unit’s armor will be changed to -2+3*

@patch 1.29
*/
native BlzSetUnitArmor                             takes unit whichUnit, real armorAmount returns nothing

/**
Hides or unhides an ability for a unit.

@param whichUnit
Unit to apply this to

@param abilId
Rawcode of ability

@param flag
isHidden: true to hide, false to show

@bug The boolean flag doesn't work as expected, it acts more like an integer counter: https://www.hiveworkshop.com/threads/blzunithideability-and-blzunitdisableability-dont-work.312477/

@patch 1.29
*/
native BlzUnitHideAbility                          takes unit whichUnit, integer abilId, boolean flag returns nothing

/**
Enables/disables and hides/unhides an ability for a unit. A visible disabled ability is shown as deactivated, an invisible ability disappears from the grid.

@param whichUnit
Unit to apply this to

@param abilId
Rawcode of ability

@param flag
isDisabled: true to disable, false to enable ability

@param hideUI
isHidden: true to hide, false to show

@bug (1.32.10 confirmed) The flags doesn't work as expected, act more like an integer counter: https://www.hiveworkshop.com/threads/blzunithideability-and-blzunitdisableability-dont-work.312477/

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
Changes(set) an ability’s cooldown at runtime for a specific unit.

@param whichUnit Target unit (handle)
@param abilId Rawcode of ability
@param level Ability level
@param cooldown New cooldown

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

@patch 1.29
*/
native BlzGetUnitAbilityCooldownRemaining          takes unit whichUnit, integer abilId returns real

/**
Reduces the current ability cooldown of a specific ability to 0.

@patch 1.29
*/
native BlzEndUnitAbilityCooldown                   takes unit whichUnit, integer abilCode returns nothing

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
Get a specific unit’s Z coordinate (altitude) (Cartesian System), Z is desync prone, this version might cause desyncs, but (unconfirmed) should be faster, hence why both `BlzGetUnitZ` and `BlzGetLocalUnitZ` exist (In case that you are doing a campaign, or something single player, you might decide to use this one instead of `BlzGetUnitZ`).

@note Terrain height is not synced between clients in multiplayer

@async
@patch 1.29
*/
native BlzGetLocalUnitZ                            takes unit whichUnit returns real   

/**
@note Returns the same result as `BlzGetLocalUnitZ`.
@async
@patch 1.30
*/
native BlzGetUnitZ                                 takes unit whichUnit returns real


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

The getter equivalent of the native above does not work too (returns 0)

@patch 1.31
*/
native BlzSetUnitWeaponRealField                   takes unit whichUnit, unitweaponrealfield whichField, integer index, real value returns boolean

/**
@patch 1.31
*/
native BlzSetUnitWeaponStringField                 takes unit whichUnit, unitweaponstringfield whichField, integer index, string value returns boolean

/**
This does not update `IsUnitPaused` and keeps the command card visible. Otherwise identical to `PauseUnit()`.

@patch 1.31
*/
native BlzPauseUnitEx                              takes unit whichUnit, boolean flag returns nothing

/**
@patch 1.31
*/
native BlzGetUnitAbility                           takes unit whichUnit, integer abilId returns ability

/**
Returns a handle to specific unit's ability instance.

@note Last added ability is at index 0, older abilities are pushed up

@patch 1.31
*/
native BlzGetUnitAbilityByIndex                    takes unit whichUnit, integer index returns ability


/**
@patch 1.32
*/
native BlzStartUnitAbilityCooldown                 takes unit whichUnit, integer abilCode, real cooldown returns nothing


/**
@patch 1.32
*/
native BlzGetEventIsAttack                         takes nothing returns boolean

/**
@patch 1.32
*/
native BlzSetUnitFacingEx                          takes unit whichUnit, real facingAngle returns nothing
