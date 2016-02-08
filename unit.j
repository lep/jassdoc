// Unit API

// Facing arguments are specified in degrees

/**
@param face Unit facing in degrees
*/
native CreateUnit takes player id, integer unitid, real x, real y, real face returns unit

/**
@param face Unit facing in degrees
*/
native CreateUnitByName takes player whichPlayer, string unitname, real x, real y, real face returns unit

/**
@param face Unit facing in degrees
*/
native CreateUnitAtLoc takes player id, integer unitid, location whichLocation, real face returns unit

/**
@param face Unit facing in degrees
*/
native CreateUnitAtLocByName takes player id, string unitname, location whichLocation, real face returns unit

/**
@param face Unit facing in degrees
*/
native CreateCorpse takes player whichPlayer, integer unitid, real x, real y, real face returns unit



native KillUnit takes unit whichUnit returns nothing

native RemoveUnit takes unit whichUnit returns nothing

native ShowUnit takes unit whichUnit, boolean show returns nothing



native SetUnitState takes unit whichUnit, unitstate whichUnitState, real newVal returns nothing

native SetUnitX takes unit whichUnit, real newX returns nothing

native SetUnitY takes unit whichUnit, real newY returns nothing

native SetUnitPosition takes unit whichUnit, real newX, real newY returns nothing

native SetUnitPositionLoc takes unit whichUnit, location whichLocation returns nothing

native SetUnitFacing takes unit whichUnit, real facingAngle returns nothing

native SetUnitFacingTimed takes unit whichUnit, real facingAngle, real duration returns nothing

native SetUnitMoveSpeed takes unit whichUnit, real newSpeed returns nothing

native SetUnitFlyHeight takes unit whichUnit, real newHeight, real rate returns nothing

native SetUnitTurnSpeed takes unit whichUnit, real newTurnSpeed returns nothing

native SetUnitPropWindow takes unit whichUnit, real newPropWindowAngle returns nothing

native SetUnitAcquireRange takes unit whichUnit, real newAcquireRange returns nothing

native SetUnitCreepGuard takes unit whichUnit, boolean creepGuard returns nothing



native GetUnitAcquireRange takes unit whichUnit returns real

native GetUnitTurnSpeed takes unit whichUnit returns real

native GetUnitPropWindow takes unit whichUnit returns real

native GetUnitFlyHeight takes unit whichUnit returns real



native GetUnitDefaultAcquireRange takes unit whichUnit returns real

native GetUnitDefaultTurnSpeed takes unit whichUnit returns real

native GetUnitDefaultPropWindow takes unit whichUnit returns real

native GetUnitDefaultFlyHeight takes unit whichUnit returns real



native SetUnitOwner takes unit whichUnit, player whichPlayer, boolean changeColor returns nothing

native SetUnitColor takes unit whichUnit, playercolor whichColor returns nothing


/**
@bug Only takes scaleX int account and uses scaleX for all three dimensions.
*/
native SetUnitScale takes unit whichUnit, real scaleX, real scaleY, real scaleZ returns nothing

native SetUnitTimeScale takes unit whichUnit, real timeScale returns nothing

native SetUnitBlendTime takes unit whichUnit, real blendTime returns nothing

native SetUnitVertexColor takes unit whichUnit, integer red, integer green, integer blue, integer alpha returns nothing



native QueueUnitAnimation takes unit whichUnit, string whichAnimation returns nothing

native SetUnitAnimation takes unit whichUnit, string whichAnimation returns nothing

native SetUnitAnimationByIndex takes unit whichUnit, integer whichAnimation returns nothing

native SetUnitAnimationWithRarity takes unit whichUnit, string whichAnimation, raritycontrol rarity returns nothing

native AddUnitAnimationProperties takes unit whichUnit, string animProperties, boolean add returns nothing



native SetUnitLookAt takes unit whichUnit, string whichBone, unit lookAtTarget, real offsetX, real offsetY, real offsetZ returns nothing

native ResetUnitLookAt takes unit whichUnit returns nothing



native SetUnitRescuable takes unit whichUnit, player byWhichPlayer, boolean flag returns nothing

native SetUnitRescueRange takes unit whichUnit, real range returns nothing



native SetHeroStr takes unit whichHero, integer newStr, boolean permanent returns nothing

native SetHeroAgi takes unit whichHero, integer newAgi, boolean permanent returns nothing

native SetHeroInt takes unit whichHero, integer newInt, boolean permanent returns nothing



native GetHeroStr takes unit whichHero, boolean includeBonuses returns integer

native GetHeroAgi takes unit whichHero, boolean includeBonuses returns integer

native GetHeroInt takes unit whichHero, boolean includeBonuses returns integer



native UnitStripHeroLevel takes unit whichHero, integer howManyLevels returns boolean



native GetHeroXP takes unit whichHero returns integer

native SetHeroXP takes unit whichHero, integer newXpVal, boolean showEyeCandy returns nothing



native GetHeroSkillPoints takes unit whichHero returns integer

native UnitModifySkillPoints takes unit whichHero, integer skillPointDelta returns boolean



/**
This function adds xpToAdd experience to the given hero (whichHero).

@bug Adding negative value to experience will decrease it
by the stated value, but won't lower the level even if the experience value
after deduction is lower than the lower bound of the experience required to get
the stated level.

@bug If the value will become lower than zero, the experience won't
be negative, instead of it it'll be equal
to 4294967296+(supposed_negative_experience_value) which actually proves 
that WarCraft III uses unsigned int type for storing experience points.
*/
native AddHeroXP takes unit whichHero, integer xpToAdd, boolean showEyeCandy returns nothing

native SetHeroLevel takes unit whichHero, integer level, boolean showEyeCandy returns nothing

constant native GetHeroLevel takes unit whichHero returns integer

constant native GetUnitLevel takes unit whichUnit returns integer

native GetHeroProperName takes unit whichHero returns string

native SuspendHeroXP takes unit whichHero, boolean flag returns nothing

native IsSuspendedXP takes unit whichHero returns boolean

native SelectHeroSkill takes unit whichHero, integer abilcode returns nothing

native GetUnitAbilityLevel takes unit whichUnit, integer abilcode returns integer

native DecUnitAbilityLevel takes unit whichUnit, integer abilcode returns integer

native IncUnitAbilityLevel takes unit whichUnit, integer abilcode returns integer

native SetUnitAbilityLevel takes unit whichUnit, integer abilcode, integer level returns integer

native ReviveHero takes unit whichHero, real x, real y, boolean doEyecandy returns boolean

native ReviveHeroLoc takes unit whichHero, location loc, boolean doEyecandy returns boolean

native SetUnitExploded takes unit whichUnit, boolean exploded returns nothing

native SetUnitInvulnerable takes unit whichUnit, boolean flag returns nothing

native PauseUnit takes unit whichUnit, boolean flag returns nothing

native IsUnitPaused takes unit whichHero returns boolean

native SetUnitPathing takes unit whichUnit, boolean flag returns nothing



native ClearSelection takes nothing returns nothing

native SelectUnit takes unit whichUnit, boolean flag returns nothing



native GetUnitPointValue takes unit whichUnit returns integer

native GetUnitPointValueByType takes integer unitType returns integer

//native SetUnitPointValueByType takes integer unitType, integer newPointValue returns nothing



native UnitAddItem takes unit whichUnit, item whichItem returns boolean

native UnitAddItemById takes unit whichUnit, integer itemId returns item

native UnitAddItemToSlotById takes unit whichUnit, integer itemId, integer itemSlot returns boolean

native UnitRemoveItem takes unit whichUnit, item whichItem returns nothing

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



constant native GetUnitX takes unit whichUnit returns real

constant native GetUnitY takes unit whichUnit returns real

constant native GetUnitLoc takes unit whichUnit returns location

constant native GetUnitFacing takes unit whichUnit returns real

constant native GetUnitMoveSpeed takes unit whichUnit returns real

constant native GetUnitDefaultMoveSpeed takes unit whichUnit returns real

constant native GetUnitState takes unit whichUnit, unitstate whichUnitState returns real

constant native GetOwningPlayer takes unit whichUnit returns player

constant native GetUnitTypeId takes unit whichUnit returns integer

constant native GetUnitRace takes unit whichUnit returns race

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

constant native IsUnitType takes unit whichUnit, unittype whichUnitType returns boolean

/**
@note Useless. Use operator== instead.
*/
constant native IsUnit takes unit whichUnit, unit whichSpecifiedUnit returns boolean

constant native IsUnitInRange takes unit whichUnit, unit otherUnit, real distance returns boolean

constant native IsUnitInRangeXY takes unit whichUnit, real x, real y, real distance returns boolean

constant native IsUnitInRangeLoc takes unit whichUnit, location whichLocation, real distance returns boolean

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

native UnitRemoveAbility takes unit whichUnit, integer abilityId returns boolean

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



native SetResourceAmount takes unit whichUnit, integer amount returns nothing

/**
Adds the amount more gold to the whichUnit gold mine.

@bug If the value after adding negative amount will be less than zero, then it
will display negative resource amount, but if some peasant or peon will try to
gather resources from such a mine, he will bring back 0 gold and the mine will
be auto-destroyed.
*/
native AddResourceAmount takes unit whichUnit, integer amount returns nothing

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

native SetUnitUserData takes unit whichUnit, integer data returns nothing
