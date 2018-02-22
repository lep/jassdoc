type mousebuttontype    extends     handle


native GetTriggerPlayerMouseX                   takes nothing returns real
native GetTriggerPlayerMouseY                   takes nothing returns real
native GetTriggerPlayerMousePosition            takes nothing returns location
native GetTriggerPlayerMouseButton              takes nothing returns mousebuttontype
native SetAbilityTooltip                        takes integer abilCode, string tooltip, integer level returns nothing
native SetAbilityOnTooltip                      takes integer abilCode, string tooltip, integer level returns nothing
native SetAbilityExtendedTooltip                takes integer abilCode, string ExtendedTooltip, integer level returns nothing
native SetAbilityOnExtendedTooltip              takes integer abilCode, string ExtendedTooltip, integer level returns nothing
native SetAbilityResearchTooltip                takes integer abilCode, string researchTooltip, integer level returns nothing
native SetAbilityResearchExtendedTooltip        takes integer abilCode, string researchExtendedTooltip, integer level returns nothing
native GetAbilityTooltip                        takes integer abilCode, integer level returns string
native GetAbilityOnTooltip                      takes integer abilCode, integer level returns string
native GetAbilityExtendedTooltip                takes integer abilCode, integer level returns string
native GetAbilityOnExtendedTooltip              takes integer abilCode, integer level returns string
native GetAbilityResearchTooltip                takes integer abilCode, integer level returns string
native GetAbilityResearchExtendedTooltip        takes integer abilCode, integer level returns string
native SetAbilityIcon                           takes integer abilCode, string iconPath, integer level returns nothing
native GetAbilityIcon                           takes integer abilCode, integer level returns string
native SetAbilityOnIcon                         takes integer abilCode, string iconPath returns nothing

/**
@note OnIcon is the icon for these auto-castable ablities when they're turned on
*/
native GetAbilityOnIcon                         takes integer abilCode returns string

/**
Icon Position
*/
native GetAbilityPosX                           takes integer abilCode returns integer

/**
Gets the Abilitie Icons Y Position
*/
native GetAbilityPosY                           takes integer abilCode returns integer

/**
Sets the Abilitie Icons X Position
*/
native SetAbilityPosX                           takes integer abilCode, integer x returns nothing

/**
Sets the Abilitie Icons Y Position
*/
native SetAbilityPosY                           takes integer abilCode, integer y returns nothing
native GetAbilityOnPosX                         takes integer abilCode returns integer
native GetAbilityOnPosY                         takes integer abilCode returns integer
native SetAbilityOnPosX                         takes integer abilCode, integer x returns nothing
native SetAbilityOnPosY                         takes integer abilCode, integer y returns nothing
native GetUnitMaxHP                             takes unit whichUnit returns integer
native SetUnitMaxHP                             takes unit whichUnit, integer hp returns nothing
native GetUnitMaxMana                           takes unit whichUnit returns integer
native SetUnitMaxMana                           takes unit whichUnit, integer mana returns nothing

/**
Remove Hero Ability
*/
native DeleteHeroAbility                        takes unit whichUnit, integer abilCode returns nothing
native SetItemName                              takes item whichItem, string name returns nothing
native SetItemDescription                       takes item whichItem, string name returns nothing
native GetItemDescription                       takes item whichItem returns string
native SetItemTooltip                           takes item whichItem, string name returns nothing
native GetItemTooltip                           takes item whichItem returns string
native SetItemExtendedTooltip                   takes item whichItem, string name returns nothing
native GetItemExtendedTooltip                   takes item whichItem returns string
native SetItemIconPath                          takes item whichItem, string name returns nothing
native GetItemIconPath                          takes item whichItem returns string
native SetUnitName                              takes unit whichUnit, string name returns nothing
native SetUnitNameAll                           takes unit whichUnit, string name returns nothing
native SetHeroProperName                        takes unit whichUnit, string name returns nothing
native GetUnitBaseDamage                        takes unit whichUnit, integer weaponIndex returns integer
native SetUnitBaseDamage                        takes unit whichUnit, integer baseDamage, integer weaponIndex returns nothing
native GetUnitDiceNumber                        takes unit whichUnit, integer weaponIndex returns integer
native SetUnitDiceNumber                        takes unit whichUnit, integer diceNumber, integer weaponIndex returns nothing
native GetUnitDiceSides                         takes unit whichUnit, integer weaponIndex returns integer
native SetUnitDiceSides                         takes unit whichUnit, integer diceSides, integer weaponIndex returns nothing
native GetUnitAttackCooldown                    takes unit whichUnit, integer weaponIndex returns real

/**
Set all future attack cooldown for this specific unit
*/
native SetUnitAttackCooldown                    takes unit whichUnit, real cooldown, integer weaponIndex returns nothing
native SetSpecialEffectColorByPlayer            takes effect whichEffect, player whichPlayer returns nothing
native SetSpecialEffectColor                    takes effect whichEffect, integer r, integer g, integer b returns nothing
native SetSpecialEffectAlpha                    takes effect whichEffect, integer alpha returns nothing
native SetSpecialEffectScale                    takes effect whichEffect, real scale returns nothing
native SetSpecialEffectPosition                 takes effect whichEffect, real x, real y, real z returns nothing
native SetSpecialEffectHeight                   takes effect whichEffect, real height returns nothing
native SetSpecialEffectTimeScale                takes effect whichEffect, real timeScale returns nothing

/**
@note You will still need to destroy the effect.
*/
native SetSpecialEffectTime                     takes effect whichEffect, real time returns nothing
native SetSpecialEffectOrientation              takes effect whichEffect, real yaw, real pitch, real roll returns nothing
native SetSpecialEffectYaw                      takes effect whichEffect, real yaw returns nothing
native SetSpecialEffectPitch                    takes effect whichEffect, real pitch returns nothing
native SetSpecialEffectRoll                     takes effect whichEffect, real roll returns nothing
native PlaySpecialEffect                        takes effect whichEffect, integer anim returns nothing
native PlaySpecialEffectWithTimeScale           takes effect whichEffect, integer anim, real timeScale returns nothing
/**
Opposite to `IncPlayerTechResearched`
*/
constant native DecPlayerTechResearched         takes player whichPlayer, integer techid, integer levels returns nothing

/**
@event EVENT_UNIT_DAMAGED
*/
native SetEventDamage                           takes real damage returns nothing
native AutomationTestStart takes string testName returns nothing
native AutomationTestEnd takes string testName returns nothing
native SetSpecialEffectX                        takes effect whichEffect, real x returns nothing
native SetSpecialEffectY                        takes effect whichEffect, real y returns nothing
native SetSpecialEffectZ                        takes effect whichEffect, real z returns nothing
native SetSpecialEffectPositionLoc              takes effect whichEffect, location loc returns nothing
native GetLocalSpecialEffectX                   takes effect whichEffect returns real
native GetLocalSpecialEffectY                   takes effect whichEffect returns real
native GetLocalSpecialEffectZ                   takes effect whichEffect returns real
native GetUnitArmor                             takes unit whichUnit returns real
native SetUnitArmor                             takes unit whichUnit, real armorAmount returns nothing
native UnitHideAbility                          takes unit whichUnit, integer abilId, boolean flag returns nothing
native UnitDisableAbility                       takes unit whichUnit, integer abilId, boolean flag, boolean hideUI returns nothing
native UnitCancelTimedLife                      takes unit whichUnit returns nothing
native IsUnitSelectable                         takes unit whichUnit returns boolean
native IsUnitInvulnerable                       takes unit whichUnit returns boolean
native UnitInterruptAttack                      takes unit whichUnit returns nothing
native GetUnitCollisionSize                     takes unit whichUnit returns real
native GetAbilityManaCost                       takes integer abilId, integer level returns integer
native GetAbilityCooldown                       takes integer abilId, integer level returns real

/**
Set ability cooldown for this specific unit
*/
native SetUnitAbilityCooldown                   takes unit whichUnit, integer abilId, integer level, real cooldown returns nothing
native GetUnitAbilityCooldown                   takes unit whichUnit, integer abilId, integer level returns real
native GetUnitAbilityCooldownRemaining          takes unit whichUnit, integer abilId returns real

/**
Finish cooldown for this ability for this unit
*/
native EndUnitAbilityCooldown                   takes unit whichUnit, integer abilCode returns nothing
native GetUnitAbilityManaCost                   takes unit whichUnit, integer abilId, integer level returns integer
native GetLocalUnitZ                            takes unit whichUnit returns real

/**
@pure
*/
constant native ConvertMouseButtonType      takes integer i returns mousebuttontype

/**
@pure
*/
constant native GetBJMaxPlayers             takes nothing returns integer

/**
@pure
*/
constant native GetBJPlayerNeutralVictim    takes nothing returns integer

/**
@pure
*/
constant native GetBJPlayerNeutralExtra     takes nothing returns integer

/**
@pure
*/
constant native GetBJMaxPlayerSlots         takes nothing returns integer

/**
@pure
*/
constant native GetPlayerNeutralPassive     takes nothing returns integer

/**
@pure
*/
constant native GetPlayerNeutralAggressive  takes nothing returns integer

globals
    constant integer            PLAYER_NEUTRAL_PASSIVE          = GetPlayerNeutralPassive()
    constant integer            PLAYER_NEUTRAL_AGGRESSIVE       = GetPlayerNeutralAggressive()
    constant mousebuttontype    MOUSE_BUTTON_TYPE_LEFT          = ConvertMouseButtonType(1)
    constant mousebuttontype    MOUSE_BUTTON_TYPE_MIDDLE        = ConvertMouseButtonType(2)
    constant mousebuttontype    MOUSE_BUTTON_TYPE_RIGHT         = ConvertMouseButtonType(3)
    constant playerevent        EVENT_PLAYER_MOUSE_DOWN                 = ConvertPlayerEvent(269)
    constant playerevent        EVENT_PLAYER_MOUSE_UP                   = ConvertPlayerEvent(270)
    constant playerevent        EVENT_PLAYER_MOUSE_MOVE                 = ConvertPlayerEvent(271)
    constant playerunitevent    EVENT_PLAYER_UNIT_SELL                  = ConvertPlayerUnitEvent(272)
    constant playerunitevent    EVENT_PLAYER_UNIT_CHANGE_OWNER          = ConvertPlayerUnitEvent(273)
    constant playerunitevent    EVENT_PLAYER_UNIT_SELL_ITEM             = ConvertPlayerUnitEvent(274)
    constant playerunitevent    EVENT_PLAYER_UNIT_SPELL_CHANNEL         = ConvertPlayerUnitEvent(275)
    constant playerunitevent    EVENT_PLAYER_UNIT_SPELL_CAST            = ConvertPlayerUnitEvent(276)
    constant playerunitevent    EVENT_PLAYER_UNIT_SPELL_EFFECT          = ConvertPlayerUnitEvent(277)
    constant playerunitevent    EVENT_PLAYER_UNIT_SPELL_FINISH          = ConvertPlayerUnitEvent(278)
    constant playerunitevent    EVENT_PLAYER_UNIT_SPELL_ENDCAST         = ConvertPlayerUnitEvent(279)
    constant playerunitevent    EVENT_PLAYER_UNIT_PAWN_ITEM             = ConvertPlayerUnitEvent(280)
    constant unitevent          EVENT_UNIT_SELL                         = ConvertUnitEvent(289)
    constant unitevent          EVENT_UNIT_CHANGE_OWNER                 = ConvertUnitEvent(290)
    constant unitevent          EVENT_UNIT_SELL_ITEM                    = ConvertUnitEvent(291)
    constant unitevent          EVENT_UNIT_SPELL_CHANNEL                = ConvertUnitEvent(292)
    constant unitevent          EVENT_UNIT_SPELL_CAST                   = ConvertUnitEvent(293)
    constant unitevent          EVENT_UNIT_SPELL_EFFECT                 = ConvertUnitEvent(294)
    constant unitevent          EVENT_UNIT_SPELL_FINISH                 = ConvertUnitEvent(295)
    constant unitevent          EVENT_UNIT_SPELL_ENDCAST                = ConvertUnitEvent(296)
    constant unitevent          EVENT_UNIT_PAWN_ITEM                    = ConvertUnitEvent(297)
endglobals

// JAPI Test Functions

// native GetUnitMovementType                      takes unit whichUnit returns integer
// native SetUnitMovementType                      takes unit whichUnit, integer movementType returns nothing
// native GetUnitArmorType                         takes unit whichUnit returns integer
// native SetHeroStatEx                            takes unit whichHero, integer whichStat, integer statValue, boolean permanent returns nothing
// native GetHeroPrimaryStat                       takes unit whichHero returns integer
// native GetHeroPrimaryStatById                   takes unit whichHero returns integer
// native GetHeroStat                              takes unit whichHero, integer whichStat, boolean includeBonuses returns integer