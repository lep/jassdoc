
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
Supports Unit/Item/Ability/Tech Codes

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
Supports Unit/Item/Ability/Tech Codes

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
Supports Unit/Item/Ability/Tech Codes

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
As of the 1.31 PTR while you can specify the position of abilities such as “Build” directly in the object editor, you cannot do it with this native.

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
Requires an ability ID and the ability level and returns the ability’s (at the level passed) mana cost. *Since 1.31: use Level 0 to read manacosts from Level 1.*

@patch 1.29
*/
native BlzGetAbilityManaCost                       takes integer abilId, integer level returns integer

/**
Requires an ability ID and the ability level and returns the ability’s (at the level passed) cooldown. *Since 1.31: use Level 0 to read cooldown from Level 1.*

@patch 1.29
*/
native BlzGetAbilityCooldown                       takes integer abilId, integer level returns real


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
@bug Should not be used (crash): Use `BlzGetAbilityIntegerLevelField`
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
@bug Should not be used (crash): Use `BlzSetAbilityIntegerLevelField`
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
