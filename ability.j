
/**
@patch 1.29
*/
native BlzSetAbilityTooltip                        takes integer abilCode, string tooltip, integer level returns nothing

/**
@patch 1.29
*/
native BlzSetAbilityActivatedTooltip               takes integer abilCode, string tooltip, integer level returns nothing

/**
@patch 1.29
*/
native BlzSetAbilityExtendedTooltip                takes integer abilCode, string extendedTooltip, integer level returns nothing

/**
@patch 1.29
*/
native BlzSetAbilityActivatedExtendedTooltip       takes integer abilCode, string extendedTooltip, integer level returns nothing

/**
@patch 1.29
*/
native BlzSetAbilityResearchTooltip                takes integer abilCode, string researchTooltip, integer level returns nothing

/**
@patch 1.29
*/
native BlzSetAbilityResearchExtendedTooltip        takes integer abilCode, string researchExtendedTooltip, integer level returns nothing

/**
@patch 1.29
*/
native BlzGetAbilityTooltip                        takes integer abilCode, integer level returns string

/**
@patch 1.29
*/
native BlzGetAbilityActivatedTooltip               takes integer abilCode, integer level returns string

/**
@patch 1.29
*/
native BlzGetAbilityExtendedTooltip                takes integer abilCode, integer level returns string

/**
@patch 1.29
*/
native BlzGetAbilityActivatedExtendedTooltip       takes integer abilCode, integer level returns string

/**
@patch 1.29
*/
native BlzGetAbilityResearchTooltip                takes integer abilCode, integer level returns string

/**
@patch 1.29
*/
native BlzGetAbilityResearchExtendedTooltip        takes integer abilCode, integer level returns string

/**
@patch 1.29
*/
native BlzSetAbilityIcon                           takes integer abilCode, string iconPath returns nothing

/**
@patch 1.29
*/
native BlzGetAbilityIcon                           takes integer abilCode returns string

/**
@patch 1.29
*/
native BlzSetAbilityActivatedIcon                  takes integer abilCode, string iconPath returns nothing

/**
@patch 1.29
*/
native BlzGetAbilityActivatedIcon                  takes integer abilCode returns string

/**
@patch 1.29
*/
native BlzGetAbilityPosX                           takes integer abilCode returns integer
/**
@patch 1.29
*/
native BlzGetAbilityPosY                           takes integer abilCode returns integer
/**
@patch 1.29
*/
native BlzSetAbilityPosX                           takes integer abilCode, integer x returns nothing
/**
@patch 1.29
*/
native BlzSetAbilityPosY                           takes integer abilCode, integer y returns nothing
/**
@patch 1.29
*/
native BlzGetAbilityActivatedPosX                  takes integer abilCode returns integer
/**
@patch 1.29
*/
native BlzGetAbilityActivatedPosY                  takes integer abilCode returns integer
/**
@patch 1.29
*/
native BlzSetAbilityActivatedPosX                  takes integer abilCode, integer x returns nothing
/**
@patch 1.29
*/
native BlzSetAbilityActivatedPosY                  takes integer abilCode, integer y returns nothing
/**
@patch 1.29
*/
native BlzGetAbilityManaCost                       takes integer abilId, integer level returns integer
/**
@patch 1.29
*/
native BlzGetAbilityCooldown                       takes integer abilId, integer level returns real


// Intanced Object Operations
// Ability
native BlzGetAbilityBooleanField                   takes ability whichAbility, abilitybooleanfield whichField returns boolean
native BlzGetAbilityIntegerField                   takes ability whichAbility, abilityintegerfield whichField returns integer
native BlzGetAbilityRealField                      takes ability whichAbility, abilityrealfield whichField returns real
native BlzGetAbilityStringField                    takes ability whichAbility, abilitystringfield whichField returns string
native BlzGetAbilityBooleanLevelField              takes ability whichAbility, abilitybooleanlevelfield whichField, integer level returns boolean
native BlzGetAbilityIntegerLevelField              takes ability whichAbility, abilityintegerlevelfield whichField, integer level returns integer
native BlzGetAbilityRealLevelField                 takes ability whichAbility, abilityreallevelfield whichField, integer level returns real
native BlzGetAbilityStringLevelField               takes ability whichAbility, abilitystringlevelfield whichField, integer level returns string
native BlzGetAbilityBooleanLevelArrayField         takes ability whichAbility, abilitybooleanlevelarrayfield whichField, integer level, integer index returns boolean
native BlzGetAbilityIntegerLevelArrayField         takes ability whichAbility, abilityintegerlevelarrayfield whichField, integer level, integer index returns integer
native BlzGetAbilityRealLevelArrayField            takes ability whichAbility, abilityreallevelarrayfield whichField, integer level, integer index returns real
native BlzGetAbilityStringLevelArrayField          takes ability whichAbility, abilitystringlevelarrayfield whichField, integer level, integer index returns string
native BlzSetAbilityBooleanField                   takes ability whichAbility, abilitybooleanfield whichField, boolean value returns boolean
native BlzSetAbilityIntegerField                   takes ability whichAbility, abilityintegerfield whichField, integer value returns boolean
native BlzSetAbilityRealField                      takes ability whichAbility, abilityrealfield whichField, real value returns boolean
native BlzSetAbilityStringField                    takes ability whichAbility, abilitystringfield whichField, string value returns boolean
native BlzSetAbilityBooleanLevelField              takes ability whichAbility, abilitybooleanlevelfield whichField, integer level, boolean value returns boolean
native BlzSetAbilityIntegerLevelField              takes ability whichAbility, abilityintegerlevelfield whichField, integer level, integer value returns boolean
native BlzSetAbilityRealLevelField                 takes ability whichAbility, abilityreallevelfield whichField, integer level, real value returns boolean
native BlzSetAbilityStringLevelField               takes ability whichAbility, abilitystringlevelfield whichField, integer level, string value returns boolean
native BlzSetAbilityBooleanLevelArrayField         takes ability whichAbility, abilitybooleanlevelarrayfield whichField, integer level, integer index, boolean value returns boolean
native BlzSetAbilityIntegerLevelArrayField         takes ability whichAbility, abilityintegerlevelarrayfield whichField, integer level, integer index, integer value returns boolean
native BlzSetAbilityRealLevelArrayField            takes ability whichAbility, abilityreallevelarrayfield whichField, integer level, integer index, real value returns boolean
native BlzSetAbilityStringLevelArrayField          takes ability whichAbility, abilitystringlevelarrayfield whichField, integer level, integer index, string value returns boolean
native BlzAddAbilityBooleanLevelArrayField         takes ability whichAbility, abilitybooleanlevelarrayfield whichField, integer level, boolean value returns boolean
native BlzAddAbilityIntegerLevelArrayField         takes ability whichAbility, abilityintegerlevelarrayfield whichField, integer level, integer value returns boolean
native BlzAddAbilityRealLevelArrayField            takes ability whichAbility, abilityreallevelarrayfield whichField, integer level, real value returns boolean
native BlzAddAbilityStringLevelArrayField          takes ability whichAbility, abilitystringlevelarrayfield whichField, integer level, string value returns boolean
native BlzRemoveAbilityBooleanLevelArrayField      takes ability whichAbility, abilitybooleanlevelarrayfield whichField, integer level, boolean value returns boolean
native BlzRemoveAbilityIntegerLevelArrayField      takes ability whichAbility, abilityintegerlevelarrayfield whichField, integer level, integer value returns boolean
native BlzRemoveAbilityRealLevelArrayField         takes ability whichAbility, abilityreallevelarrayfield whichField, integer level, real value returns boolean
native BlzRemoveAbilityStringLevelArrayField       takes ability whichAbility, abilitystringlevelarrayfield whichField, integer level, string value returns boolean
