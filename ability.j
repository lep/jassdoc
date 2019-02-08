native BlzSetAbilityTooltip                        takes integer abilCode, string tooltip, integer level returns nothing
native BlzSetAbilityActivatedTooltip               takes integer abilCode, string tooltip, integer level returns nothing
native BlzSetAbilityExtendedTooltip                takes integer abilCode, string extendedTooltip, integer level returns nothing
native BlzSetAbilityActivatedExtendedTooltip       takes integer abilCode, string extendedTooltip, integer level returns nothing
native BlzSetAbilityResearchTooltip                takes integer abilCode, string researchTooltip, integer level returns nothing
native BlzSetAbilityResearchExtendedTooltip        takes integer abilCode, string researchExtendedTooltip, integer level returns nothing
native BlzGetAbilityTooltip                        takes integer abilCode, integer level returns string
native BlzGetAbilityActivatedTooltip               takes integer abilCode, integer level returns string
native BlzGetAbilityExtendedTooltip                takes integer abilCode, integer level returns string
native BlzGetAbilityActivatedExtendedTooltip       takes integer abilCode, integer level returns string
native BlzGetAbilityResearchTooltip                takes integer abilCode, integer level returns string
native BlzGetAbilityResearchExtendedTooltip        takes integer abilCode, integer level returns string
native BlzSetAbilityIcon                           takes integer abilCode, string iconPath returns nothing
native BlzGetAbilityIcon                           takes integer abilCode returns string
native BlzSetAbilityActivatedIcon                  takes integer abilCode, string iconPath returns nothing
native BlzGetAbilityActivatedIcon                  takes integer abilCode returns string
native BlzGetAbilityPosX                           takes integer abilCode returns integer
native BlzGetAbilityPosY                           takes integer abilCode returns integer
native BlzSetAbilityPosX                           takes integer abilCode, integer x returns nothing
native BlzSetAbilityPosY                           takes integer abilCode, integer y returns nothing
native BlzGetAbilityActivatedPosX                  takes integer abilCode returns integer
native BlzGetAbilityActivatedPosY                  takes integer abilCode returns integer
native BlzSetAbilityActivatedPosX                  takes integer abilCode, integer x returns nothing
native BlzSetAbilityActivatedPosY                  takes integer abilCode, integer y returns nothing
native BlzGetAbilityManaCost                       takes integer abilId, integer level returns integer
native BlzGetAbilityCooldown                       takes integer abilId, integer level returns real
