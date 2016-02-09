// Trigger Player Based Event API

native TriggerRegisterPlayerEvent takes trigger whichTrigger, player whichPlayer, playerevent whichPlayerEvent returns event


/**
@event EVENT_PLAYER_DEFEAT
@event EVENT_PLAYER_VICTORY
*/
constant native GetTriggerPlayer takes nothing returns player



native TriggerRegisterPlayerUnitEvent takes trigger whichTrigger, player whichPlayer, playerunitevent whichPlayerUnitEvent, boolexpr filter returns event

/**
@event EVENT_PLAYER_HERO_LEVEL
@event EVENT_UNIT_HERO_LEVEL
*/
constant native GetLevelingUnit takes nothing returns unit


/**
@event EVENT_PLAYER_HERO_SKILL
@event EVENT_UNIT_HERO_SKILL
*/
constant native GetLearningUnit takes nothing returns unit

/**
@event EVENT_PLAYER_HERO_SKILL
@event EVENT_UNIT_HERO_SKILL
*/
constant native GetLearnedSkill takes nothing returns integer

/**
@event EVENT_PLAYER_HERO_SKILL
@event EVENT_UNIT_HERO_SKILL
*/
constant native GetLearnedSkillLevel takes nothing returns integer


/**
@event EVENT_PLAYER_HERO_REVIVABLE
*/
constant native GetRevivableUnit takes nothing returns unit


/**
@event EVENT_PLAYER_HERO_REVIVE_START
@event EVENT_PLAYER_HERO_REVIVE_CANCEL
@event EVENT_PLAYER_HERO_REVIVE_FINISH
@event EVENT_UNIT_HERO_REVIVE_START
@event EVENT_UNIT_HERO_REVIVE_CANCEL
@event EVENT_UNIT_HERO_REVIVE_FINISH
*/
constant native GetRevivingUnit takes nothing returns unit


/**
@event EVENT_PLAYER_UNIT_ATTACKED
@event EVENT_UNIT_ATTACKED
*/
constant native GetAttacker takes nothing returns unit


/**
@event EVENT_PLAYER_UNIT_RESCUED
@event EVENT_UNIT_RESCUEDED
*/
constant native GetRescuer takes nothing returns unit


/**
@event EVENT_PLAYER_UNIT_DEATH
@event EVENT_UNIT_DEATH


*/
constant native GetDyingUnit takes nothing returns unit

/**
@event EVENT_PLAYER_UNIT_DEATH
*/
constant native GetKillingUnit takes nothing returns unit


/**
@event EVENT_PLAYER_UNIT_DECAY
@event EVENT_UNIT_DECAY
*/
constant native GetDecayingUnit takes nothing returns unit



// EVENT_PLAYER_UNIT_SELECTED

//constant native GetSelectedUnit takes nothing returns unit


/**
@event EVENT_PLAYER_UNIT_CONSTRUCT_START
*/
constant native GetConstructingStructure takes nothing returns unit


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


/**
@event EVENT_PLAYER_UNIT_TRAIN_START
@event EVENT_PLAYER_UNIT_TRAIN_CANCEL

@event EVENT_UNIT_TRAIN_START
@event EVENT_UNIT_TRAIN_CANCELLED
@event EVENT_UNIT_TRAIN_FINISH
*/
constant native GetTrainedUnitType takes nothing returns integer


/**
@event EVENT_PLAYER_UNIT_TRAIN_FINISH
@event EVENT_UNIT_TRAIN_FINISH
*/
constant native GetTrainedUnit takes nothing returns unit


/**
@event EVENT_PLAYER_UNIT_DETECTED
*/
constant native GetDetectedUnit takes nothing returns unit


/**
@event EVENT_PLAYER_UNIT_SUMMONED
*/
constant native GetSummoningUnit takes nothing returns unit

/**
@event EVENT_PLAYER_UNIT_SUMMONED
*/
constant native GetSummonedUnit takes nothing returns unit


/**
@event EVENT_PLAYER_UNIT_LOADED
*/
constant native GetTransportUnit takes nothing returns unit

/**
@event EVENT_PLAYER_UNIT_LOADED
*/
constant native GetLoadedUnit takes nothing returns unit


/**
@event EVENT_PLAYER_UNIT_SELL
@event EVENT_UNIT_SELL
*/
constant native GetSellingUnit takes nothing returns unit

/**
@event EVENT_PLAYER_UNIT_SELL
@event EVENT_UNIT_SELL
*/
constant native GetSoldUnit takes nothing returns unit

/**
@event EVENT_PLAYER_UNIT_SELL
@event EVENT_UNIT_SELL
*/
constant native GetBuyingUnit takes nothing returns unit


/**
@event EVENT_PLAYER_UNIT_SELL_ITEM
*/
constant native GetSoldItem takes nothing returns item


/**
@event EVENT_PLAYER_UNIT_CHANGE_OWNER
*/
constant native GetChangingUnit takes nothing returns unit

/**
@event EVENT_PLAYER_UNIT_CHANGE_OWNER
*/
constant native GetChangingUnitPrevOwner takes nothing returns player


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
constant native GetManipulatedItem takes nothing returns item


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



/**
@event EVENT_PLAYER_UNIT_ISSUED_TARGET_ORDER
@event EVENT_UNIT_ISSUED_TARGET_ORDER
*/
constant native GetOrderTarget takes nothing returns widget

/**
@event EVENT_PLAYER_UNIT_ISSUED_TARGET_ORDER
@event EVENT_UNIT_ISSUED_TARGET_ORDER
*/
constant native GetOrderTargetDestructable takes nothing returns destructable

/**
@event EVENT_PLAYER_UNIT_ISSUED_TARGET_ORDER
@event EVENT_UNIT_ISSUED_TARGET_ORDER
*/
constant native GetOrderTargetItem takes nothing returns item

/**
@event EVENT_PLAYER_UNIT_ISSUED_TARGET_ORDER
@event EVENT_UNIT_ISSUED_TARGET_ORDER
*/
constant native GetOrderTargetUnit takes nothing returns unit


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
constant native GetSpellAbilityUnit takes nothing returns unit

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
constant native GetSpellAbilityId takes nothing returns integer

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
constant native GetSpellAbility takes nothing returns ability

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
constant native GetSpellTargetLoc takes nothing returns location

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
constant native GetSpellTargetDestructable takes nothing returns destructable

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
constant native GetSpellTargetItem takes nothing returns item

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
constant native GetSpellTargetUnit takes nothing returns unit

native TriggerRegisterPlayerAllianceChange takes trigger whichTrigger, player whichPlayer, alliancetype whichAlliance returns event

native TriggerRegisterPlayerStateEvent takes trigger whichTrigger, player whichPlayer, playerstate whichState, limitop opcode, real limitval returns event


/**
@event EVENT_PLAYER_STATE_LIMIT
*/
constant native GetEventPlayerState takes nothing returns playerstate



native TriggerRegisterPlayerChatEvent takes trigger whichTrigger, player whichPlayer, string chatMessageToDetect, boolean exactMatchOnly returns event


/**
Returns the actual string they typed in ( same as what you registered for
 if you required exact match )

@event EVENT_PLAYER_CHAT
*/
constant native GetEventPlayerChatString takes nothing returns string


/**
Returns the string that you registered for

@event EVENT_PLAYER_CHAT
*/
constant native GetEventPlayerChatStringMatched takes nothing returns string



native TriggerRegisterDeathEvent takes trigger whichTrigger, widget whichWidget returns event



