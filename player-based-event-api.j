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

@event ConvertPlayerEvent(96)
*/
native TriggerRegisterPlayerChatEvent takes trigger whichTrigger, player whichPlayer, string chatMessageToDetect, boolean exactMatchOnly returns event


/**
Returns the actual string they typed in ( same as what you registered for
 if you required exact match ).
Used in conjunction with `TriggerRegisterPlayerChatEvent`.

@event ConvertPlayerEvent(96)

@bug This function only returns `""` when called in response to `EVENT_PLAYER_CHAT`.
*/
constant native GetEventPlayerChatString takes nothing returns string


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

@param whichTrigger Register death event to execute this trigger.
@param whichWidget Trigger when this widget dies.
*/
native TriggerRegisterDeathEvent takes trigger whichTrigger, widget whichWidget returns event



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
