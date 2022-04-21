// Trigger Unit Based Event API

/**
Returns handle to unit which triggered the most recent event when called from
within a trigger action function...returns null handle when used incorrectly
*/
constant native GetTriggerUnit takes nothing returns unit



native TriggerRegisterUnitStateEvent takes trigger whichTrigger, unit whichUnit, unitstate whichState, limitop opcode, real limitval returns event


/**
@event EVENT_UNIT_STATE_LIMIT
*/
constant native GetEventUnitState takes nothing returns unitstate



native TriggerRegisterUnitEvent takes trigger whichTrigger, unit whichUnit, unitevent whichEvent returns event


/**
@event EVENT_UNIT_DAMAGED
*/
constant native GetEventDamage takes nothing returns real

/**
@event EVENT_UNIT_DAMAGED
*/
constant native GetEventDamageSource takes nothing returns unit


/**
Set the damage amount of a damage event.

In 1.31 PTR there’s currently 3 new damage events:

1. `EVENT_UNIT_DAMAGED` - old classic event for a specific unit;
2. `EVENT_PLAYER_UNIT_DAMAGED` - Same as 1, but for all units of a specific player on the map;

        // This seems to work fine anyway:
        call TriggerRegisterAnyUnitEventBJ(gg_trg_a, EVENT_PLAYER_UNIT_DAMAGING)

3. `EVENT_UNIT_DAMAGING` - triggers before any armor, armor type and other resistances. Event for a specific unit like 1;
4. `EVENT_PLAYER_UNIT_DAMAGING` - triggers before any armor, armor type and other resistances. Useful to modify either damage amount, attack type or damage type before any reductions done by game.

1 and 2 - modify the damage after any reduction.
3 and 4 - changes damage before reduction. Amount you set will be reduced later according to target’s resistance, armor etc.

If set to <=0 during 3 or 4, then 1 or 2 will never fire.
Misses don’t trigger any damage events.
Set to 0.00 to completely block the damage.
Set to negative value to heal the target instead of damaging

@note Tip: calling `GetEventDamage` after you set it with this function will return the value you set.
@note If you’ll call `UnitDamageTarget` from within a trigger, which reacts to a damage event or triggered by one, it will cause infinite loop and game will crash, so you should handle such scenarios with additional logic.

@event EVENT_UNIT_DAMAGED
@patch 1.29
*/
native BlzSetEventDamage                           takes real damage returns nothing

/**
The target unit of the damage event.
If damage is AoE, your trigger will run separately for each target without known issues.
This returns the same result as `GetTriggerUnit`

@event EVENT_UNIT_DAMAGED
@patch 1.31
*/
native BlzGetEventDamageTarget 	                   takes nothing returns unit

/**
Returns attacktype of the damage being taken.
Spell-damage is `ATTACK_TYPE_NORMAL`

@event EVENT_UNIT_DAMAGED
@patch 1.31
*/
native BlzGetEventAttackType  	                   takes nothing returns attacktype

/**
Returns damagetype of the damage being taken.
Regular attack is `DAMAGE_TYPE_NORMAL`

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
@event EVENT_UNIT_DETECTED 
*/
constant native GetEventDetectingPlayer takes nothing returns player



native TriggerRegisterFilterUnitEvent takes trigger whichTrigger, unit whichUnit, unitevent whichEvent, boolexpr filter returns event


/**
@event EVENT_UNIT_ACQUIRED_TARGET
@event EVENT_UNIT_TARGET_IN_RANGE
*/
constant native GetEventTargetUnit takes nothing returns unit


native TriggerRegisterUnitInRange takes trigger whichTrigger, unit whichUnit, real range, boolexpr filter returns event

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

