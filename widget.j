// Widget API

/**
Returns target's health points on succcess. Returns 0.0 if widget was removed or is null.

@note See: `SetWidgetLife`.
See `widget` for an explanation how this applies to units, destructables, items.

@param whichWidget target widget
*/
native GetWidgetLife takes widget whichWidget returns real

/**
Sets target's health points.

It is limited by target's maximum health points. A value of ≤0 kills the target.

@note See: `GetWidgetLife`.
See `widget` for an explanation how this applies to units, destructables, items.

@param whichWidget target widget
@param newLife set health points to this amount
*/
native SetWidgetLife takes widget whichWidget, real newLife returns nothing

/**
Returns X map coordinate of widget on success. Returns 0.0 if widget was removed or is null.

@note See: `GetWidgetY`.
See `widget` for an explanation how this applies to units, destructables, items.

@param whichWidget target widget
*/
native GetWidgetX takes widget whichWidget returns real

/**
Returns Y map coordinate of widget on success. Returns 0.0 if widget was removed or is null.

@note See: `GetWidgetX`.
See `widget` for an explanation how this applies to units, destructables, items.

@param whichWidget target widget
*/
native GetWidgetY takes widget whichWidget returns real

/**
Returns the target widget inside a trigger action. Otherwise returns null.

@note Only works in triggers that operate on actual `widget` type, like `TriggerRegisterDeathEvent`.
*/
constant native GetTriggerWidget takes nothing returns widget
