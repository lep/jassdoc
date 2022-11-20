// Group API

native CreateGroup                          takes nothing returns group

/**
Destroys the group.

Accessing a destroyed group shows no units, a size of 0 and cannot be modified in any way.
*/
native DestroyGroup                         takes group whichGroup returns nothing

/**
Appends unit at the end of group, increasing size by 1 (see `BlzGroupGetSize`).
Returns true if the unit was added, false if the unit is already in the group or the group is destroyed.

Even if there's a null "hole" at index 0, the unit will still be added at the tail end.

@param whichGroup Target group.
@param whichUnit Target unit.
*/
native GroupAddUnit                         takes group whichGroup, unit whichUnit returns boolean

/**
Removes unit from group, returns true on success; returns false on failure (no operation).

If unit is null, does nothing and returns false regardless if there're null values at any index in the group (does not remove destroyed units which are still in group).
*/
native GroupRemoveUnit                      takes group whichGroup, unit whichUnit returns boolean

/**
Adds a target addGroup to the desired whichGroup immediately.

@patch 1.31
*/
native BlzGroupAddGroupFast                 takes group whichGroup, group addGroup returns integer

/**
@patch 1.31
*/
native BlzGroupRemoveGroupFast              takes group whichGroup, group removeGroup returns integer

/**
Erase every unit from the group, it becomes size = 0.
*/
native GroupClear                           takes group whichGroup returns nothing

/**
Returns the size (length) of group.
The size refers to game's internal representation of group data (array), group's last index is `size - 1`.

@note See: `BlzGroupUnitAt`.
@patch 1.31
*/
native BlzGroupGetSize                      takes group whichGroup returns integer

/**
Returns unit at the given index in group. Groups start at index 0.

If the unit was removed from the game or index is out of bounds, returns null.

@patch 1.31
*/
native BlzGroupUnitAt                       takes group whichGroup, integer index returns unit

native GroupEnumUnitsOfType                 takes group whichGroup, string unitname, boolexpr filter returns nothing

/**
@note In contrast to other Enum-functions this function enumarates units with locust.
*/
native GroupEnumUnitsOfPlayer               takes group whichGroup, player whichPlayer, boolexpr filter returns nothing

/**
@bug Causes irregular behavior when used with large numbers.
@note *Probably* countLimit doesn't work similar to `GroupEnumUnitsInRangeCounted`. Instead see `GroupEnumUnitsOfType`.
*/
native GroupEnumUnitsOfTypeCounted          takes group whichGroup, string unitname, boolexpr filter, integer countLimit returns nothing

native GroupEnumUnitsInRect                 takes group whichGroup, rect r, boolexpr filter returns nothing

/**
@bug Causes irregular behavior when used with large numbers.
@note *Probably* countLimit doesn't work similar to `GroupEnumUnitsInRangeCounted`. Instead see `GroupEnumUnitsInRect`.
*/
native GroupEnumUnitsInRectCounted          takes group whichGroup, rect r, boolexpr filter, integer countLimit returns nothing

/**
Adds units within radius of map coordinates X, Y who match filter to whichGroup.
A null as filter means that every nearby unit is added to group.

If the group has had units previously, it will be first cleared (old units will not be preserved).
A group that has been destroyed will not be recreated.

@param whichGroup Group to add units to.
@param x X map coordinate.
@param y Y map coordinate.
@param radius Radius in map units.
@param filter Filter function.

@note See: `GroupEnumUnitsInRect`, `GroupEnumUnitsInRangeOfLoc`.
*/
native GroupEnumUnitsInRange                takes group whichGroup, real x, real y, real radius, boolexpr filter returns nothing

native GroupEnumUnitsInRangeOfLoc           takes group whichGroup, location whichLocation, real radius, boolexpr filter returns nothing

/**
@bug Causes irregular behavior when used with large numbers.
@bug countLimit does not work, tested in 1.32.10.18067. Therefore behaves like `GroupEnumUnitsInRange` adding all units in range.
*/
native GroupEnumUnitsInRangeCounted         takes group whichGroup, real x, real y, real radius, boolexpr filter, integer countLimit returns nothing

/**
@bug Causes irregular behavior when used with large numbers.
@note *Probably* countLimit doesn't work similar to `GroupEnumUnitsInRangeCounted`. Instead see `GroupEnumUnitsInRangeOfLoc`.
*/
native GroupEnumUnitsInRangeOfLocCounted    takes group whichGroup, location whichLocation, real radius, boolexpr filter, integer countLimit returns nothing

native GroupEnumUnitsSelected               takes group whichGroup, player whichPlayer, boolexpr filter returns nothing



native GroupImmediateOrder                  takes group whichGroup, string order returns boolean

native GroupImmediateOrderById              takes group whichGroup, integer order returns boolean

native GroupPointOrder                      takes group whichGroup, string order, real x, real y returns boolean

native GroupPointOrderLoc                   takes group whichGroup, string order, location whichLocation returns boolean

native GroupPointOrderById                  takes group whichGroup, integer order, real x, real y returns boolean

native GroupPointOrderByIdLoc               takes group whichGroup, integer order, location whichLocation returns boolean

native GroupTargetOrder                     takes group whichGroup, string order, widget targetWidget returns boolean

native GroupTargetOrderById                 takes group whichGroup, integer order, widget targetWidget returns boolean

native ForGroup                 takes group whichGroup, code callback returns nothing

/**
Returns the unit at the first position in group or null if that unit no longer exists.

Equivalent to: `BlzGroupUnitAt(varGroup, 0)`.

@bug If the first unit of this group was removed from the game (RemoveUnit or decayed) then null be returned, regardless if there're valid units in group at further indeces. To iterate over all existing units of a group, use `ForGroup`/`ForGroupBJ`.
You cannot remove such null "holes" from a group without destroying or clearing it (`DestroyGroup`/`GroupClear`).
If you use FirstOfGroup in iterations with removal, units in the group will eventually leak.

@note See [GroupUtils Library](https://web.archive.org/web/20200918161954/http://wc3c.net/showthread.php?t=104464) for vJass.
*/
native FirstOfGroup             takes group whichGroup returns unit
