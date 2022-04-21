// Group API

native CreateGroup                          takes nothing returns group

native DestroyGroup                         takes group whichGroup returns nothing

native GroupAddUnit                         takes group whichGroup, unit whichUnit returns boolean
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

native GroupClear                           takes group whichGroup returns nothing

/**
@patch 1.31
*/
native BlzGroupGetSize                      takes group whichGroup returns integer

/**
@patch 1.31
*/
native BlzGroupUnitAt                       takes group whichGroup, integer index returns unit

native GroupEnumUnitsOfType                 takes group whichGroup, string unitname, boolexpr filter returns nothing

/**
@note In contrast to other Enum-functions this function enumarates units with locust.
*/
native GroupEnumUnitsOfPlayer               takes group whichGroup, player whichPlayer, boolexpr filter returns nothing

/**
@bug Causes irregular behavior when used with large numbers
*/
native GroupEnumUnitsOfTypeCounted          takes group whichGroup, string unitname, boolexpr filter, integer countLimit returns nothing

native GroupEnumUnitsInRect                 takes group whichGroup, rect r, boolexpr filter returns nothing

/**
@bug Causes irregular behavior when used with large numbers
*/
native GroupEnumUnitsInRectCounted          takes group whichGroup, rect r, boolexpr filter, integer countLimit returns nothing

native GroupEnumUnitsInRange                takes group whichGroup, real x, real y, real radius, boolexpr filter returns nothing

native GroupEnumUnitsInRangeOfLoc           takes group whichGroup, location whichLocation, real radius, boolexpr filter returns nothing

/**
@bug Causes irregular behavior when used with large numbers
*/
native GroupEnumUnitsInRangeCounted         takes group whichGroup, real x, real y, real radius, boolexpr filter, integer countLimit returns nothing

/**
@bug Causes irregular behavior when used with large numbers
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
@bug May return `null` even if there are still units in the group.
This happens when a unit in the group dies and decays since the group still
holds a reference to that unit but that unit is pretty much null.
See <http://wc3c.net/showthread.php?t=104464>.
*/
native FirstOfGroup             takes group whichGroup returns unit
