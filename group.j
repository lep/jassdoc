// Group API

native CreateGroup                          takes nothing returns group

native DestroyGroup                         takes group whichGroup returns nothing

native GroupAddUnit                         takes group whichGroup, unit whichUnit returns nothing

native GroupRemoveUnit                      takes group whichGroup, unit whichUnit returns nothing

native GroupClear                           takes group whichGroup returns nothing

native GroupEnumUnitsOfType                 takes group whichGroup, string unitname, boolexpr filter returns nothing

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

native FirstOfGroup             takes group whichGroup returns unit