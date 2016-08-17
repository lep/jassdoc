// Region and Location API

native Rect                     takes real minx, real miny, real maxx, real maxy returns rect

native RectFromLoc              takes location min, location max returns rect

native RemoveRect               takes rect whichRect returns nothing

native SetRect                  takes rect whichRect, real minx, real miny, real maxx, real maxy returns nothing

native SetRectFromLoc           takes rect whichRect, location min, location max returns nothing

native MoveRectTo               takes rect whichRect, real newCenterX, real newCenterY returns nothing

native MoveRectToLoc            takes rect whichRect, location newCenterLoc returns nothing



native GetRectCenterX           takes rect whichRect returns real

native GetRectCenterY           takes rect whichRect returns real

native GetRectMinX              takes rect whichRect returns real

native GetRectMinY              takes rect whichRect returns real

native GetRectMaxX              takes rect whichRect returns real

native GetRectMaxY              takes rect whichRect returns real



native CreateRegion             takes nothing returns region

native RemoveRegion             takes region whichRegion returns nothing



native RegionAddRect            takes region whichRegion, rect r returns nothing

native RegionClearRect          takes region whichRegion, rect r returns nothing



native RegionAddCell           takes region whichRegion, real x, real y returns nothing

native RegionAddCellAtLoc      takes region whichRegion, location whichLocation returns nothing

native RegionClearCell         takes region whichRegion, real x, real y returns nothing

native RegionClearCellAtLoc    takes region whichRegion, location whichLocation returns nothing



native Location                 takes real x, real y returns location

native RemoveLocation           takes location whichLocation returns nothing

native MoveLocation             takes location whichLocation, real newX, real newY returns nothing

native GetLocationX             takes location whichLocation returns real

native GetLocationY             takes location whichLocation returns real



/**
@note This function is asynchronous. The values it returns are not guaranteed synchronous between each player.
If you attempt to use it in a synchronous manner, it may cause a desync.

@note Reasons for returning different values might be terrain-deformations
caused by spells/abilities and different graphic settings.
Other reasons could be the rendering state of destructables and visibility differences.

@async
*/
native GetLocationZ             takes location whichLocation returns real



native IsUnitInRegion               takes region whichRegion, unit whichUnit returns boolean

native IsPointInRegion              takes region whichRegion, real x, real y returns boolean

native IsLocationInRegion           takes region whichRegion, location whichLocation returns boolean



/**
Returns full map bounds, including unplayable borders, in world coordinates
*/
native GetWorldBounds           takes nothing returns rect
