// Region and Location API

/**
Returns a new rectangle as defined by two points (minX, minY) and (maxX, maxY).

The rectangle size and coordinates are limited to valid map coordinates, see
`GetWorldBounds`.

In Warcraft 3 the coordinates follow the regular cartesian system you know from
school math. The minimum coordinates (towards negative infinity) are on the left/bottom,
the maximum coordinates on right/top (towards positive infinity).

In the following graphic the N stands for the minimum point (minX, minY) and
X for the maximum point (maxX, maxY).

    +----X
    |    |
    |    |
    N----+
	
@bug You can't create your own rectangle that would match the dimensions
of `GetWorldBounds`. The maxX and maxY will be smaller by `16.0` than that of
the world bounds.

@note See: `RectFromLoc`, `RemoveRect`, `GetWorldBounds`

*/
native Rect                     takes real minx, real miny, real maxx, real maxy returns rect

/**
Returns new rectangle as defined by two locations: `min` (bottom-left) and
`max` (top-right).

The rectangle size and coordinates are limited to valid map coordinates, see
`GetWorldBounds`.

@bug You can't create your own rectangle that would match the dimensions
of `GetWorldBounds`. The maxX and maxY will be smaller by `16.0` than that of
the world bounds.

@note See: `Rect`, `RemoveRect`, `GetWorldBounds`

*/
native RectFromLoc              takes location min, location max returns rect

/**
Destroys the rectangle.

If you access the rectangle after removal, all of its values will return zero.

*/
native RemoveRect               takes rect whichRect returns nothing

/**
Changes a rectangle's minimum and maximum points that define it.

The rectangle size and coordinates are limited to valid map coordinates, see
`GetWorldBounds`.

@bug You can't create your own rectangle that would match the dimensions
of `GetWorldBounds`. The maxX and maxY will be smaller by `16.0` than that of
the world bounds.

@note See: `Rect`, `SetRectFromLoc`, `MoveRectTo`, `MoveRectToLoc`

*/
native SetRect                  takes rect whichRect, real minx, real miny, real maxx, real maxy returns nothing

/**
Changes a rectangle's minimum and maximum points (that define it) to those specified
by `min` and `max` locations.

Does nothing if either location is null or invalid.

@bug You can't create your own rectangle that would match the dimensions
of `GetWorldBounds`. The maxX and maxY will be smaller by `16.0` than that of
the world bounds.

@note See: `Rect`, `SetRect`, `MoveRectTo`, `MoveRectToLoc`

*/
native SetRectFromLoc           takes rect whichRect, location min, location max returns nothing

/**
Changes the minimum and maximum point of a rectangle to make it centered around the
specified point. Thus it moves the rectangle to a new position.

@bug This can be used to move the rectangle outside of the map bounds, bypassing
the limiting checks.

@note See: `Rect`, `SetRect`, `SetRectFromLoc`, `MoveRectToLoc`

*/
native MoveRectTo               takes rect whichRect, real newCenterX, real newCenterY returns nothing

/**
Changes the minimum and maximum point of a rectangle to make it centered around the
specified point. Thus it moves the rectangle to a new position.

Does nothing if either location is null or invalid.

@bug This can be used to move the rectangle outside of the map bounds, bypassing
the limiting checks.

@note See: `Rect`, `SetRect`, `SetRectFromLoc`, `MoveRectTo`

*/
native MoveRectToLoc            takes rect whichRect, location newCenterLoc returns nothing


/**
Returns rectangle's center X coordinate. This is equal to `((maxX + minX)/2)`.

Returns zero if `whichRect` is null or invalid.
*/
native GetRectCenterX           takes rect whichRect returns real

/**
Returns rectangle's center Y coordinate. This is equal to `((maxY + minY)/2)`.

Returns zero if `whichRect` is null or invalid.
*/
native GetRectCenterY           takes rect whichRect returns real

/**
Returns rectangle's bottom-left X coordinate. 

Returns zero if `whichRect` is null or invalid.
*/
native GetRectMinX              takes rect whichRect returns real

/**
Returns rectangle's bottom-left Y coordinate. 

Returns zero if `whichRect` is null or invalid.
*/
native GetRectMinY              takes rect whichRect returns real

/**
Returns rectangle's top-right X coordinate. 

Returns zero if `whichRect` is null or invalid.
*/
native GetRectMaxX              takes rect whichRect returns real

/**
Returns rectangle's top-right Y coordinate. 

Returns zero if `whichRect` is null or invalid.
*/
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
Returns a new instance of rectangle that spans the entire map, including
unplayable borders, in world coordinates.

Since this creates a new rectangle on each call, the rectangle object must be
destroyed manually by calling `RemoveRect`.

@note See: `Rect`, `RemoveRect`

*/
native GetWorldBounds           takes nothing returns rect
