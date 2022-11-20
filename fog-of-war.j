
// Fog of War API

/**
Sets target player's fog of war data in the specified rectangle.

Individual player's fog state is a reflection of player's map exploration progress:
which areas were explored or still hidden; which are fogged (not visible); which are
visible. What is visible in game is a combination of personal fog state & fog modifiers.

@param forWhichPlayer Target player.
@param whichState Change fog to this type. See `fogstate` for type explanation.
@param where Target rectangle area.
@param useSharedVision
If true, apply new state to player and whoever player shares their vision.
If false, apply only to player themself.
*/
native SetFogStateRect takes player forWhichPlayer, fogstate whichState, rect where, boolean useSharedVision returns nothing


/**
Sets target player's fog of war data in the specified circle area.

Individual player's fog state is a reflection of player's map exploration progress:
which areas were explored or still hidden; which are fogged (not visible); which are
visible. What is visible in game is a combination of personal fog state & fog modifiers.

@param forWhichPlayer Target player.
@param whichState Change fog to this type. See `fogstate` for type explanation.
@param centerx X-coordinate of the circle center.
@param centerY Y-coordinate of the circle center.
@param radius Circle's radius (from center to its edge).
@param useSharedVision
If true, apply new state to player and whoever player shares their vision.
If false, apply only to player themself.
*/
native SetFogStateRadius takes player forWhichPlayer, fogstate whichState, real centerx, real centerY, real radius, boolean useSharedVision returns nothing

/**
Sets target player's fog of war data in the specified circle area.

Individual player's fog state is a reflection of player's map exploration progress:
which areas were explored or still hidden; which are fogged (not visible); which are
visible. What is visible in game is a combination of personal fog state & fog modifiers.

@param forWhichPlayer Target player.
@param whichState Change fog to this type. See `fogstate` for type explanation.
@param center Location describing the center of the circle.
@param radius Circle's radius (from center to its edge).
@param useSharedVision
If true, apply new state to player and whoever player shares their vision.
If false, apply only to player themself.
*/
native SetFogStateRadiusLoc takes player forWhichPlayer, fogstate whichState, location center, real radius, boolean useSharedVision returns nothing


/**
Toggles global FOW masking of unexplored areas.

An individual player's fog state is not modified, that means this toggle is fully
reversible.

@param enable
True: unexplored areas are masked.

False: unexplored areas are not masked (whether visible depends on `IsFogEnabled`).

@note See: `IsFogMaskEnabled`, `IsFogEnabled`. "Fog mask" is explained in `fogstate`.
*/
native FogMaskEnable takes boolean enable returns nothing


/**
Returns whether global FOW masking is in effect.

True: unexplored areas are globally masked.

False: unexplored areas are not globally masked (whether visible depends on `IsFogEnabled`).

@note See: `FogMaskEnable`, `FogEnable`. "Fog mask" is explained in `fogstate`.
*/
native IsFogMaskEnabled takes nothing returns boolean


/**
Toggles global FOW fogging of explored yet not visible areas.

An individual player's fog state is not modified, that means this toggle is fully
reversible.

True: explored areas are fogged if not in sight.

False: explored areas remain permanently visible.

@note See: `IsFogEnabled`, `IsFogMaskEnabled`. "Fog" is explained in `fogstate`.
*/
native FogEnable takes boolean enable returns nothing


/**
Toggles global FOW fogging of explored yet not visible areas.

True: explored areas are fogged if not in sight.

False: explored areas remain permanently visible.

@note See: `FogEnable`, `FogMaskEnable`. "Fog" is explained in `fogstate`.
*/
native IsFogEnabled takes nothing returns boolean


/**
Creates an object that overrides the fog in a rect for a specific player.

A fog modifier is disabled by default, use `FogModifierStart` to enable.

This creates a new object with a handle and must be removed to avoid leaks: `DestroyFogModifier`.

@param whichState
Determines what type of fog the area is being modified to. See `fogstate` for type explanation.

@param where The rect where the fog is.

@param useSharedVision
Apply modifier to target's allied players with shared vision?

@param afterUnits
Will determine whether or not units in that area will be masked by the fog. If it is set to true and the fogstate is masked, it will hide all the units in the fog modifier's radius and mask the area. If set to false, it will only mask the areas that are not visible to the units.

@bug (v1.32.10) Just by creating a modifier of type `FOG_OF_WAR_FOGGED` or
`FOG_OF_WAR_VISIBLE`, this will modify the player's global fog state before it is
enabled. "VISIBLE" will instantly become "FOGGED" and "FOGGED" will cause unexplored
areas to become explored. You can workaround this by using e.g. `SetFogStateRect`
after fog modifier creation.
*/
native CreateFogModifierRect takes player forWhichPlayer, fogstate whichState, rect where, boolean useSharedVision, boolean afterUnits returns fogmodifier

/**
Creates an object that overrides the fog in a circular radius for a specific player.

A fog modifier is disabled by default, use `FogModifierStart` to enable.

This creates a new object with a handle and must be removed to avoid leaks: `DestroyFogModifier`.

@param whichState
Determines what type of fog the area is being modified to. See `fogstate` for type explanation.

@param centerx
The x-coordinate where the fog modifier begins.

@param centerY
The y-coordinate where the fog modifier begins.

@param radius
Determines the extent that the fog travels (expanding from the coordinates ( centerx , centery )).

@param useSharedVision
Apply modifier to target's allied players with shared vision?

@param afterUnits
Will determine whether or not units in that area will be masked by the fog. If it is set to true and the `fogstate` is masked, it will hide all the units in the fog modifier's radius and mask the area. If set to false, it will only mask the areas that are not visible to the units.

@bug (v1.32.10) Just by creating a modifier of type `FOG_OF_WAR_FOGGED` or
`FOG_OF_WAR_VISIBLE`, this will modify the player's global fog state before it is
enabled. "VISIBLE" will instantly become "FOGGED" and "FOGGED" will cause unexplored
areas to become explored. You can workaround this by using e.g. `SetFogStateRect`
after fog modifier creation.
*/
native CreateFogModifierRadius takes player forWhichPlayer, fogstate whichState, real centerx, real centerY, real radius, boolean useSharedVision, boolean afterUnits returns fogmodifier

/**
Creates an object that overrides the fog in a circular radius for a specific player.

A fog modifier is disabled by default, use `FogModifierStart` to enable.

This creates a new object with a handle and must be removed to avoid leaks: `DestroyFogModifier`.

@param whichState
Determines what type of fog the area is being modified to. See `fogstate` for type explanation.

@param center
The location where the fog modifier begins.

@param radius
Determines the extent that the fog travels (expanding from the location `center`).

@param useSharedVision
Apply modifier to target's allied players with shared vision?

@param afterUnits
Will determine whether or not units in that area will be masked by the fog. If it is set to true and the `fogstate` is masked, it will hide all the units in the fog modifier's radius and mask the area. If set to false, it will only mask the areas that are not visible to the units.

@bug (v1.32.10) Just by creating a modifier of type `FOG_OF_WAR_FOGGED` or
`FOG_OF_WAR_VISIBLE`, this will modify the player's global fog state before it is
enabled. "VISIBLE" will instantly become "FOGGED" and "FOGGED" will cause unexplored
areas to become explored. You can workaround this by using e.g. `SetFogStateRect`
after fog modifier creation.
*/
native CreateFogModifierRadiusLoc takes player forWhichPlayer, fogstate whichState, location center, real radius, boolean useSharedVision, boolean afterUnits returns fogmodifier


/**
Destroys the fog modifier object and removes its effect.
*/
native DestroyFogModifier takes fogmodifier whichFogModifier returns nothing


/**
Enable the effect of the modifier. While enabled, it will override the player's
regular fog state.
*/
native FogModifierStart takes fogmodifier whichFogModifier returns nothing


/**
Disable the effect of the modifier. Once disabled the player's visibility
will return to player's regular fog state.
*/
native FogModifierStop takes fogmodifier whichFogModifier returns nothing
