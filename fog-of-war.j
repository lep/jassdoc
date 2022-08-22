
// Fog of War API

native SetFogStateRect takes player forWhichPlayer, fogstate whichState, rect where, boolean useSharedVision returns nothing

native SetFogStateRadius takes player forWhichPlayer, fogstate whichState, real centerx, real centerY, real radius, boolean useSharedVision returns nothing

native SetFogStateRadiusLoc takes player forWhichPlayer, fogstate whichState, location center, real radius, boolean useSharedVision returns nothing

native FogMaskEnable takes boolean enable returns nothing

native IsFogMaskEnabled takes nothing returns boolean

native FogEnable takes boolean enable returns nothing

native IsFogEnabled takes nothing returns boolean


/**
Creates an object that modifies the fog in a rect for a specific player.

@param whichState
Determines what type of fog the area is being modified to.

@param where The rect where the fog is

@param useSharedVision
Determines whether or not the fog modifier will be applied to allied players with shared vision.

@param afterUnits
Will determine whether or not units in that area will be masked by the fog. If it is set to true and the fogstate is masked, it will hide all the units in the fog modifier's radius and mask the area. If set to false, it will only mask the areas that are not visible to the units.
*/
native CreateFogModifierRect takes player forWhichPlayer, fogstate whichState, rect where, boolean useSharedVision, boolean afterUnits returns fogmodifier

/**
Creates an object that modifies the fog in a circular radius for a specific player.

@param whichState
Determines what type of fog the area is being modified to.

@param centerx
The x-coordinate where the fog modifier begins.

@param centery
The y-coordinate where the fog modifier begins.

@param radius
Determines the extent that the fog travels (expanding from the coordinates ( centerx , centery )).

@param useSharedVision
Determines whether or not the fog modifier will be applied to allied players with shared vision.

@param afterUnits
Will determine whether or not units in that area will be masked by the fog. If it is set to true and the fogstate is masked, it will hide all the units in the fog modifier's radius and mask the area. If set to false, it will only mask the areas that are not visible to the units.

@note You must use `FogModifierStart` to enable the fog modifier. 
*/
native CreateFogModifierRadius takes player forWhichPlayer, fogstate whichState, real centerx, real centerY, real radius, boolean useSharedVision, boolean afterUnits returns fogmodifier

/**
Creates an object that modifies the fog in a circular radius for a specific player.

@param whichState
Determines what type of fog the area is being modified to.

@param center
The location where the fog modifier begins.

@param radius
Determines the extent that the fog travels (expanding from the location `center`).

@param useSharedVision
Determines whether or not the fog modifier will be applied to allied players with shared vision.

@param afterUnits
Will determine whether or not units in that area will be masked by the fog. If it is set to true and the fogstate is masked, it will hide all the units in the fog modifier's radius and mask the area. If set to false, it will only mask the areas that are not visible to the units.

@note You must use `FogModifierStart` to enable the fog modifier. 
*/
native CreateFogModifierRadiusLoc takes player forWhichPlayer, fogstate whichState, location center, real radius, boolean useSharedVision, boolean afterUnits returns fogmodifier

native DestroyFogModifier takes fogmodifier whichFogModifier returns nothing

native FogModifierStart takes fogmodifier whichFogModifier returns nothing

native FogModifierStop takes fogmodifier whichFogModifier returns nothing
