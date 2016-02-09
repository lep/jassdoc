
// Fog of War API

native SetFogStateRect takes player forWhichPlayer, fogstate whichState, rect where, boolean useSharedVision returns nothing

native SetFogStateRadius takes player forWhichPlayer, fogstate whichState, real centerx, real centerY, real radius, boolean useSharedVision returns nothing

native SetFogStateRadiusLoc takes player forWhichPlayer, fogstate whichState, location center, real radius, boolean useSharedVision returns nothing

native FogMaskEnable takes boolean enable returns nothing

native IsFogMaskEnabled takes nothing returns boolean

native FogEnable takes boolean enable returns nothing

native IsFogEnabled takes nothing returns boolean



native CreateFogModifierRect takes player forWhichPlayer, fogstate whichState, rect where, boolean useSharedVision, boolean afterUnits returns fogmodifier

native CreateFogModifierRadius takes player forWhichPlayer, fogstate whichState, real centerx, real centerY, real radius, boolean useSharedVision, boolean afterUnits returns fogmodifier

native CreateFogModifierRadiusLoc takes player forWhichPlayer, fogstate whichState, location center, real radius, boolean useSharedVision, boolean afterUnits returns fogmodifier

native DestroyFogModifier takes fogmodifier whichFogModifier returns nothing

native FogModifierStart takes fogmodifier whichFogModifier returns nothing

native FogModifierStop takes fogmodifier whichFogModifier returns nothing
